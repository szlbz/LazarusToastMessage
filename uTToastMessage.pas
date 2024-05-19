unit uTToastMessage;
{------------------------------------------------------------------------------
秋风(QQ:315795176)
  来自https://github.com/pcplayer/DelphiToastMessage
  基于pcplayer版本，修改后适用于lazarus,并可以跨平台使用
1、根据显示内容调整totast的宽度，防止显示内容不完整。
2、增加从form底向上显示选择
秋风 2024-05-12

  来自 https://github.com/desenvolvimentojd3/DelphiToastMessage

  我对它进行了一些修改：
  1. 增加一个全局变量  ToastMessage: TToastMessage; 这样就一个实例可以显示到所有 Form 里面去，
     而不是它的例子那样，每个 Form 都需要自己创建实例；
  2. 因为要一个实例，显示到所有 Form 里面去，因此增加了一个 Toast 方法，给 Parent；
  3. 因为整个程序一个实例，所以本单元自己创建实例，使用 initialization

  todo: 上述修改都基于一种情况：每次只显示一条信息。
        如果一次要显示很多条，类似一下弹出好几条信息，依次伸出多条显示，然后依次缩回去，就需要多个实例。
        这种情况下，需要增加一些代码，比如内部增加一个 List 来存放多个实例。

  4. 修改：去掉全局的 ToastMessage，用类变量和类方法来封装。这样用起来就简单了，直接调用类方法搞定。

  5. todo: 因此，如果同时要显示多个 Toast，则需要有一个类变量是 List

  6. About free object:
  6.1. if PanelBox is not on showing, release PanelBox is no problem.
  6.2. if PanelBox has shown on a form, its parent is this form, but PanelBox.Owner is nil.
       So when it is showing and user close Application, we can free PanelBox safely.
       But, even if I add a if Assigned(PanelBox) and then release it, there will raise a exception.
       So, I set it's name property, and in this situastion, it's name is ''.
       So, I add if PanelBox.Name <> ''.
       The problem is: when user close application, the different of two situastion is PanelBox.Parent,
       when it is showing, it's parent is a Form, and when form is destroy, it should not free PanelBox.
       Because the Form is just its parent, not its owner.

  7. the parent of PanelBox is a TForm, when we show message, we take over the OnResize event of TForm,
     so, when hide message, we must return this event handler to TForm.

  8. add message queue;
  8.1. FMessageList: TStringList 用于存放消息队列；字符串用于存放消息，格式是：modetype;title;text, 中间用[;]分割；
  8.2. TWinControl 是指消息的 Parent;

  pcplayer 2024-5-11
--------------------------------------------------------------------------------}
interface

uses
{$ifdef fpc}
  Base64,
  lazutf8,
  LConvEncoding,
  Graphics,
  Controls,
  Extctrls,
  StdCtrls,
  Classes,
  SysUtils,
  Forms,
  Messages;
{$else}
  System.NetEncoding,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Extctrls,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  Forms,
  Winapi.Windows,
  Winapi.Messages;
{$endif}

const
  ToastFontName='default';

type
    tpMode = (tpSuccess,tpInfo,tpError);
    TToastDirection = (tdTopLeft, tdTopCenter, tdTopRight, tdBottomLeft, tdBottomCenter, tdBottomRight);

type
  TToastMessage = class
  private
    {Timer}
    procedure Animate(Sender : TObject);
    procedure Wait   (Sender : TObject);

    procedure PanelBoxPosition (Sender: TObject);
    procedure CreatePanelBox   (const Parent : TWinControl);
    procedure RegisterColors;

    procedure SetParent(const Parent: TWinControl);

    {$ifdef fpc}
    procedure Base64ToPng(imagepng:TImage;const StringBase64: string);
    {$else}
    function Base64ToPng(const StringBase64: string): TPngImage;
    {$endif}
    function GetIsShowing: Boolean;
    procedure CheckMessageQueue;
    var
      SuccessImage : string;
      ErrorImage   : string;
      InfoImage    : string;
      PanelBox     : TPanel;
      PanelLine    : TPanel;
      Image        : TImage;
      Title        : TLabel;
      Text         : TLabel;
      MaxTop       : Integer;
      MinTop       : Integer;

      ToastHeith   : integer;
      PanelBoxTop:integer;
      OldPanelBoxTop :integer;
      ToastDirection : TToastDirection;

      TimerAnimation : TTimer;
      TimerWaiting   : TTimer;

      PanelBoxColor : TColor;
      TitleColor    : TColor;
      TextColor     : TColor;
      SuccessColor  : TColor;
      InfoColor     : TColor;
      ErrorColor    : TColor;

      FFormOnResize: TNotifyEvent;

    class var FToastMessage: TToastMessage;
    class var FMessageList: TStringList;
  public
    procedure Toast(const MessageType : tpMode; pTitle, pText : string;TD:TToastDirection;_ToastFontName:string = ToastFontName); overload;
    procedure Toast(const Parent: TWinControl; const MessageType : tpMode; pTitle, pText : string;TD:TToastDirection;_ToastFontName:string = ToastFontName); overload;

    constructor Create(const Parent : TWinControl); overload;
    destructor Destroy; override;

    class procedure ToastIt(const Parent : TWinControl; const MessageType : tpMode; pTitle, pText : string;TD:TToastDirection =tdTopCenter;_ToastFontName:string=ToastFontName);
    class procedure ReleaseMe;

    property IsShowing: Boolean read GetIsShowing;
  end;

implementation

{ ToastMessage }

procedure TToastMessage.CheckMessageQueue;
var
  {$ifndef fpc}
  AArray: TArray<string>;
  {$endif}
  AParent: TWinControl;
  S, AMode, ATitle, AText,tmpstr: string;
  MessageType: tpMode;
  TD,i,j,err:integer;
begin
  if not Assigned(FMessageList) then Exit;
  if FMessageList.Count = 0 then Exit;

  AParent := FMessageList.Objects[0] as TWinControl;
  {$ifdef fpc}
  s:=FMessageList.Strings[0];
  tmpstr:='';
  j:=0;
  for i:=0 to length(s) do
  begin
    if (s[i]=';') or (i=length(s)) then
    begin
      inc(j);
      if j=1 then
        AMode:= trim(tmpstr);
      if j=2 then
        ATitle:= tmpstr;
      if j=3 then
        AText:= tmpstr;
      if i=length(s) then
      begin
        val(s[i],td,err);
      end;
      tmpstr:='';
    end
    else
    begin
      tmpstr:=tmpstr+s[i];
    end;
  end;
  {$else}
  AArray := S.Split([';']);
  AMode := AArray[0];
  ATitle := AArray[1];
  AText := AArray[2];
  TD:=AArray[3];
  {$endif}
  FMessageList.Delete(0);

  MessageType := tpMode(StrToInt(AMode));

  Self.Toast(AParent, MessageType, ATitle, AText,TToastDirection(td));
end;

constructor TToastMessage.Create(const Parent : TWinControl);
begin
  MaxTop := 7;
  MinTop := -40;

  SuccessImage := 'iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAYAAAA7MK6iAAAABmJLR0QA/wD/AP+gvaeTAAACoklEQVRIie2Wz0tUURTHP+e9cZfLInAQRaEhdKZCkDHb+GPAhEwIWuTORdCiokXLrP6 '+
                  'EltEq20r5C23GEstxaiE4/qBVKmQFBi2UIOy9d1qo8Gbee/NDJVr43d1zzj2fcx73vnPhWP9IUmpg60pHtePQA9KNao0i4d0EuoHIuqiOmWIMv2tMfjkScHyhrUoM4wFIP2AWCXcEhm '+
                  'zbvP/h/OT6gcHxxcRVQQeBE8UKzJVui0pfOpYaCYowAqHZzjuCDpUPBZBKFV62LCVuB0b4Qnc7HSpUWIlyROn169wDbs52hU2xPnGgTv2k22JJJH0h9c1t9XRkivX46KAAUqkVPPJY3 '+
                  'YvWlY5qx5ZVip/ecmXbGqr5GJvY2DfkdLx7Tw8F3TSQJsPRS3l20xCrx23IAYsalw8DRaV9NpqcV1N+5zsFcnKH3AtF6w8I/aFoRyaWWo4vJiIoo6B5IVrnXuUfrtMHhLZnolNL8cVE '+
                  'xIBpRX3ySFUBsOaXuW++pkivwq88zyYqbZno1FJLNtEg6Iw/1Js7DyzfffcYzkImmnwFdLvgm6i0z8WSyy3ZRAOib4BT/lAAgu+xIKv+xZqTzdmucCaamtmDr5UJBeRzIBic8YBd9aZ '+
                  'Y0/vwnZ2fZ8qDAio5uXPAphjDgF0MPt80/6csKFg2Zs7/2vOvjmc7n4nQH1g4rInKc0RvASdLgAL6dC46ddNt8U4ftQdAt4NSCNQiOlA6lC0zZD7MN3rAmXNvvwrGdYI/eTlyDJW+92 '+
                  'dfe26L77xNR5MTiNwDnMNAEb07G0uO+jkLPn0uZjuvqOgLkMoyoVsqciPTmBwLCij4wkjHUiOmVVGH8gSwSgA6KINmyIgUgkIZz9vmbFd4b7R1C9QC4T3XBrCKyriNOeKeucf6L/QXd '+
                  'uEFdOVOF8gAAAAASUVORK5CYII=                                                                                                                 ';

  ErrorImage := 'iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAYAAAA7MK6iAAAABmJLR0QA/wD/AP+gvaeTAAADJklEQVRIie2W20tUQRzHP3PmuK2ra6gRFQRJj10hd8Mk2kiCoIgefKq/oQeLspuXLqTd/ '+
                '4OIfAsKoggiURCLTUHo8lCUXSCKSqVMPbo7Mz3spb2cs67aW32fDjPzm8/85nfm9/vBvyYxl8UmErHHrYmwJahBiKWJQfPVQgyXqsCA6O2N/1Xwr+3hDVg0AbuBKo9lI8B9EJfLu6PPFg '+
                'T+Gdm0xLLlFQz7AauYQwIauKnhUEX305E5g8d31q4VyroL1BQJzJKBYaTeG3w4+KJocBLaD1TMB5qhH0ab+mDPwMtZwT93hKslRA2sXiA0pffGFwsHHwx9yxzMi5uFuPYXoQCrxEzJxdz '+
                'BLI9/7di8HsyQ24EARGUVZmzUdfdCc4AWQteWPRocSg3kAMwhL6i9rYFA1x3klq15czJUR+DmbeyGXV5gyxh5MOugaWQkYk/IyS9AtZs3ga47sMgPsRmctmZUtD8B3VyPv60DSnww7TB5 '+
                'YJ+X5yNlKrAslWTS3o1bE2E3KIAZG8Vpa4aZGSjx4W/vRG7ZigzV4W9NQuMxnHMnC1139ZQ9VZu+gvSHKPxe1cATnNYjCbhdgr+1E/+ZS+BLQk8fQz3uK7QFWps04088jbW8oFUmPB4DK '+
                'cG2QamioAAIsyIfLLSZ3TK9uPilOWQXsPg8m5kM1eFvv5D0NA7xOEiJv+W869+eK4P5lAfW2rwvCpqKaXszzqnDf2JeBFwK60MeOFi9Kgp8dzMQlVWJJ+PzJZ5T61HU475EzNubIZaEnz '+
                'iLqPSqmoyUxksH88Di1i0F3HezMGOjTF84DdMOzpnj6TcMoKL9OC1HwJli+mqH93MS5m5mo5CdMhvq1mHUECC9PJ9nylRG6o2ZJTIrPZY/evIc6PKyLrBxwTngRm5dzsvLypZNQvCm0C5 '+
                'z1Dut1NHcQfdGYHtojbBEP7B4gVDPRsC1EgV7Bl5qCAOv5ksU8NYL6gkGqOh++lpDPXCDRANXrBTCXI/bMuwFTR5sdiX+dt0EZg8eFQz4DuKekeqyV4M3Z3BKprFRTo19DGltarBYBoDm '+
                'i2Ws4dIlKweTueC/XPUbY21OJhza4nIAAAAASUVORK5CYII=                                                                                              ';

  InfoImage := 'iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAYAAAA7MK6iAAAABmJLR0QA/wD/AP+gvaeTAAACnUlEQVRIie3WXUiTURzH8e85z5pIboOWZmXhEgKhF4oa1C7yuhp11U1BUEFowwu96Nb7UI '+
               'LKCqIXvEi6lLwPwkgShEghmTpRSU0Dpxioz/l3MafTbXl8u6rf1bP/zs7n7Hn5Pwf+taiNDK5qEM9w8UxYtA5hKAFAM+EqMxAa83350KAWtxUOPZs9iTF1oC4De/IMmwJpB904WFP0dUvw '+
               '0efJvQsuTaCuA9pmkYABaZk31I/G/FMbhiuezhwzotpAQpbg2gxoxZX+at83aziF0gH4N4mmM+0YIvGYr2dd+ODjZHCXVp0KKraIpomEsyjheK3vZ2Y167p5NQ9t0EN+TZnP5rJLueuRB1 '+
               'nLyfwQap49AdKda0GZuXDY4cXFQgButf/m47C7nm6M0meGqnd3pwtrAFO/Hgpwap+Do8BRcLrUWW84gHaQ2lWF9EFVg3hAXbKZpbV3gU8jLh0jLq29CzY/QUSiKSOV5VNd8SR53ijVYTXL '+
               'JiOYc4mawGfI+MeitfXzGihQRMocImUOgQL7rqvFWTZWYMN+2wkqg5qWaCEt0UIqg7YNDURzIAtGI9YzbDJKVgydUfyx07ARGc0Bm8ROwygzlAX3F/s6gckdZKfKJ/xdWTDXlJt6n+5MBN '+
               'WWuVHwZH6pXN0ojtwA/tqOxueEt0uNY3zO6p50HSVNq6y1I0LNydegbtrMZhsR9TJxr+h2Zi3rIVzwSh0Q3z5WDXo95v7aahY8cifwyzFcBaa3QZ12jET77vqzbtqcbSce8/UoI2Hg+2ZF '+
               'gf58u4+8MMBAzN83byQC8gYwGzBdQb1a9JpwPhQst7dHHs0ex0OdiESBYJ5hk0qp9wppzLfB2zC8nHfilE8mzy69ZUqXqmNG3IFEib8r1Qv+J3f+AFvm52nI9Zh8AAAAAElFTkSuQmCC   ';

  CreatePanelBox(Parent);

  {Create Timer}
  TimerAnimation := TTimer.Create(PanelBox);
  TimerWaiting   := TTimer.Create(PanelBox);

  TimerAnimation.Interval := 15;
  TimerAnimation.OnTimer  := {$ifdef fpc}@{$endif}Animate;
  TimerAnimation.Enabled  := False;

  TimerWaiting.Interval := 2000;
  TimerWaiting.OnTimer  := {$ifdef fpc}@{$endif}Wait;
  TimerWaiting.Enabled  := False;
end;

procedure TToastMessage.Animate(Sender: TObject);
var
  PanelBoxMaxTop,PanelBoxMinTop:integer;
begin
  //Tag 0 Show
  if PanelBox.Tag = 0 then
  begin
    PanelBox.Visible := True;
    if (ToastDirection=tdTopCenter) or (ToastDirection=tdBottomCenter) then  //居中
      PanelBox.Left := Trunc(((Self.PanelBox.Parent as TForm).Width / 2) - (PanelBox.Width / 2));

    if (ToastDirection=tdTopLeft) or (ToastDirection=tdBottomLeft) then  //左对齐
      PanelBox.Left := 7;

    if (ToastDirection=tdTopRight) or (ToastDirection=tdBottomRight) then  //右对齐
      PanelBox.Left := ((Self.PanelBox.Parent as TForm).Width) - PanelBox.Width-7;

    if (ToastDirection=tdBottomCenter) or (ToastDirection=tdBottomLeft) or (ToastDirection=tdBottomRight) then  //从屏幕底向上
    begin
      dec(ToastHeith);
      PanelBoxMaxTop:=(Self.PanelBox.Parent as TForm).Height-abs(MinTop)-MaxTop;
      PanelBox.Top := ToastHeith ;
    end;
    if (ToastDirection=tdTopCenter) or (ToastDirection=tdTopLeft) or (ToastDirection=tdTopRight) then //从屏幕顶向下
    begin
      PanelBoxTop:= PanelBoxTop+1;
      PanelBoxMaxTop:= MaxTop;
      PanelBox.Top :=PanelBoxTop;
    end;

    if PanelBox.Top = PanelBoxMaxTop then
    begin
      TimerAnimation.Enabled := False;
      TimerWaiting.Enabled   := True;
      PanelBox.Tag           := 1;
    end;
  end
  else      //Tag 1 Hide
  if PanelBox.Tag = 1 then
  begin
    if (ToastDirection=tdBottomCenter) or (ToastDirection=tdBottomLeft) or (ToastDirection=tdBottomRight) then //从屏幕底向下收
    begin
      inc(ToastHeith);
      PanelBoxMinTop:= (Self.PanelBox.Parent as TForm).Height ;//-MaxTop;
      PanelBox.Top := ToastHeith ;
    end ;
    if (ToastDirection=tdTopCenter) or (ToastDirection=tdTopLeft) or (ToastDirection=tdTopRight) then   //从屏幕顶向上收
    begin
      PanelBoxTop:=PanelBoxTop-1;
      PanelBoxMinTop:=MinTop;
      PanelBox.Top :=PanelBoxTop;
    end;

    if PanelBox.Top = PanelBoxMinTop then
    begin
      TimerAnimation.Enabled := False;
      TimerWaiting.Enabled   := False;
      PanelBox.Tag           := 0;

      if (PanelBox.Parent is TForm) then
      begin
        if Assigned(Self.FFormOnResize) then
        begin
          (PanelBox.Parent as TForm).OnResize := Self.FFormOnResize;
        end;
      end;

      Self.SetParent(nil);
      Self.CheckMessageQueue; //if there are some new message, show it.
    end;
  end;
end;

procedure TToastMessage.Wait(Sender: TObject);
begin
  TimerAnimation.Enabled := True;
end;

{$ifdef fpc}
procedure TToastMessage.Base64ToPng(imagepng:TImage;const StringBase64: string);
var
  Input  : TStringStream;
  Outstream : TStringStream;
  Decoder   : TBase64DecodingStream;
begin
  Input := TStringStream.Create(StringBase64, TEncoding.ASCII);
  try
    Outstream:=TStringStream.Create('');
    try
      Decoder:=TBase64DecodingStream.Create(Input,bdmMIME);
      try
         Outstream.CopyFrom(Decoder,Decoder.Size);
      finally
        Decoder.Free;
      end;
      Outstream.Position:=0;
      imagepng.Picture.LoadFromStream(Outstream);
    finally
      Outstream.Free;
    end;
  finally
    Input.Free;
  end;
end;
{$else}
function TToastMessage.Base64ToPng(const StringBase64: string) : TPngImage;
var
  Input  : TStringStream;
  Output : TBytesStream;
begin
  Input := TStringStream.Create(StringBase64, TEncoding.ASCII);
  try
    Output := TBytesStream.Create;
    try
      TNetEncoding.Base64.Decode(Input, Output);
      Output.Position := 0;
      Result := TPngImage.Create;
      try
        Result.LoadFromStream(Output);
      except
        Result.Free;
        raise;
      end;
    finally
      Output.Free;
    end;
  finally
    Input.Free;
  end;
end;
{$endif}

procedure TToastMessage.CreatePanelBox(const Parent : TWinControl);
var
  PanelImage   : TPanel;
  PanelMessage : TPanel;
begin
  RegisterColors;

  {Create Principal Panel}
  PanelBox                  := TPanel.Create(nil);
  PanelBox.Name := 'MyPanelBox';
  PanelBox.Caption := '';
  PanelBox.Visible          := True;
  PanelBox.Parent           := Parent;
  PanelBox.BorderStyle      := {$ifndef fpc}Forms.{$endif}bsNone;
  PanelBox.Color            := PanelBoxColor;
  PanelBox.Height           := 38;
  PanelBox.Width            := 185;
  PanelBox.Top              := MinTop;
  PanelBox.BevelOuter       := bvNone;
  PanelBox.BevelInner       := bvNone;
  {$ifndef fpc}
  PanelBox.BevelKind        := bkNone;
  PanelBox.Ctl3d            := False;
  {$endif}
  PanelBox.ParentBackground := False;
  PanelBox.Tag              := 0;
  PanelBox.DoubleBuffered := True;

  {Create Panel Vertical Line}
  PanelLine                  := TPanel.Create(PanelBox);
  PanelLine.Name := 'MyPanelLine';
  PanelLine.Caption := '';
  PanelLine.Parent           := PanelBox;
  PanelLine.BorderStyle      := {$ifndef fpc}Forms.{$endif}bsNone;
  PanelLine.Align            := alLeft;
  PanelLine.BevelOuter       := bvNone;
  PanelLine.BevelInner       := bvNone;
  {$ifndef fpc}
  PanelLine.BevelKind        := bkNone;
  PanelLine.Ctl3d            := False;
  {$endif}
  PanelLine.Width            := 5;
  PanelLine.ParentBackground := False;
  PanelLine.Visible          := True;
  PanelLine.FullRepaint      := True;
  PanelLine.DoubleBuffered := True;

  {Create Image}
  PanelImage             := TPanel.Create(PanelBox);
  PanelImage.Name := 'MyPanelImage';
  PanelImage.Caption := '';
  PanelImage.Parent      := PanelBox;
  PanelImage.Visible     := True;
  PanelImage.Align       := alLeft;
  PanelImage.BevelOuter  := bvNone;
  PanelImage.BevelInner  := bvNone;
  {$ifndef fpc}
  PanelImage.BevelKind   := bkNone;
  {$endif}
  PanelImage.BorderStyle := {$ifndef fpc}Forms.{$endif}bsNone;
  PanelImage.Color       := PanelBoxColor;
  PanelImage.Height      := 38;
  PanelImage.Left        := 0;
  PanelImage.Width       := 31;
  PanelImage.DoubleBuffered := True;

  Image := TImage.Create(PanelImage);
  Image.Name := 'MyImage';

  Image.Align        := AlClient;
  Image.Parent       := PanelImage;
  Image.Visible      := True;
  Image.Center       := True;
  Image.Proportional := True;

  {Create Panel Message}
  PanelMessage             := TPanel.Create(PanelBox);
  PanelMessage.Name := 'MyPanelMessage';
  PanelMessage.Caption := '';
  PanelMessage.Parent      := PanelBox;
  PanelMessage.Visible     := True;
  PanelMessage.Align       := alClient;
  PanelMessage.BevelOuter  := bvNone;
  PanelMessage.BevelInner  := bvNone;
  {$ifndef fpc}
  PanelMessage.BevelKind   := bkNone;
  {$endif}
  PanelMessage.BorderStyle := {$ifndef fpc}Forms.{$endif}bsNone;
  PanelMessage.Color       := PanelBoxColor;
  PanelMessage.DoubleBuffered := True;

  {Create Title}
  Title := TLabel.Create(PanelMessage);
  Title.Name := 'LabelTitle';

  Title.Parent      := PanelMessage;
  Title.AutoSize    := True;
  Title.Align       := AlTop;
  Title.Alignment   := taCenter;
  Title.Layout      := tlCenter;
  Title.WordWrap    := True;
  Title.Enabled     := True;
  Title.Font.Color  := TitleColor;
  Title.Font.Name   := ToastFontName;
  Title.Font.Size   := 10;
  Title.Transparent := True;
  Title.Font.Style  := [fsBold];
  Title.Top         := 0;

 {Create Text}
  Text := TLabel.Create(PanelMessage);
  Text.Name := 'LabelText';

  Text.Parent       := PanelMessage;
  Text.AutoSize     := True;
  Text.Align        := alClient;
  Text.Alignment    := taCenter;
  Text.Layout       := tlCenter;
  Text.WordWrap     := True;
  Text.Enabled      := True;
  Text.Font.Color   := TextColor;
  Text.Font.Name    := ToastFontName;
  Text.Font.Size    := 8;
  Text.Transparent  := True;
  Text.Font.Style   := [fsBold];
  {$ifndef fpc}
  Text.AlignWithMargins := True;
  {$endif}
  Text.Transparent := True;
end;

destructor TToastMessage.Destroy;
begin
  if Assigned(PanelBox) then
  begin
    if PanelBox.Name <> '' then
    begin
      FreeAndNil(PanelBox);
    end;
  end;
end;

function TToastMessage.GetIsShowing: Boolean;
begin
  Result := (Self.TimerAnimation.Enabled) or (Self.TimerWaiting.Enabled);
end;

procedure TToastMessage.PanelBoxPosition(Sender: TObject);
begin
  inherited;

  if not Assigned(Sender) then Exit;

  PanelBox.Left := Trunc(((Sender as TForm).Width / 2) - (PanelBox.Width / 2));

  if Assigned(Self.FFormOnResize) then
  begin
    FFormOnResize(nil);
  end;
end;

class procedure TToastMessage.ReleaseMe;
begin
  if Assigned(FToastMessage) then
  begin
    FreeAndNil(FToastMessage);
  end;
end;

procedure TToastMessage.RegisterColors;
begin
  PanelBoxColor := clWhite;
  TitleColor    := $003F3F3F;
  TextColor     := $00616161;
  SuccessColor  := $0064D747;
  InfoColor     := $00EA7012;
  ErrorColor    := $003643F4;
end;

procedure TToastMessage.SetParent(const Parent: TWinControl);
begin
  //add by pcplayer
  Self.PanelBox.Parent := Parent;

  if not Assigned(Parent) then Exit;

  PanelBoxPosition(Parent);

  if Parent is TForm then
  begin
    Self.FFormOnResize := (Parent as TForm).OnResize;
    (Parent as TForm).OnResize :={$ifdef fpc}@{$endif}PanelBoxPosition;
  end;
end;

procedure TToastMessage.Toast(const Parent: TWinControl;
  const MessageType: tpMode; pTitle, pText: string;TD:TToastDirection;_ToastFontName:string=ToastFontName);
var
  hs, tmp: Integer;
begin
  Title.Font.Name:=ToastFontName;
  Text.Font.Name:=ToastFontName;
  if Self.IsShowing then
  begin
    if not Assigned(FMessageList) then
    begin
      FMessageList := TStringList.Create;
    end;
    FMessageList.AddObject(Ord(MessageType).ToString + ';' + pTitle + ';' + pText+';'+ord(td).ToString, Parent); //push message to queue;2024-5-12
    Exit;
  end
  else   ToastDirection:=td;

  //秋风
  //计算宽度
  OldPanelBoxTop:=PanelBox.Top;
  PanelBox.Top:=-50;

  PanelBoxTop:=PanelBox.Top;

  PanelBox.Parent := Parent;
  PanelBox.Height := 50;
  PanelBox.Width := 50 + Text.Canvas.TextWidth(pText);
  tmp := Text.Canvas.TextWidth('W');
  if PanelBox.Width > ((Parent as TForm).Width / 2) then
  begin
    PanelBox.Width:=50+Text.Canvas.TextWidth(pText) div 2;
    if PanelBox.Width> (Self.PanelBox.Parent as TForm).Width then
      PanelBox.Width:=(Self.PanelBox.Parent as TForm).Width -10;
    hs:= (PanelBox.Width-50) div tmp; //每行容纳n个字符
    hs:=utf8length(pText) div hs;  //n行
    PanelBox.Height:=Title.Height+Text.Height+(hs-1)*Text.Canvas.TextHeight(pText);
    MinTop:=-PanelBox.Height;
  end;

  ToastHeith:=(Self.PanelBox.Parent as TForm).Height;

  Self.SetParent(Parent);
  Self.Toast(MessageType, pTitle, pText,td,_ToastFontName);
end;

class procedure TToastMessage.ToastIt(const Parent : TWinControl; const MessageType: tpMode; pTitle,
  pText: string;TD:TToastDirection =tdTopCenter;_ToastFontName:string=ToastFontName);
begin
  if not Assigned(FToastMessage) then
  begin
    FToastMessage := TToastMessage.Create(Parent);
  end;

  FToastMessage.Toast(Parent, MessageType, pTitle, pText,td,_ToastFontName);
end;

procedure TToastMessage.Toast(const MessageType : tpMode; pTitle, pText : string;TD:TToastDirection;_ToastFontName:string=ToastFontName);
begin
  Self.PanelBox.BringToFront; //Z轴方向放到最顶上； //pcplayer
  Title.Caption := pTitle;
  Text.Caption  := pText;
  Title.Font.Name:=_ToastFontName;
  Text.Font.Name:=_ToastFontName;

  if MessageType = tpSuccess then
  begin
    PanelLine.Color := SuccessColor;
    {$ifdef fpc}
    Base64ToPng(Image,Trim(SuccessImage));
    {$else}
    Image.Picture.Assign(Base64ToPng(Trim(SuccessImage)));
    {$endif}
  end
  else if MessageType = tpInfo then
  begin
    PanelLine.Color := InfoColor;
    {$ifdef fpc}
    Base64ToPng(Image,Trim(InfoImage));
    {$else}
    Image.Picture.Assign(Base64ToPng(Trim(InfoImage)));
    {$endif}
  end
  else if MessageType = tpError then
  begin
    PanelLine.Color := ErrorColor;
    {$ifdef fpc}
    Base64ToPng(Image,Trim(ErrorImage));
    {$else}
    Image.Picture.Assign(Base64ToPng(Trim(ErrorImage)));
    {$endif}
  end;

  //Start Toast
  TimerAnimation.Enabled := True;
end;

initialization

finalization
  TToastMessage.ReleaseMe;

end.
