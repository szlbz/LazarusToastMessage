unit uTToastMessage;
{------------------------------------------------------------------------------
秋风(QQ:315795176)
  来自https://github.com/pcplayer/DelphiToastMessage
  基于pcplayer版本，修改后适用于lazarus,并可以跨平台使用
1、根据显示内容调整totast的宽度，防止显示内容不完整。

秋风 2024-05-12

pcplayer修改日志：
  来自https://github.com/desenvolvimentojd3/DelphiToastMessage
  我对它进行了一些修改：
  1. 增加一个全局变量  ToastMessage: TToastMessage; 这样就一个实例可以显示到所有 Form 里面去，
     而不是它的例子那样，每个 Form 都需要自己创建实例；
  2. 因为要一个实例，显示到所有 Form 里面去，因此增加了一个 Toast 方法，给 Parent；
  3. 因为整个程序一个实例，所以本单元自己创建实例，使用 initialization

  todo: 上述修改都基于一种情况：每次只显示一条信息。
        如果一次要显示很多条，类似一下弹出好几条信息，依次伸出多条显示，然后依次缩回去，就需要多个实例。
        这种情况下，需要增加一些代码，比如内部增加一个 List 来存放多个实例。

  pcplayer 2024-5-11
--------------------------------------------------------------------------------}
interface

uses
     System.NetEncoding,
     lazutf8,
     Graphics,
     Controls,
     Extctrls,
     StdCtrls,
     Classes,
     SysUtils,
     Forms,
     Messages;

type tpMode = (tpSuccess,tpInfo,tpError);

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

      procedure Base64ToPng(imagepng:TImage;const StringBase64: string);
      var
        Timer : TTimer;

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

        TimerAnimation : TTimer;
        TimerWaiting   : TTimer;

        PanelBoxColor : TColor;
        TitleColor    : TColor;
        TextColor     : TColor;
        SuccessColor  : TColor;
        InfoColor     : TColor;
        ErrorColor    : TColor;
       class var FToastMessage: TToastMessage;
   public
      procedure Toast(const MessageType : tpMode; pTitle, pText : string); overload;
      procedure Toast(const Parent: TWinControl; const MessageType : tpMode; pTitle, pText : string); overload;

      constructor Create(const Parent : TWinControl); overload;
      destructor Destroy; override;
      class procedure ToastIt(const Parent : TWinControl; const MessageType : tpMode; pTitle, pText : string);
      class procedure RealseMe;
  end;

var
  ToastMessage: TToastMessage;

implementation

{ ToastMessage }

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
  TimerAnimation := TTimer.Create(Parent);
  TimerWaiting   := TTimer.Create(Parent);

  TimerAnimation.Interval := 15;
  TimerAnimation.OnTimer  := @Animate;
  TimerAnimation.Enabled  := False;

  TimerWaiting.Interval := 2000;
  TimerWaiting.OnTimer  := @Wait;
  TimerWaiting.Enabled  := False;
end;

procedure TToastMessage.Animate(Sender: TObject);
begin
  //Tag 0 Show
  if PanelBox.Tag = 0 then
    begin
      PanelBox.Visible := True;

      PanelBox.Top := PanelBox.Top + 1;

      if PanelBox.Top = MaxTop then
        begin
          TimerAnimation.Enabled := False;
          TimerWaiting.Enabled   := True;
          PanelBox.Tag           := 1;
        end;
    end
  //Tag 1 Hide
  else if PanelBox.Tag = 1 then
    begin
      PanelBox.Top := PanelBox.Top - 1;

      if PanelBox.Top = MinTop then
        begin
          TimerAnimation.Enabled := False;
          TimerWaiting.Enabled   := False;
          PanelBox.Tag           := 0;
          PanelBox.Parent := nil;
        end;
    end;
end;

procedure TToastMessage.Wait(Sender: TObject);
begin
  TimerAnimation.Enabled := True;
end;

procedure TToastMessage.Base64ToPng(imagepng:TImage;const StringBase64: string) ;
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
      try
        imagepng.Picture.LoadFromStream(Output);
      except
        raise;
      end;
    finally
      Output.Free;
    end;
  finally
    Input.Free;
  end;
end;

procedure TToastMessage.CreatePanelBox(const Parent : TWinControl);
var
  PanelImage   : TPanel;
  PanelMessage : TPanel;
begin
  RegisterColors;

  {Create Principal Panel}
  PanelBox                  := TPanel.Create(nil);
  PanelBox.Visible          := True;
  PanelBox.Parent           := Parent;
  PanelBox.BorderStyle      := bsNone;
  PanelBox.Color            := PanelBoxColor;
  PanelBox.Height           := 38;
  PanelBox.Width            := 185;
  PanelBox.Top              := MinTop;
  PanelBox.BevelOuter       := bvNone;
  PanelBox.BevelInner       := bvNone;
  PanelBox.ParentBackground := False;
  PanelBox.Tag              := 0;

  {Create Panel Vertical Line}
  PanelLine                  := TPanel.Create(PanelBox);
  PanelLine.Parent           := PanelBox;
  PanelLine.BorderStyle      := bsNone;
  PanelLine.Align            := alLeft;
  PanelLine.BevelOuter       := bvNone;
  PanelLine.BevelInner       := bvNone;
  PanelLine.Width            := 5;
  PanelLine.ParentBackground := False;
  PanelLine.Visible          := True;
  PanelLine.FullRepaint      := True;

  {Create Image}
  PanelImage             := TPanel.Create(PanelBox);
  PanelImage.Parent      := PanelBox;
  PanelImage.Visible     := True;
  PanelImage.Align       := alLeft;
  PanelImage.BevelOuter  := bvNone;
  PanelImage.BevelInner  := bvNone;
  PanelImage.BorderStyle := bsNone;
  PanelImage.Color       := PanelBoxColor;
  PanelImage.Height      := 38;
  PanelImage.Left        := 0;
  PanelImage.Width       := 31;

  Image := TImage.Create(PanelImage);

  Image.Align        := AlClient;
  Image.Parent       := PanelImage;
  Image.Visible      := True;
  Image.Center       := True;
  Image.Proportional := True;

  {Create Panel Message}
  PanelMessage             := TPanel.Create(PanelBox);
  PanelMessage.Parent      := PanelBox;
  PanelMessage.Visible     := True;
  PanelMessage.Align       := alClient;
  PanelMessage.BevelOuter  := bvNone;
  PanelMessage.BevelInner  := bvNone;
  PanelMessage.BorderStyle := bsNone;
  PanelMessage.Color       := PanelBoxColor;

  {Create Title}
  Title := TLabel.Create(PanelMessage);

  Title.Parent      := PanelMessage;
  Title.AutoSize    := True;
  Title.Align       := AlTop;
  Title.Alignment   := taCenter;
  Title.Layout      := tlCenter;
  Title.WordWrap    := True;
  Title.Enabled     := True;
  Title.Font.Color  := TitleColor;
  Title.Font.Name   := 'Segoe UI';
  Title.Font.Size   := 10;
  Title.Transparent := True;
  Title.Font.Style  := [fsBold];
  Title.Top         := 0;

 {Create Text}
  Text := TLabel.Create(PanelMessage);

  Text.Parent       := PanelMessage;
  Text.AutoSize     := True;
  Text.Align        := alClient;
  Text.Alignment    := taCenter;
  Text.Layout       := tlCenter;
  Text.WordWrap     := True;
  Text.Enabled      := True;
  Text.Font.Color   := TextColor;
  Text.Font.Name    := 'Segoe UI';
  Text.Font.Size    := 8;
  Text.Transparent  := True;
  Text.Font.Style   := [fsBold];

  if Assigned(Parent) then
  begin
    Self.SetParent(Parent);
  end;
end;

destructor TToastMessage.Destroy;
begin
  if Assigned(PanelBox) then
    PanelBox.Destroy;

  if Assigned(TimerAnimation) then
    TimerAnimation.Destroy;

  if Assigned(TimerWaiting) then
    TimerWaiting.Destroy;
end;

procedure TToastMessage.PanelBoxPosition(Sender: TObject);
begin
  inherited;
  PanelBox.Left := Trunc(((Sender as TForm).Width / 2) - (PanelBox.Width / 2));
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
  PanelBoxPosition(Parent);

  if Parent is TForm then
    (Parent as TForm).OnResize := @PanelBoxPosition;
end;

class procedure TToastMessage.RealseMe;
begin
  if Assigned(FToastMessage) then
  begin
    FreeAndNil(FToastMessage);
  end;
end;

class procedure TToastMessage.ToastIt(const Parent : TWinControl; const MessageType: tpMode; pTitle,
  pText: string);
begin
  if not Assigned(FToastMessage) then
  begin
    FToastMessage := TToastMessage.Create(Parent);
  end;

  FToastMessage.Toast(Parent, MessageType, pTitle, pText);
end;

procedure TToastMessage.Toast(const Parent: TWinControl;
  const MessageType: tpMode; pTitle, pText: string);
begin
  Self.SetParent(Parent);
  Self.Toast(MessageType, pTitle, pText);
end;

procedure TToastMessage.Toast(const MessageType : tpMode; pTitle, pText : string);
var hs,tmp:integer;
begin
  Self.PanelBox.BringToFront; //Z轴方向放到最顶上； //pcplayer
  Title.Caption := pTitle;
  Text.Caption  := pText;
  //秋风
  PanelBox.Height:=50;
  PanelBox.Width:=50+Text.Canvas.TextWidth(pText);
  tmp:=Text.Canvas.TextWidth('W');
  if PanelBox.Width>(Self.PanelBox.Parent as TForm).Width then
  begin
    PanelBox.Width:=50+Text.Canvas.TextWidth(pText) div 2;
    if PanelBox.Width> (Self.PanelBox.Parent as TForm).Width then
      PanelBox.Width:=(Self.PanelBox.Parent as TForm).Width -10;
    hs:= (PanelBox.Width-50) div tmp; //每行容纳n个字符
    hs:=utf8length(pText) div hs;  //n行
    PanelBox.Height:=Title.Height+Text.Height+(hs-2)*Text.Canvas.TextHeight(pText);
    MinTop:=-47*(hs-1);
  end;
  PanelBox.Left := Trunc(((Self.PanelBox.Parent as TForm).Width / 2) - (PanelBox.Width / 2));
  //秋风
  if MessageType = tpSuccess then
    begin
      PanelLine.Color := SuccessColor;
      Base64ToPng(Image,Trim(SuccessImage));
    end
  else if MessageType = tpInfo then
    begin
      PanelLine.Color := InfoColor;
      Base64ToPng(Image,Trim(InfoImage));
    end
  else if MessageType = tpError then
    begin
      PanelLine.Color := ErrorColor;
      Base64ToPng(Image,Trim(ErrorImage));
    end;

  //Start Toast
  TimerAnimation.Enabled := True;
end;

initialization
  ToastMessage := TToastMessage.Create(nil);

finalization
  ToastMessage.Free;  //一旦使用，就会给它设置 Parent，一旦有 Parent，某个 Form 退出关闭时，就会消灭它。因此，必须在隐藏后，取消它的 Parent;


end.
