unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uTToastMessage;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ToggleBox1: TToggleBox;
    ToggleBox2: TToggleBox;
    ToggleBox3: TToggleBox;
    ToggleBox4: TToggleBox;
    ToggleBox5: TToggleBox;
    ToggleBox6: TToggleBox;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
    procedure ToggleBox2Change(Sender: TObject);
    procedure ToggleBox3Change(Sender: TObject);
    procedure ToggleBox4Change(Sender: TObject);
    procedure ToggleBox5Change(Sender: TObject);
    procedure ToggleBox6Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToggleBox1Change(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpError,'Error','Hello, found a error!Toast lazarus的版本可以跨平台使用.Hello, found a error!Toast lazarus的版本可以跨平台使用.');
end;

procedure TForm1.ToggleBox2Change(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpError,'Error','Hello, found a error!Toast lazarus的版本可以跨平台使用.Hello, found a error!Toast lazarus的版本可以跨平台使用.',2);
end;

procedure TForm1.ToggleBox3Change(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpError,'Error','Hello, found a error!Toast lazarus的版本可以跨平台使用.Hello, found a error!Toast lazarus的版本可以跨平台使用.',3);
end;

procedure TForm1.ToggleBox4Change(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpError,'Error','Hello, found a error!Toast lazarus的版本可以跨平台使用.Hello, found a error!Toast lazarus的版本可以跨平台使用.',4);
end;

procedure TForm1.ToggleBox5Change(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpError,'Error','Hello, found a error!Toast lazarus的版本可以跨平台使用.Hello, found a error!Toast lazarus的版本可以跨平台使用.',5);
end;

procedure TForm1.ToggleBox6Change(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpError,'Error','Hello, found a error!Toast lazarus的版本可以跨平台使用.Hello, found a error!Toast lazarus的版本可以跨平台使用.',6);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpSuccess,'Success','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.');
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpInfo,'Info','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',5);
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpInfo,'Info','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',6);
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpSuccess,'Success','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',6);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpInfo,'Info','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpSuccess,'Success','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',2);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpInfo,'Info','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',2);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpSuccess,'Success','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',3);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpSuccess,'Success','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',4);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpInfo,'Info','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',3);
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpInfo,'Info','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',4);
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpSuccess,'Success','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.',5);
end;


end.

