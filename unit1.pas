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
    Button2: TButton;
    ToggleBox1: TToggleBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
    procedure ToggleBox2Change(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  TToastMessage.ToastIt(Self, tpSuccess,'Success','Toast lazarus的版本可以跨平台使用.Toast lazarus的版本可以跨平台使用.');
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


end.

