unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uTToastMessage;

type

  { TForm1 }

  TForm1 = class(TForm)
    ToggleBox1: TToggleBox;
    procedure ToggleBox1Change(Sender: TObject);
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
  TToastMessage.ToastIt(Self, tpError,'Error','Hello, found a error!Toast lazarus的版本可以跨平台使用.');
end;

end.

