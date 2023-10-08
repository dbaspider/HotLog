unit unit21;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    PanelTop:      TPanel;
      ButtonStart: TButton;
    Memo1:         TMemo;
    StatusBar1:    TStatusBar;
    procedure ButtonStartClick (Sender: TObject);
    procedure FormCreate       (Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  HotLog;

{ TForm1 }

procedure TForm1.ButtonStartClick (Sender: TObject);
var
  i: integer;
begin
  For i:=1 to 1000 do
    hLog.Add('This is event number :' + IntToStr(i));
  Memo1.Lines.Add('Done');
end;

procedure TForm1.FormCreate (Sender: TObject);
begin
  hLog.hlWriter.hlFileDef.ext := 'log';
  hLog.hlWriter.hlFileDef.UseSafeFilenames := True;
  hLog.hlWriter.hlFileDef.LogFileMaxSize := 1024;
  hLog.hlWriter.hlFileDef.ddname:=ChangeFileExt(ExtractFileName(Application.ExeName),'');
  hLog.hlWriter.hlFileDef.SafeGdgMax:=10;
  hLog.hlWriter.hlFileDef.BuildSafeFileName;
  Memo1.Lines.Add(hLog.hlWriter.hlFileDef.fileName);
end;

end.

