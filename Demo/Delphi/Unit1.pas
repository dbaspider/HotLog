unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    btnTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  HotLog;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  { init }
  hLog.hlWriter.hlFileDef.append := True;
  hLog.hlWriter.hlFileDef.LogFileMaxSize := 1024*1024*5; // limit file size to 5MB
end;

procedure TForm1.btnTestClick(Sender: TObject);
var
  i: Integer;
begin
  { test }
  for i := 1 to 10 do
  begin
    hLog.Add('{now} A log line ' + inttostr(i));
    hLog.Add('{gtc} a log line ' + inttostr(i));
  end;
end;

end.
