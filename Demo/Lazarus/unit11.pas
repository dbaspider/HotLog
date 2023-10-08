unit unit11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1:       TPanel;
      BitBtnTest: TBitBtn;
    Memo1:        TMemo;
    StatusBar1:   TStatusBar;

    procedure BitBtnTestClick (Sender: TObject);
    procedure FormCreate      (Sender: TObject);

  private
    { private declarations }

  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  HotLog;


{ TForm1 }

procedure TForm1.BitBtnTestClick (Sender: TObject);
var
  i64:     int64;
  short:   ShortString;
  ansi:    AnsiString;
  wide:    WideString;
  unicode: UnicodeString;
  curr:    currency;
  qw:      QWord;
  v:       variant;

  i:       integer;
  x,y,z:   real;
begin
  short  := 'This is a short string';
  ansi   := 'This is an ANSI string';
  wide   := UTF8Decode('Δυο σαυσαε ασομμοδαρε αδ is a WideString');
  unicode:= UTF8Decode('竹比表本政殊廟面有補細真野稿第 is a UnicodeString');
  i64    := High(int64);
  curr   := 87.35;
  qw     := High(QWord);

//  hLog.ModifyLineNumbering(true);
  if FileExists(hLog.hlWriter.hlFileDef.fileName) then
    hLog.Add('{/}{LNumOff}{*80*}');
  hLog.Add('{now} 1. Zeile{/}{@21}2. Zeile');
  hLog.Add('');
  hLog.Add('{now} 4. Zeile');
  hLog.Add('{/}');
  hLog.Add('{now} 7. Zeile');

{ hLog.Add('{/}{LNumOff}{*80*}');
  hLog.Add('{now} {app_name} {@40}Version {app_ver}');
  hLog.Add('{@21} from:      {@40}{app_path}');
  hLog.Add('{@21} parameter: {@40}{app_prm-}');

  hLog.ModifyLineNumbering(true);

  hLog.Add('');
  hLog.AddStr('** Add(''{app_name}'') **');
  hLog.Add('{app_name}');

  hLog.Add('');
  hLog.AddStr('** Add(''{app_path}'') **');
  hLog.Add('{app_path}');

  hLog.Add('');
  hLog.AddStr('** Add(''{app_lfn}'') **');
  hLog.Add('{app_lfn}');

  hLog.Add('');
  hLog.AddStr('** Add(''{app_prm}'') **');
  hLog.Add('{app_prm}');

  hLog.Add('');
  hLog.AddStr('** Add(''{app_prm-}'') **');
  hLog.Add('{app_prm-}');

  hLog.Add('');
  hLog.AddStr('** Add(''{dte}'') **');
  hLog.Add('{dte}');

  hLog.Add('');
  hLog.AddStr('** Add(''{hms}'') **');
  hLog.Add('{hms}');

  hLog.Add('');
  hLog.AddStr('** Add(''{now}'') **');
  hLog.Add('{now}');

  hLog.Add('');
  hLog.AddStr('** Add(''{gtc}'') **');
  hLog.Add('{gtc}');

  hLog.Add('');
  hLog.AddStr('** Add(''{dhg}'') **');
  hLog.Add('{dhg}');

  hLog.Add('');
  hLog.AddStr('** Line number: {lNum}'') **');
  hLog.Add('Line number: {lNum}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Ruler}'') **');
  hLog.Add('{Ruler}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Ruler+}'') **');
  hLog.Add('{Ruler+}');

  hLog.Add('');
  hLog.AddStr('** Add(''{*9Δ}|10Δ{@20}|20Δ{*27Δ}30|{@19@}σαυ|40α{}|50{80@}Δ80|'') **');
  hLog.Add('{*9Δ}|10Δ{@20}|20Δ{*27Δ}30|{@19@}σαυ|40α{}|50{80@}Δ80|');

  hLog.Add('');
  hLog.AddStr('** Add(''{@10}This is the first part{@50}second part'') **');
  HLog.Add('{@10}This is the first part{@50}second part');

  hLog.Add('');
  hLog.AddStr('** Add(''{@10}Δυο σαυσαε ασομμοδαρε αδ,{@50}δισαμ αππαρεατ'') **');

  HLog.Add('{@10}Δυο σαυσαε ασομμοδαρε αδ,{@50}δισαμ αππαρεατ');

  hLog.Add('');
  hLog.AddStr('** Add(''{30@}This is the first part{@50}second part'') **');
  HLog.Add('{30@}This is the first part{@50}second part');

  hLog.Add('');
  hLog.AddStr('** Add(''{30@}Δυο σαυσαε{@50}ασομμοδαρε'') **');
  HLog.Add('{30@}Δυο σαυσαε{@50}ασομμοδαρε');

  hLog.Add('');
  hLog.AddStr('** Add(''{@40@}This is centered'') **');
  HLog.Add('{@40@}This is centered');

  hLog.Add('');
  hLog.AddStr('** Add(''{@40@}Δυο σαυσαε ασομμοδαρε αδ'') **');
  HLog.Add('{@40@}Δυο σαυσαε ασομμοδαρε αδ');

  hLog.Add('');
  hLog.AddStr('** Add(''Left part{@25@}Middle string{}End of the line'') **');
  hLog.Add('Left part{@25@}Middle string{}End of the line');

  hLog.Add('');
  hLog.AddStr('** Add(''Δυο σαυσαε{@25@}ασομμοδαρε{}δισαμ αππαρεατ'') **');
  hLog.Add('Δυο σαυσαε{@25@}ασομμοδαρε{}δισαμ αππαρεατ');

  hLog.Add('');
  hLog.AddStr('** Add(''Padding{*19.}up to position 19{@50}|50'') **');
  HLog.Add('Padding{*19.}up to position 19{@50}|50');

  hLog.Add('');
  hLog.AddStr('** Add(''Δυο {*19Δ} αυσαε ασομμοδαρε αδ{@50}|50'') **');
  HLog.Add('Δυο {*19Δ} αυσαε ασομμοδαρε αδ{@50}|50');

  hLog.Add('');
  hLog.AddStr('** Add(''{*19Δ} αυσαε ασομμοδαρε αδ{@50}|50'') **');
  HLog.Add('{*19Δ} αυσαε ασομμοδαρε αδ{@50}|50');

  hLog.Add('');
  hLog.AddStr('** Add(''Left part{@25@}Middle string{}End of the line'') **');
  hLog.Add('Left part{@25@}Middle string{}End of the line');

  hLog.Add('');
  hLog.AddStr('** AddStr(''Δυο σαυσαε ασομμοδαρε αδ'') **');
  hLog.AddStr('Δυο σαυσαε ασομμοδαρε αδ');

  hLog.Add('');
  hLog.AddStr('** Add(''Δυο σαυσαε ασομμοδαρε αδ'') **');
  hLog.Add('Δυο σαυσαε ασομμοδαρε αδ');

  hLog.Add('');
  hLog.AddStr('** Add(''{OSVI}'') **');
  hLog.Add('{OSVI}');

  hLog.Add('');
  hLog.AddStr('** Add(''{CPUI}'') **');
  hLog.Add('{CPUI}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Mem}'') **');
  hLog.Add('{Mem}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Disk}'') **');
  hLog.Add('{Disk}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Disk-}'') **');
  hLog.Add('{Disk-}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Disk--}'') **');
  hLog.Add('{Disk--}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Disk---}'') **');
  hLog.Add('{Disk---}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Ram}'') **');
  hLog.Add('{Ram}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Ram-}'') **');
  hLog.Add('{Ram-}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Ram--}'') **');
  hLog.Add('{Ram--}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Ram---}'') **');
  hLog.Add('{Ram---}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Heap}'') **');
  hLog.Add('{Heap}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Heap-}'') **');
  hLog.Add('{Heap-}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Heap--}'') **');
  hLog.Add('{Heap--}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Heap---}'') **');
  hLog.Add('{Heap---}');

  hLog.Add('');
  hLog.AddStr('** Add(''{Timers}'') **');
  hLog.Add('{Timers}');

  Randomize;
  i:=round(Random(100));

  hLog.Add('');
  hLog.AddStr('** Add(vsNone,[''aString'',''{/}another one, followed by integers '',1,2,3,''{/}and now, the "i" value: '',i]) **');
  hLog.Add(vsNone,['aString','{/}another one, followed by integers ',
                   1,2,3,'{/}and now, the "i" value: ',i]);

  hLog.Add('');
  hLog.AddStr('** Add(vsBasic,[''aString'',''another one, followed by integers'',1,2,3,''and now, the "i" value:'',i]) **');
  hLog.Add(vsBasic,['aString','another one, followed by integers',
                    1,2,3,'and now, the "i" value:',i]);

  hLog.Add('');
  hLog.AddStr('** Add(vsExtended,[''aString'',''another one, followed by integers'',1,2,3,''and now, the "i" value:'',i]) **');
  hLog.Add(vsExtended,['aString','another one, followed by integers',
                       1,2,3,'and now, the "i" value:',i] );

  hLog.Add('');
  hLog.AddStr('** Add(vsNone,[Sender,'' at '',@Sender]) **');
  hLog.Add(vsNone,[Sender,' at ',@Sender]);

  hLog.Add('');
  hLog.AddStr('** Add(vsBasic,[Sender,''at'',@Sender]) **');
  hLog.Add(vsBasic,[Sender,'at',@Sender]);

  hLog.Add('');
  hLog.AddStr('** Add(vsExtended,[Sender,''at'',@Sender]) **');
  hLog.Add(vsExtended,[Sender,'at',@Sender]);

  hLog.Add('');
  hLog.Add(vsExtended,[1]);
  hLog.Add(vsExtended,[true]);
  hLog.Add(vsExtended,[ansi]);
  hLog.Add(vsExtended,[ansi[1]]);
  hLog.Add(vsExtended,[Pi]);
  hLog.Add(vsExtended,[short]);
  hLog.Add(vsExtended,[@Sender]);
  hLog.Add(vsExtended,[pAnsiChar(@ansi[1])]);
  hLog.Add(vsExtended,[Sender]);
  hLog.Add(vsExtended,[wide]);
  hLog.Add(vsExtended,[wide[1]]);
  hLog.Add(vsExtended,[pWideChar(@wide[1])]);
  hLog.Add(vsExtended,[pAnsiString(@ansi)]);
  hLog.Add(vsExtended,[curr]);
  v:=Pi;
  hLog.Add(vsExtended,[v]);
  v:=ansi;
  hLog.Add(vsExtended,[v]);
  v:=ansi[1];
  hLog.Add(vsExtended,[v]);
  v:=wide[1];
  hLog.Add(vsExtended,[v]);
  v:=unicode[1];
  hLog.Add(vsExtended,[v]);
  hLog.Add(vsExtended,[i64]);
  hLog.Add(vsExtended,[unicode]);
  hLog.Add(vsExtended,[qw]);

  hLog.Add('');
  hLog.SetErrorCaption('ΔΔΔ    E R R O R    ΔΔΔ','ασομμοδαρε');
  hLog.SetErrorViewStyle(vsExtended);
  hLog.AddStr('** AddException(e,''Δυο σαυσαε ασομμοδαρε αδ'',[Sender,x,y,z,''Some additional information'']) **');
  x:=2.0;
  y:=1.0;
  z:=0.0;
  try
    x:=y/z;
  except
    on e:Exception do
      hLog.AddException(e,'Δυο σαυσαε ασομμοδαρε αδ',[Sender,x,y,z,'Some additional information']);
  end;
}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  hLog.hlWriter.hlFileDef.path:=ExtractFilePath(Application.ExeName);
  hLog.hlWriter.hlFileDef.ddname:=ChangeFileExt(ExtractFileName(Application.ExeName),'');
  hLog.hlWriter.hlFileDef.append:=true;
  hLog.hlWriter.hlFileDef.gdgMax:=0;
  //hLog.hlWriter.hlFileDef.UseFileSizeLimit:=true;
  hLog.hlWriter.hlFileDef.LogFileMaxSize:=OneMegabyte;
  hLog.DisplayFeedBackInto(Memo1);
  hLog.ScrollMemo(true);
  BitBtnTest.Click;
end;

end.

