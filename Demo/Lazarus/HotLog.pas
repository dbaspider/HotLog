unit HotLog;

    (*********************************************************************
      v 3.3 (2019-02) Rolf Wetjen (rolf.wetjen@mail.de)
        - CheckLogFiles moved to THotLogWriter.Execute
        - THotLogWriter.Execute: F is TFileStreamUTF8
        - References to unit RWUtils removed
        - Additional index FSafeIndex for safe filenames in THLFileDef
        - ddname can be used to set a prefix for safe filenames.
          Filename in this case: ddname-yyyymmddd-hhnnss-FSafeIndex
        - SafeGdgMax now working
        - UseFileSizeLimit, FUseSizeLimit removed.
          Set LogFileMaxSize > 0 instead.
        - References to CodeSiteLogging removed.

      v 3.2.1 (2017-07) Rolf Wetjen (rolf.wetjen@mail.de)
        - Bug fixing

      v 3.2 (2017-01) Rolf Wetjen (rolf.wetjen@mail.de)
      	- FormatDateTime(..,Now) replaced with DateTimeToStr
        - THotLogWriter CreateFile & OpenFile with flag FILE_FLAG_WRITE_THROUGH

      v 3.1 (2016-05) Rolf Wetjen (rolf.wetjen@mail.de)
        - No need to call THotLog.StartLogging. THotLog.Add... will do this.
        - New property THotLog.Started
        Bug fixing

      v 3.0 (2016-04) Rolf Wetjen (rolf.wetjen@mail.de)
        - Support for Delphi XE7
        - Logfile is UTF8 encoded
        - Heap monitoring changed (GetMemoryManagerState)
        - Ram monitoring changed  (GlobalMemoryStatusEx)
        - Unicode support for "array of const" functions
        - {disk...} tags show information for  all disks
        - Delphi & Lazarus
        - {app_prm} is now a standalone tag
        Bug fixing, of course

      Thanks to Olivier Touzot for earlier versions.
    *********************************************************************)

    { ****************************************************************** }
    {                                                                    }
    {         Delphi (6&7) unit  --   LogFile manager, buffered and      }
    {                                 multithreaded.                     }
    {                                                                    }
    {         Copyright © 2004 by Olivier Touzot "QnnO"                  }
    {    (http://mapage.noos.fr/qnno/delphi_en.htm  -  qnno@noos.fr)     }
    {                                                                    }
    {                  ----------------------------                      }
    {                                                                    }
    { v 2.0 (2005-07-10).                                                }
    {   Thanks to Robert Becker's work, (rbecker@compuserve.com) the log }
    {   file size can now be limited, and Hotlog will create a new one   }
    {   once a limit is reached.                                         }
    { v 2.1 (2005-07-24).                                                }
    {   Support for Delphi 5 added by Femi Fadayomi.                     }
    { v 2.2 (2005-07-24).  Luis Gonzalo Constantini Von Rickel           }
    {   - Faster Memo Cleanning, Stop Repainting During Process, and do  }
    {     the Scroll to the Last Line of the Memo.                       }
    {   - TFileStream Object Creating is placed outside try...finally    }
    {     block to avoid Access Violation Exception if the object can not}
    {     be created.                                                    }
    {                                                                    }
    {   Older versions fixed several bugs, see Readme.txt for history.   }
    { ****************************************************************** }


    //  This unit is freeware, but under copyrights that remain mine for my
    //  parts of the code, and original writters for their parts of the code.
    //  This is mainly the case about "variant open array parameters"
    //  copying routines, that come from Rudy Velthuis' pages on the web at :
    //  http://rvelthuis.bei.t-online.de/index.htm

    //  This unit can be freely used in any application, freeware, shareware
    //  or commercial. However, I would apreciate your sending me an email if
    //  you decide to use it. Of course, you use it under your own and single
    //  responsability. Neither me, nor contributors, could be held responsible
    //  for any problem resulting from the use of this unit.  ;-)

    //  It can also be freely distributed, provided this licence and the copyright
    //  notice remains within it unchanged, and its help file is distributed
    //  with it too.

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses Classes,
{$IFDEF FPC}
     LCLIntf,
{$ELSE}
     Windows,
{$ENDIF}
     Messages,
     SysUtils,
     StrUtils,
     Math,
     Forms,        // needed by "application.XXX"
     Registry,     // SysInfo, Timing
     StdCtrls;     // needed for visual feedback;
// --- Rolf
//     ExtCtrls;     // need for LogFileTimer - rbecker


const
//Windows messaging sub system:
  UM_HLSOON_USED    = 0;                                       // Change this, to slide HotLog's UM values if needed
  //parser
  UM_HLSERVICEMSG   = WM_USER + UM_HLSOON_USED + 10;           // Parser wake-up message,
  UM_HLACTIONMSG    = WM_USER + UM_HLSOON_USED + 11;           // Parser, line(s) to add.
  //writer & feedBack
  UM_HLSIMPLESTRING = WM_USER + UM_HLSOON_USED + 12;           // Writer thread; decides what to...
  UM_HLSTRINGLIST   = WM_USER + UM_HLSOON_USED + 13;           // ...work with (String or StringList)
//UM_STARTNEWLOG    = WM_USER + UM_HLSOON_USED + 14;           // Rolf // writer thread, start a new log file - rbecker

//messages for Parser : wParam  ("job to do")
  HL_LINENUMCHANGED = 0;
  HL_SIMPLESTRING   = 1;
  HL_PARSEEXCEPTION = 2;
  HL_JUMPLINES      = 3;
  HL_PARSELINE      = 4;
  HL_PARSELIST      = 5;
  HL_PARSEARRAY     = 6;
  HL_HEAPMONITOR    = 7;
  HL_RAMMONITOR     = 8;
  HL_DSKMONITOR     = 9;
  HL_RESETHEAPTABLE = 10;
  HL_RESETRAMTABLE  = 11;
  HL_RESETRULER     = 12;

// Parser tags
  HLT_AND      = 38;       HLT_PAD      = 42;       HLT_AT       = 64;
  HLT_RAM      = 16799;    HLT_HEAP     = 38281;    HLT_DISK     = 38039;
  HLT_RAM1     = 28319;    HLT_HEAP1    = 38326;    HLT_DISK1    = 38084;
  HLT_RAM2     = 28364;    HLT_HEAP2    = 49846;    HLT_DISK2    = 49604;
  HLT_RAM3     = 39884;    HLT_HEAP3    = 49891;    HLT_DISK3    = 49649;
  HLT_CPUI     = 39320;    HLT_OSVI     = 40101;    HLT_MEM      = 17818;
  HLT_TIMERS   = 57843;    HLT_DHG      = 18571;    HLT_DTE      = 21641;
  HLT_GTC      = 21642;    HLT_HMS      = 19867;    HLT_LNUM     = 39841;
  HLT_NOW      = 20389;    HLT_CRLF     = 47;       HLT_LNUMOFF  = 57910;
  HLT_APP_NAME = 79404;    HLT_APP_PATH = 80181;    HLT_APP_LFN  = 63019;
  HLT_APP_VER  = 62777;    HLT_RULER    = 39664;    HLT_RULER1   = 50672;
  HLT_APP_PRM  = 66094;    APP_PRM_LN   = 77614;


//other const
  MB = 1024*1024;                                     // Bytes conversion to Megs.
  HL_MAX_INT = High(integer);
  FROM_LINE  : boolean = true;
  FROM_LIST  : boolean = false;

(* begin rbecker *)
// timer delays
  OneSecond  = 1000; //in milleseconds
  OneMinute  = OneSecond * 60;
  OneHour    = OneMinute * 60;
  OneHalfDay = OneHour * 12;
  OneDay     = OneHalfDay * 2;

// log file sizes - approximate
  OneKilobyte = 1024;
  OneMegabyte = OneKilobyte * 1024;                   //probably the most reasonable size
  TenMegabyte = OneMegabyte * 10;
(* end rbecker *)

//VarRec -> String
  Bool2Str :      array[boolean] of string = ('false', 'true');

// --- Rolf
{$IFDEF FPC}
  vTypeDesc :     array[0..18] of string = (
    'vtInteger',       'vtBoolean',       'vtChar',          'vtExtended',
    'vtString',        'vtPointer',       'vtPChar',         'vtObject',
    'vtClass',         'vtWideChar',      'vtPWideChar',     'vtAnsiString',
    'vtCurrency',      'vtVariant',       'vtInterface',     'vtWideString',
    'vtInt64',         'vtQWord',         'vtUnicodeString');
  vTypeAsSring :  array[0..19] of string = (
    'LongInt',         'Boolean',         'Char',            'Extended',
    'ShortString',     'Pointer',         'PChar',           'TObject',
    'TClass',          'WideChar',        'PWideChar',       'AnsiString',
    'Currency',        'Variant',         'Interface',       'WideString',
    'int64',           'QWord',           'UnicodeString',   '#HLType');
{$ELSE}
  vTypeDesc :     array[0..17] of string = (
    'vtInteger',       'vtBoolean',       'vtChar',
    'vtExtended',      'vtString',        'vtPointer',       'vtPChar',
    'vtObject',        'vtClass',         'vtWideChar',      'vtPWideChar',
    'vtAnsiString',    'vtCurrency',      'vtVariant',       'vtInterface',
    'vtWideString',    'vtInt64',         'vtUnicodeString');
  vTypeAsSring :  array[0..18] of string = (
    'Integer',         'Boolean',         'AnsiChar',
    'Extended',        'ShortString',     'Pointer',         'AnsiChar',
    'TObject',         'TClass',          'WideChar',        'WideChar',
    'AnsiString',      'Currency',        'Variant',         'Interface',
    'WideString',      'int64',           'UnicodeString',   '#HLType');
{$ENDIF}

  TimeScale2Str:  array[0..3] of string = ('s.', 'ms', 'µs', 'ns');
  TimerUnit2Str:  array[0..3] of string = ( 's.', 'ms.', '"units"', 'cycles' );

type

 {Types généraux}

  // --- Rolf
  TVarRecValue   = record
    vType: integer;
    Value: string;
  end;
//TConstArray    = array of TVarRec;                  // "Variant open array parameters"
  TConstArray    = array of TVarRecValue;

  TMonitorKind   = (mkHeap, mkRam);
  TVarRecStyle   = (vsNone, vsBasic, vsExtended);     // Wanted output format for VarRec values

  TInt4Array     = array[0..3] of integer;

  TParseItemKind = (ikStr, ikTag);                    // Kind of a part of a string ("TParseItem"), before parsing

  PParseItem     = ^TParseItem;
  TParseItem     = record                             // item (to) parse(d)
    kind:     TParseItemKind;                         // (ikTag | ikStr);
    sValue:   string;                                 // Tag or string original | parsed value ;
    iValue:   cardinal;                               // Tag value, after convertion to a unique integer identifier
    isSingle: boolean;                                // Filters inLine / standAlone tags
  end;

//Heap and ram monitoring
// --- Rolf
  PHeapMRec      = ^THeapMRec;
  THeapMRec      = record
    Extra:                                 string;
    case integer of
      // Heap
{$IFDEF FPC}
      0: (MaxHeapSize:                     PtrUInt;   // Maximium allowed size for the heap, in bytes
          MaxHeapUsed:                     PtrUInt;   // Maximum used size for the heap, in bytes
          CurrHeapSize:                    PtrUInt;   // Current heap size, in bytes
          CurrHeapUsed:                    PtrUInt;   // Currently used heap size, in bytes
          CurrHeapFree:                    PtrUInt);  // Currently free memory on heap, in bytes
{$ELSE}
      0: (AllocatedSmallBlockCount:        cardinal;
          TotalAllocatedSmallBlockSize:    NativeUInt;
          ReservedSmallBlockAddressSpace:  NativeUInt;
          AllocatedMediumBlockCount:       cardinal;
          TotalAllocatedMediumBlockSize:   NativeUInt;
          ReservedMediumBlockAddressSpace: NativeUInt;
          AllocatedLargeBlockCount:        cardinal;
          TotalAllocatedLargeBlockSize:    NativeUInt;
          ReservedLargeBlockAddressSpace:  NativeUInt);
{$ENDIF}
      // Ram
      1: (MemoryLoad:                      cardinal;
          TotalPhysical:                   UInt64;
          AvailPhysical:                   UInt64;
          TotalPageFile:                   UInt64;
          AvailPageFile:                   UInt64;
          TotalVirtual:                    UInt64;
          AvailVirtual:                    UInt64);
    end;

  THeapMonitor = class (TObject)
  private
    FKindOfVals: TMonitorKind;
    heapRecords: array of THeapMRec;
    // --- Rolf
    procedure    AddRec         (rec: THeapMRec);
    procedure    ResetMemTable;
    function     GetFirstEntry: THeapMRec;
  public
    constructor  Create         (asKind: TMonitorKind);
    destructor   Destroy;       override;
  end;

//Timer
  TQTimeScale   = (tsSeconds, tsMilliSec, tsMicroSec, tsNanosSec);
  TQTimerAction = (taStart,taStop,taGlobalStop);
  TQTimerWanted = (twStart,twStop,twDelta);
  TQTimerKind   = (tkHMS,tkGTC,tkQPC,tkRDT);

  TQTimerEntry  = record
    tStart,
    tStop:  int64;
    kind:   TQTimerKind;
  end;


  TQTimer = class (TObject)
  private
    FEntry:           array of TQTimerEntry;
    FIsFinalized:     boolean;
    FRegFrq,                                          // Registry stored frequency  * 1000000
    FQpcFrq,                                          // Returned value of QueryPerformanceFrequency
    FRDTOverhead,                                     // direct ASM call overhead
    FQpcOverhead:     int64;                          // direct API call overhead

  //Internals
    function  GetCount:             integer;
    function  GetStartAsInt         (ix: integer): int64;
    function  GetStopAsInt          (ix: integer): int64;
    function  GetDeltaAsInt         (ix: integer): int64;
    function  GetStartAsStr         (ix: integer): string;
    function  GetStopAsStr          (ix: integer): string;
    function  GetDeltaAsStr         (ix: integer): string;
    function  GetFormatedDeltaAsExt (ix: integer): extended;
    function  GetFormatedDeltaAsStr (ix: integer): string;
    function  GetEntry              (ix: integer; wanted: TQTimerWanted): int64;
    function  Overhead              (ix: integer): integer;
    function  GetTimeMeasureAs      (timeScale: TQTimeScale; measure: int64; counter: TQTimerKind): real;
    function  GetOptimalMeasure     (measure: int64; counter: TQTimerKind; var tScale: TQTimeScale): real;
    procedure Store                 (value: int64; inOrOut: TQTimerAction; timerKind: TQTimerKind);
    function  Int64ToTime           (value: int64): string;
    procedure InitTimerEnvir;

  public
    fmtOutput:        string;                         // Used for the Format() function ; Defaults to '%3.9n'
    timeUnitsWanted:  set of TQTimeScale;
    removeOverHead,
    deltaShowNatives,                                 // outputs deltas in native units;
    deltaShowOptimal: boolean;                        // outputs deltas in the most "readable" unit possible;

    constructor Create;
    destructor  Destroy;            override;

    function    HMS                 (startOrStop: TQTimerAction = taStart): TDateTime;
    function    GTC                 (startOrStop: TQTimerAction = taStart): integer;
    function    QPC                 (startOrStop: TQTimerAction = taStart): int64;
    function    RDT                 (startOrStop: TQTimerAction = taStart): int64;
    procedure   GlobalStop;
    procedure   Reset;

    property    isFinalized:             boolean  read FIsFinalized;
    property    RegistryFreq:            int64    read FRegFrq;
    property    QueryPerfFreq:           int64    read FQpcFrq;
    property    ReaDTSCOverhead:         int64    read FRDTOverhead;
    property    QueryPerfOverhead:       int64    read FQpcOverhead;

    property    Count:                   integer  read GetCount;
    property    iStart    [ix: integer]: int64    read GetStartAsInt;
    property    iStop     [ix: integer]: int64    read GetStopAsInt;
    property    iDelta    [ix: integer]: int64    read GetDeltaAsInt;

    property    sStart    [ix: integer]: string   read GetStartAsStr;
    property    sStop     [ix: integer]: string   read GetStopAsStr;
    property    sDelta    [ix: integer]: string   read GetDeltaAsStr;

    property    iDeltaFmt [ix: integer]: extended read GetFormatedDeltaAsExt;
    property    sDeltaFmt [ix: integer]: string   read GetFormatedDeltaAsStr;
  end;
  

//log file name management
  PFlName = ^FlName;
  FlName  = record                     // Will temporary store existing files names...
    fullName: TFileName;               // ...descriptions in order to manage generations.
    ext3:     string;
    isGdg:    boolean;
  end;

  THLFileDef = class (TObject)         // Log file name and acces mode definition
  private
  //fields
    FPath,                             // Log path
    FDdn,                              // log name, without path nor ext
    FExt,                              // log extention. '.log' by default if non Gdg, Gdg count otherwise
    FLFN:              string;         // Long file name
    FGdgMax:           word;           // max Number of generations to keep. 0 < Gdg < 999. //0 <=> no Gdg
    FSafeGdgMax:       word;           // same, but for safe log file names - rbecker
    FDoAppend:         boolean;        // Shall log be emptied or not ? , if exists. Ignored if FGdgMax > 0
    FBuild:            boolean;        // "BuildFileName" function soon called or not;
// --- Rolf: not used
//  FUseSizeLimit:     boolean;        // close log file and start a new one when size limit exceeded? - rbecker
    FLogFileMaxSize:   integer;        // maximum size of the log file, used for continuous operation  - rbecker
    FUseSafeFileNames: boolean;        // use a unique name for each log file rather than generational - rbecker
// --- Rolf
    FSafeIndex:        cardinal;       // Additional index to make safe filenames unique
    FSafePrefix:       string;         // Use ddname property for safe file names too

  //procs & functions
    procedure   SetPath        (Value: string);
    procedure   SetExt         (Value: string);
    procedure   SetFileName    (lfn: TFileName);
    procedure   SetDoAppend    (Value: boolean);
    procedure   SetGdgMax      (Value: word);
    function    BuildFileName: TFileName;
    function    GetGdg         (sL: TList): string;
    function    GetFileName:   TFileName;
    function    GetOpenMode:   word;
    function    FileInUse      (f: string): boolean;

  public
    constructor Create;
    destructor  Destroy; override;

    property  ddname:           string    read FDdn              write FDdn;
    property  path:             string    read FPath             write SetPath;
    property  ext:              string    read FExt              write SetExt;
    property  fileName:         TFileName read GetFileName       write SetFileName;
    property  append:           boolean   read FDoAppend         write SetDoAppend;
    property  GdgMax:           word      read FGdgMax           write SetGdgMax;
    property  SafeGdgMax:       word      read FSafeGdgMax       write FSafeGdgMax;           //rbecker
    property  OpMode:           word      read GetOpenMode;
// --- Rolf: not used
//  property  UseFileSizeLimit: boolean   read FUseSizeLimit     write FUseSizeLimit;         //rbecker
    property  LogFileMaxSize:   integer   read FLogFileMaxSize   write FLogFileMaxSize;       //rbecker
    property  UseSafeFilenames: boolean   read FUseSafeFileNames write FUseSafeFileNames;     //rbecker

    procedure BuildSafeFileName;                                                              //rbecker
  end;{class THLFileDef}


 {Internal messages, used for communication between threads}

  THLStringMsg = class (TObject)                 // basic string
  private
    FHlMsg: string;
    procedure   Post     (toThread: THandle; kindOfString: integer = HL_SIMPLESTRING);
  public
    constructor Create   (s: string);
    destructor  Destroy; override;
  end;


  THLConstArrayMsg = class (TObject)             // Arrays of const
    FConstArray:  TConstArray;
    FOutputStyle: TVarRecStyle;                  // basic or extended output
    procedure   Post     (toThread: THandle);
  public
    constructor Create   (outputStyle: TVarRecStyle);
    destructor  Destroy; override;
  end;


  THLErrMsg = class (TObject)                    // Exceptions ans errors to be parsed
  private
    FEMsg:    string;
    FlastErr: integer;
    Ffull:    boolean;
    FFunc:    string;
    Fargs:    TConstArray;
    procedure   Post     (toThread: THandle);
  public
    constructor Create;
    destructor  Destroy; override;
  end;


  // --- Rolf
  PHLHeapMonMsg = ^THLHeapMonMsg;
  THLHeapMonMsg = class (TObject)                // Heap & Ram values monitoring message
  private
    hmr: THeapMRec;
    procedure   Post     (toThread: THandle; RamOrHeap: integer; dirOutput: boolean = false);
  public
    // Ram
    constructor Create   (ML: cardinal; TP,AP,TPF,APF,TV,AV: UInt64; ex: string); overload;
    // Heap
{$IFDEF FPC}
    constructor Create   (MHS, MHU,CHS,CHU,CHF: PtrUInt; ex: string); overload;
{$ELSE}
    constructor Create   (ASBC: cardinal; TASBS, RSBAS: NativeUInt;
                          AMBC: cardinal; TAMBS, RMBAS: NativeUInt;
                          ALBC: cardinal; TALBS, RLBAS: NativeUInt;
                          ex: string); overload;
{$ENDIF}
    destructor  Destroy; override;
  end;


 {Thread : Parser}

  THotLogParser = class(TThread)
  private
    mxHLLineNum:      THandle;
    FStarted:         boolean;
    FWriterTID:       THandle;                   // Writer ThreadId, needed to post messages to.
  //Lines counting
    FpvLineCount:     cardinal;
    FshowLineNum:     boolean;
    FpvLnNumLen:      word;
    FShowLNTemp:      boolean;                   // used for temporary override;
    FpvMaxValue:      cardinal;                  // ;-)
    FpvLnAlign:       TAlignment;                // line numbers output formatting;
  //Ruler
    fRulerLength:     integer;
    fRulerDots,
    fRulerNums:       string;
    fRulerBuild:      boolean;
  //Error displaying
    FErrorCaption:    string;
    FErrViewStyle:    TVarRecStyle;              // (vsBasic, vsExtended)
    FErrJumpLines,
    FErrIsSurrounded: boolean;
    FErrSurroundChar,
    FErrSurroundLine,
    FErrWideSurround: string;
  //Timers
    FsRegSpeed:       string;                    // Proc speed, from registry

  protected
    FLnShowNum:       boolean;
    FLnNumLen:        integer;
    FLnAlign:         TAlignment;

  private
    HMonitor:         THeapMonitor;
    RMonitor:         THeapMonitor;

  //Line Numbers management
    procedure   UpdateLineNum;
    function    GetLineNum:                string;
    function    AddLineNum                 (line: string): string; overload;
    procedure   AddLineNum                 (var lst: TStringList); overload;

  //Errors display setting
    procedure   SetFErrCaption             (Value: string);
    procedure   SetFErrSurroundChar        (Value: string);

  //VarRec handling
  // --- Rolf
    function    VarRecToStr                (vrv: TVarRecValue): string;
    function    ConstArrayToString         (const Elements: TConstArray;
                                            style: TVarRecStyle = vsBasic): string;
    function    ConstArrayToTSList         (const Elements: TConstArray): TStringList;
    procedure   FinalizeConstArray         (var Arr: TConstArray);
//  procedure   FinalizeVarRec             (var Item: TVarRec);
    function    GetBasicValue              (s: string): string;
//  function    GetOriginalValue           (s: string): string;

  //parsing
    function    ParseLine                  (source: string): TStringList;
  // --- Rolf
    function    ParseList                  (StringList: TStringList): TStringList;
//    function    ParseList                  (source: NativeInt): TStringList;
    function    ParseArray                 (HLConstArrayMsg: THLConstArrayMsg): TStringList;
//    function    ParseArray                 (source: NativeInt): TStringList;
    function    LineToList                 (line: string): TList;
    function    ExtractItem                (var source: string; var itm: PParseItem): boolean;
    procedure   PrepareParsing             (var lst: Tlist);
    procedure   TranslateTags              (var lst: Tlist; var omitLNum: boolean);
    function    PadString                  (src,tag: string; posPtr: integer): string;
    procedure   FreeParsedList             (var l: TList);
    function    GetRegValue                (root: HKey; key,wanted: string): string;
    // --- Rolf
    function    GetParams:                 string; overload;                                       // single line
    function    GetParams                  (var lst: TList): string; overload;                     // multi line
    function    GetVersionAsText:          string;
    function    CurrentNumVer:             TInt4Array;
    procedure   GetRuler                   (var lst: TList; showNums: boolean = true);
    procedure   ResetRuler                 (value: integer);
    function    GetHeap                    (PHeapRec: PHeapMRec; outFmt: integer): TStringList;    // --- Rolf
    function    GetRam                     (PHeapRec: PHeapMRec; outFmt: integer): TStringList;    // --- Rolf
    function    GetDisk                    (outFmt: integer): TStringList;
    procedure   GetMemoryStatus            (var lst: TList);
    procedure   GetCPUInfoAsPitem          (var lst: Tlist);
    procedure   GetOSVIAsPitem             (var lst: Tlist);
    procedure   GetRamOrHeapAsPitem        (PHeapRec: PHeapMRec; RamOrHeap, outFmt,                // --- Rolf
                                            ptr: integer; var lst: Tlist);
    procedure   GetTimerResolutionsAsPitem (var lst: Tlist);
    function    ParseException             (HLErrMsg: THLErrMsg): TstringList;

    property  ErrorCaption: string read FErrorCaption    write SetFErrCaption;
    property  SurroundCh:   string read FErrSurroundChar write SetFErrSurroundChar;

  public
// --- Rolf
    constructor Create;
    destructor  Destroy;                   override;
    procedure   Execute;                   override;
    procedure   Terminate;                                                                         // --- Rolf
  end;


 {Thread : Writer}

//  Writer thread will have to partly manage the feedback, in order to :
//  -1- Choose whether freeing memory of received lines, or
//  -2- Send them to the FeedBackThread, which thus will be in charge of
//      freing the memory.

  THotLogWriter = class(TThread)
  private
    FExecuting:  boolean;                        // Sergueï
    FStarted,
    FDoFeedBack: boolean;
    FVisualTID:  THandle;                        // Feedbacker ThreadId

  public
    hlFileDef:   THLFileDef;                     // The log file name to use
// --- Rolf
    constructor Create;
    destructor  Destroy; override;
    procedure   Execute; override;
    procedure   Terminate;                       // --- Rolf
  end;


 {Thread : Visual feedback}

  THotLogVisual = class(TThread)
  private
    mxHLFeedBack: THandle;                       // Feedback suspention control.
    FfbMemo:      TMemo;
    FfbMemoLimit: cardinal;                      // Oleg;
    FDoFeedBack,
    FDoScroll:    boolean;
// --- Rolf
    FStarted:     boolean;
    s:            string;                        // -> DisplayLine
    sl:           TStringList;                   // -> DisplayList
{$ifndef USE_SLOW_METHOD}
    procedure   ClearExtraLines;
    procedure   DoTheScroll;
{$endif}
  public
    constructor Create;
    destructor  Destroy;     override;
    procedure   Execute;     override;
    procedure   Terminate;                       // --- Rolf
    procedure   RemoveCRLF   (fromWhat: boolean);
    procedure   DisplayLine;                     // Synchronised procs. Execute...
    procedure   DisplayList;                     // ... in main thread
  end;


 {HotLog public interface}

  THotLog = class (TObject)
  private
    FStarted: boolean;
    hlParser: THotLogParser;                     // parser thread
    hlVisual: THotLogVisual;                     // feedBack manager thread

  //VarRec handling
  // --- Rolf
    function    CreateConstArray        (const Elements: array of const): TConstArray;
    function    CopyVarRec              (const Item: TVarRec): TVarRecValue;

  public
    hlWriter: THotLogWriter;                     // made public, to allow direct acces to the log file name definition properties.
    header,
    footer:   TStringList;
// --- Rolf
//  LogFileTimer: TTimer;                        //rbecker

    constructor Create;
    destructor  Destroy;                override;

  //Settings
    procedure   DisplayFeedBackInto     (aMemo: TMemo);
    function    StopVisualFeedBack:     boolean;
    procedure   ScrollMemo              (doScroll: boolean);
    procedure   SetMemoLimit            (value: cardinal); // Oleg
    procedure   SetErrorCaption         (value: string; surroundChar: string;
                                         jumpLines: boolean = true);
    procedure   SetErrorViewStyle       (style: TVarRecStyle);
    procedure   SetRulerLength          (newLength: integer);
    function    ModifyLineNumbering     (doShow: boolean; alignment: TAlignment = taRightJustify;
                                         size: integer = 3): boolean;

  //ram & heap monitoring
    procedure   ResetHeapTable;
    function    HeapMonitor             (hmExtra: string = '';
                                         directOutput: boolean = false): THeapMRec;
    procedure   ResetRamTable;
    function    RamMonitor              (hmExtra: string = '';
                                         directOutput: boolean = false): THeapMRec;
  //Logging
    procedure   StartLogging;
    procedure   JumpLine                (LinesToJump: integer = 1);
    // Parsing involved
    procedure   Add                     (aString: string); overload;
    procedure   Add                     (aStringList: TStringList); overload;
    procedure   Add                     (style: TVarRecStyle; aConstArray: array of const); overload;
    // -No- parsing
    procedure   AddStr                  (aString: string);
    procedure   AddException            (ex: Exception; err: integer = 0;
                                         freeAfterPost: boolean = false); overload;
    procedure   AddException            (ex: Exception; func: string; args : array of const;
                                         err: integer = 0; freeAfterPost: boolean = false); overload;
    procedure   AddError                (err: integer); overload;
    procedure   AddError                (err: integer; func: string; args: array of const); overload;

// --- Rolf
//  procedure   SetLogFileTimerInterval (Interval: cardinal);   //rbecker
//  procedure   OnLogFileTimer          (Sender: TObject);      //rbecker

// --- Rolf
    property    Started:                boolean read FStarted;
  end;

{Thread:Main(VCL)}
function  CompareGdgNames (p1, p2: pointer): integer;
function  RDTSC:          int64; assembler;

var
// hLog itself, alone but proud...
  hLog: THotLog;

implementation

uses
{$IFDEF FPC}
  Windows, LCLType, Controls, LazUTF8,
  JwaWinBase, JwaWinNt,
  // --- Rolf
  LazUTF8Classes, LazFileUtils;
{$ELSE}
  Controls;
{$ENDIF}


{-------------------------------------------------------------------------------
   Map UTF8 string functions to simple ones for Delphi
-------------------------------------------------------------------------------}
{$IFNDEF FPC}
function UTF8Copy (const AString: string; const AFrom, ALength: integer): string;
begin
  Result:=copy(AString,AFrom,ALength);
end;

function UTF8Pos (SubStr, Source: string): integer;
begin
  Result:=Pos(SubStr,Source);
end;

function UTF8LeftStr (const AString: string; const ACount: integer): string;
begin
  Result:=LeftStr(AString,ACount);
end;

function UTF8RightStr (const AString: string; const ACount: integer): string;
begin
  Result:=RightStr(AString,ACount);
end;

function UTF8Length (const AString: string): integer;
begin
  Result:=Length(AString);
end;

function UTF8Encode (const AString: string): string;
begin
  Result:=AString;
end;

function UTF8Decode (const AString: string): string;
begin
  Result:=AString;
end;

function UTF8StringOfChar (AChar: string; ACount: integer): string;
begin
  Result:='';
  if AChar='' then
    exit;
  Result:=StringOfChar(AChar[1],ACount);
end;

funtion FileExistsUTF8 (const FileName: string): boolean;
begin
  Result:=SysUtils.FileExists(FileName);
end;

function FindFirstUTF8 (const Path: string; Attr: Integer; var F: TSearchRec): Integer;
begin
  Result:=SysUtils.FindFirst(Path,Attr,F);
end;

function FindNextUTF8 (var F: TSearchRec): Integer;
begin
  Result:=SysUtils.FindNext(F);
end;

procedure FindCloseUTF8 (var F: TSearchRec);
begin
  SysUtils.FindClose(F);
end;

function DeleteFileUTF8 (const FileName: string): Boolean;
begin
  Result:=SysUtils.DeleteFile(FileName);
end;

function SysErrorMessageUTF8 (ErrorCode: Cardinal): string;
begin
  Result:=SysUtils.SysErrorMessage(ErrorCode);
end;

type
  TFileStreamUTF8 = class(TFileStream);

{$ELSE}

function UnicodeToUtf8 (Dest: PChar; MaxDestBytes: SizeUInt; Source: PUnicodeChar; SourceChars: SizeUInt): SizeUInt;
var
  i,j: SizeUInt;
  lw:  longword;
begin
  Result:=0;
  if Source=nil then
    exit;
  i:=0;
  j:=0;
  if Assigned(Dest) then
  begin
    while (i<SourceChars) and (j<MaxDestBytes) do
    begin
      lw:=ord(Source[i]);
      case lw of
        $0000..$007F: begin
                        Dest[j]:=Char(lw);
                        inc(j);
                      end;
        $0080..$07FF: begin
                        if j+1>=MaxDestBytes then
                          Break;
                        Dest[j]:=Char($C0 or (lw shr 6));
                        Dest[j+1]:=Char($80 or (lw and $3F));
                        Inc(j,2);
                      end;
        $0800..$D7FF,
        $E000..$FFFF: begin
                        if j+2>=MaxDestBytes then
                          Break;
                        Dest[j]:=Char($e0 or (lw shr 12));
                        Dest[j+1]:=Char($80 or ((lw shr 6) and $3F));
                        Dest[j+2]:=Char($80 or (lw and $3F));
                        Inc(j,3);
                      end;
        $D800..$DBFF: begin                 // High Surrogates
                        if j+3>=MaxDestBytes then
                          Break;
                        if (i+1<SourceChars) and
                           (word(Source[i+1])>=$DC00) and
                           (word(Source[i+1])<=$DFFF) then
                        begin               // $D7C0 is ($D800 - ($10000 shr 10))
                          lw:=(longword(lw-$D7C0) shl 10) + (ord(source[i+1]) xor $DC00);
                          Dest[j]:=Char($F0 or (lw shr 18));
                          Dest[j+1]:=Char($80 or ((lw shr 12) and $3F));
                          Dest[j+2]:=char($80 or ((lw shr 6) and $3F));
                          Dest[j+3]:=char($80 or (lw and $3F));
                          Inc(j,4);
                          Inc(i);
                        end;
                      end;
      end;
      Inc(i);
    end;

    if j>MaxDestBytes-1 then
      j:=MaxDestBytes-1;

    Dest[j]:=#0;
  end
  else
  begin
    while i<SourceChars do
    begin
      case word(Source[i]) of
        $0000..$007F: Inc(j);
        $0080..$07FF: Inc(j,2);
        $0800..$D7FF,
        $E000..$FFFF: Inc(j,3);
        $D800..$DBFF: begin
                        if (i+1<SourceChars) and
                           (word(Source[i+1])>=$DC00) and
                           (word(Source[i+1])<=$DFFF) then
                        begin
                          Inc(j,4);
                          Inc(i);
                        end;
                      end;
      end;
      Inc(i);
    end;
  end;
  Result:=j+1;
end;

function UTF8Encode (const s: UnicodeString; Count: integer = -1): RawByteString; overload;
var
  i,m: SizeInt;
  u8:  UTF8String;
begin
  Result:='';
  if s='' then
    exit;
  if Count=0 then
    exit;
  if Count>0 then
    m:=Count
  else
    m:=min(Length(s),StrLen(pWideChar(s)));
  if m<=0 then
    exit;
  SetLength(u8,m*3);
  i:=UnicodeToUTF8(pChar(u8),Length(u8)+1,PUnicodeChar(s),m);
  if i>0 then
  begin
    SetLength(u8,i-1);
    Result:=u8;
  end;
end;

function UTF8Encode (const s: WideString; Count: integer = -1): RawByteString; overload;
var
  i,m: SizeInt;
  u8:  UTF8String;
begin
  Result:='';
  if s='' then
    exit;
  if Count=0 then
    exit;
  if Count>0 then
    m:=Count
  else
    m:=min(Length(s),StrLen(pWideChar(s)));
  if m<=0 then
    exit;
  SetLength(u8,m*3);
  i:=UnicodeToUTF8(pChar(u8),Length(u8)+1,PWideChar(s),m);
  if i>0 then
  begin
    SetLength(u8,i-1);
    Result:=u8;
  end;
end;
{$ENDIF}

procedure DbgSendMsg (const Message: string); overload;
begin
  OutputDebugStringW(pWideChar(UTF8Decode(Message)));
end;

procedure DbgSendMsg (const Message: string; const Args: array of const); overload;
begin
  DbgSendMsg(Format(Message,Args));
end;

{$IFDEF FPC}
function QueryPerformanceCounter(var lpPerformanceCount: Int64): LongBool; stdcall;
  external kernel32 name 'QueryPerformanceCounter';
function QueryPerformanceFrequency(var lpFrequency: TLargeInteger): BOOL; stdcall;
external kernel32 name 'QueryPerformanceFrequency';
{$ENDIF}


    {----------------------------------------------------------}
    {---        Threads:Main(VCL) : File definition         ---}
    {----------------------------------------------------------}
    { Will be used to acces the log file. File name is set ;   }
    { If generations have to be managed, -> rename (and needed }
    { deletions) are made.                                     }
    {----------------------------------------------------------}


constructor THLFileDef.Create;
begin
  inherited;
  Path             := ExtractFilePath(Application.ExeName);
  FDdn             := ChangeFileExt(ExtractFileName(Application.ExeName),'');
  FExt             := '.log';
  FGdgMax          := 0;                             // as long as an ext is provided, no Gdg is managed
  FDoAppend        := false;                         // if exists, will be overridden
  FBuild           := false;
  (* begin rbecker *)
  SafeGdgMax       := 0;
// --- Rolf
//UseFileSizeLimit := false;
  LogFileMaxSize   := 0;
  UseSafeFilenames := false;
  (* end rbecker *)
// --- Rolf
  FSafeIndex       := 0;
  FSafePrefix      := '*';                            // Invalid value
end;

destructor THLFileDef.Destroy;
begin
  inherited;
end;

function THLFileDef.GetFileName:TFileName;
begin
  If (FGdgMax = 0) Or (Self.FBuild)
     then result := FPath + FDdn + FExt
     else result := BuildFileName;
end;

procedure THLFileDef.SetFileName(lfn:TFileName);
//  Sets the full log name
begin
  If lfn <> '' then
  begin
    FPath := ExtractFilePath(lfn);                    // if not provided...
    If FPath <> '' then SetPath(FPath);               // ... then current path is kept
    FDdn  := ChangeFileExt( ExtractFileName(lfn),''); // Vittorio Loschi
    If FDdn = '' then FDdn := 'LogRecord';            // '' is forbidden;
    FExt  := ExtractFileExt(lfn);
    If FExt <> '' then FGdgMax := 0;                  // If provided, THLFileDef is no longer supposed to manage Gdg
  end
end;


procedure THLFileDef.SetPath (Value: string);
begin
  If Value <> '' then                                 // Otherwise unchanged.
  begin
    if Value[length(Value)]<>'\' then
      Value:=Value+'\';
    self.FPath:=Value;
  end;
end;

procedure THLFileDef.SetExt (Value: string);
begin
  If Value[1]<>'.' then
    Value:='.'+Value;
  Self.FExt:=Value;                                   // empty allowed
  Self.GdgMax:=0;                                     // Gdg no longer managed
end;

procedure THLFileDef.SetDoAppend (Value: boolean);
begin
  Self.FDoAppend:=Value;
  If Value then
    Self.FGdgMax := 0;                                // Gdg no longer managed
end;

procedure THLFileDef.SetGdgMax(Value:Word);
begin
  If Value > 999 then Self.FGdgMax := 999
                 else Self.FGdgMax := Value;
  If Value > 0 then Self.FDoAppend := false;
end;

function THLFileDef.GetOpenMode:Word;
// v 1.1
begin
  If Self.FDoAppend then
  begin
    If FileExists(Self.fileName)
       then Result := fmOpenReadWrite OR fmShareDenyWrite
       else Result := fmCreate OR fmShareDenyWrite;
  end
  else  Result := fmCreate OR fmShareDenyWrite;
end;

function THLFileDef.BuildFileName:TFileName;
// Called only if Gdg extention has to be managed.
// Returns the long file name of the new log.
var sRec:TSearchRec;
    lstFlNames: TList;
    sTmp:string;
    aGdgRec: PFlName;
begin
  // First populate a list of potential "on disk"
  // log files (extention temporary ingnored)
  lstFlNames := TList.Create;                                 // allways created, to avoid compiler warning
  try {finally}
  try {except}
    sTmp   := self.path + self.ddname + '.*';
    // --- Rolf
    If FindFirstUTF8(sTmp, faAnyfile, sRec) = 0 then
    begin
      new(aGdgRec);
      aGdgRec^.fullName := self.path + sRec.Name;
      aGdgRec^.isGdg := false;                                // will be tested later, if needed
      lstFlNames.Add(aGdgRec);
      // --- Rolf
      while ( FindNextUTF8(sRec) = 0 ) Do
      begin
        new(aGdgRec);
        aGdgRec^.fullName := self.path + sRec.Name;
        aGdgRec^.isGdg := false;
        lstFlNames.Add(aGdgRec);
      end;
      // --- Rolf
      FindCloseUTF8(sRec);

      Result := self.path + self.ddname + GetGdg(lstFlNames); // GetGdg wil return the new generation number
    end else Result := path + ddname + '.001';                // No fileName actually looks like the one wanted
  except  Result := self.ddname + '.~og'; end;

  finally                                                     // Cleanup list items then the list
    try
    Self.FBuild := true;
    Self.FPath  := ExtractFilePath(result);
    Self.FDdn   := ChangeFileExt(ExtractFileName(result),'');
    Self.FExt   := ExtractFileExt(result);
    Self.FLFN   := Result;
    while lstFlNames.Count > 0 do
    begin
      If assigned(lstFlNames[0]) then
      begin
        aGdgRec := lstFlNames.Items[0];
        Dispose(aGdgRec);
      end;
      lstFlNames.Delete(0);
    end;
    lstFlNames.Free;
    except; end;
  end;
end;

function THLFileDef.GetGdg (sL: TList): string;

// Called only if at least one file still exists, with a "Gdg looking like"
// filename. Sets the generation number of a Gdg file.
// (".001" < Gdg < "."Gdgmax ). Lists all Gdg present with the ddn HLFileDef.ddname,
// and keeps only the GdgMax -1 most recent (highest Gdg) ones, & renames them
// (001..GdgMax-1). then returns the new Gdg extention. NOT(FileExists)  and
// NOT(InUse) newFileName are controlled too.

var
  gTmp,i:  integer;
  sTmp:    string;
  Succes:  boolean;
  aGdgRec: PFlName;
begin
  try
    for i := 0 To sL.count -1 do
    begin
      sTmp := ExtractFileExt(PFlName(sL.items[i])^.fullName);
      try
        StrToInt(sTmp);
        PFlName(sL.items[i])^.ext3  := sTmp;                              // PROGRAMMERS, if an ex raises : Keep cool !...
        PFlName(sL.items[i])^.isGdg := true;                              // ...EConvertError wonn't appear at run time
      except
        aGdgRec := PFlName(sL.items[i]);
        Dispose(aGdgRec);                                                 // not a Gdg file name...
        sL.items[i] := nil;                                               // ...then, remove it
      end;
    end;

   {pack will apply the deletions, if any happened}
    sL.Pack;                                                              // sL now contains Gdg fileNames only.
    If sL.Count = 0 then
    begin
      Result := '.001';
      Exit;
    end;

    If sL.count > 1 then
      sL.Sort({$IFDEF FPC}@{$ENDIF}CompareGdgNames);                      // let's sort them ascending
    for i:= 0 to sl.Count - self.GdgMax do
    begin
      // --- Rolf
      If SysUtils.DeleteFile(PFlName(sL.items[i])^.fullName) then
         sL.Items[i] := nil;                                              // If unable to delete... keep that generation
    end;

    sl.Pack;
    Succes := true;
    for i:= 0 to sl.Count - 1 do                                          // Now tries to rename each kept file
    begin
      sTmp := IntToStr(i+1);                                              // generation numbers, starting from "001"
      while Length(sTmp) < 3 Do sTmp := '0' + sTmp;
      sTmp := ChangeFileExt(PFlName(sL.items[i])^.fullName, '.' + sTmp);
      If NOT( RenameFile(PFlName(sL.items[i])^.fullName, sTmp) ) then
         succes := false;
    end;

    if succes then
      gTmp := sl.Count + 1
    else
    begin
      sTmp := ExtractFileExt(PFlName(sL.items[sl.Count])^.fullName);
      try
        gTmp := StrToInt(sTmp) + 1;                                       // will try to assign the new generation a number
      except
        gTmp := 999;                                                      // higher than the one soon present.
      end;
    end;

    Result := IntToStr(gTmp);
    while Length(Result) < 3 do
      Result := '0' + Result;
    Result := '.' + Result;
    sTmp := ChangeFileExt(PFlName(sL.items[0])^.fullName, result);
    If not(Succes) or FileExists(sTmp) or FileInUse(sTmp) then
      Result := '.~og';
  except
    Result := '.~og';
  end;
end;

function THLFileDEf.FileInUse (f: string): boolean;                       // NIH
var
  hF: HFILE;
begin
  Result := false;
  // --- Rolf: UTF8
  If not FileExistsUTF8(f) then
    exit;
  hF := CreateFileW(pWideChar(UTF8Decode(f)),
                    GENERIC_READ or GENERIC_WRITE,
                    0, NIL, OPEN_EXISTING,
                    FILE_ATTRIBUTE_NORMAL, 0);
  Result := (hF = INVALID_HANDLE_VALUE);
  If not Result then
    CloseHandle(hF);
end;

procedure THLFileDef.BuildSafeFileName;  //rbecker
(* extension is assumed to remain the default of "log" *)
begin
// --- Rolf
  FSafeIndex := FSafeIndex + 1;
  if FSafePrefix = '*' then                           // Set only one time
    FSafePrefix := ddname;
  if FSafePrefix = '*' then
    ddname := Format('%s %d',
      [FormatDateTime('yyyy-mm-dd hh.mm.ss', Now),FSafeIndex])  //keep it at least semi-readable
  else
    ddname := Format('%s-%s-%d',
      [FSafePrefix,FormatDateTime('yyyymmdd-hhmmss', Now),FSafeIndex]);
end;


//Non object-linked functions

function CompareGdgNames (p1, p2: pointer): integer;
//Seems that it has to be external to the class (?)
begin
  Result:=AnsiCompareText(PFlName(p1)^.ext3,PFlName(p2)^.ext3);
end;

{$IFDEF FPC}
{$ASMMODE intel}
{$ENDIF}
function RDTSC: int64; assembler;                                         // NIH...  Result in EAX and EDX (int64)
asm
  {$IFDEF FPC}
    Rdtsc
  {$ELSE}
    {$IFDEF VER130}
      DB $0F,$31                                                          // Modification DELPHI5
    {$ELSE}
      Rdtsc                                                               // <=> DB $0F,$31  (for versions below D4 ?)
    {$ENDIF}
  {$ENDIF}
end;



    {---------------------------------------------------------------------}
    {---                   Threads : Main(VCL) : hotLog                ---}
    {---------------------------------------------------------------------}
    { THotLog methodes execute in the main thread,and are kept as basic   }
    { as possible, to avoid costing time. Most of the work will be done   }
    { in the parser thread.                                               }
    { Strings aso. to logg are sent to the parser throught messages build }
    { at run time. The parser is responsible for freeing the corresponding} 
    { memory, and possibly sending messages to the "feedbacker".          }
    {---------------------------------------------------------------------}

constructor THotLog.Create;
begin
  hlParser  := THotLogParser.Create;                 // suspended, waiting for resume
  hlWriter  := THotLogWriter.Create;

  Self.header := TstringList.Create;
  With Self.Header Do
  begin
    Add('{/}{LNumOff}{*80*}');                             // <=>  Add('{LNumOff}{/}{}{*80*}');   <- empty tag
    Add('>>>> Start {App_name}  v {App_ver}{80@}{&}{dte} {hms}{&}');
    Add('{@12}From : {App_path}');
    Add('{@12}Prms : {App_prm-}{/}');
  end;

  Self.footer := TStringList.Create;
  With self.Footer Do
  begin
    Add('{LNumOff}');
    Add('<<<< Stop  {App_name}{80@}{&}{dte} {hms}{&}');
    Add('{*80*}{/}');
  end;

  (* begin rbecker *)
// --- Rolf
//LogFileTimer := TTimer.Create(nil);
//LogFileTimer.Enabled := false;      //just to be sure, enable in StartLogging
//LogFileTimer.Interval := OneMinute; //set with SetLogFileTimerInterval before calling StartLogging!
//LogFileTimer.OnTimer := {$IFDEF FPC}@OnLogFileTimer{$ELSE}OnLogFileTimer{$ENDIF};
  (* end rbecker *)

  while Not(Self.hlParser.FStarted) do sleep(5);
  while Not(Self.hlWriter.FStarted) do sleep(5);

  hlParser.FWriterTID := hlWriter.ThreadID;
  With hlParser Do
  begin
    FErrJumpLines := true;
    SurroundCh    := '*';
    ErrorCaption  := '***    E R R O R    ***';
    FErrViewStyle :=  vsBasic;
  end;
  Self.FStarted := false;                         // will be set to true once writer and parser...
end;                                              // ... resume methode will have been called

destructor THotLog.Destroy;
begin
  // First of all, tries to Stop visualFeedBack, waiting up to one half a second.
  If assigned(hlVisual) then
     If Not Self.StopVisualFeedBack then
        Self.StopVisualFeedBack;

  // then "stop" the log file timer - rbecker
// --- Rolf
//If Assigned(LogFileTimer) then
//begin
//  LogFileTimer.Enabled := false;
//  LogFileTimer.Free;
//end;

  // then stops both parser and writer. The particular point is that
  // they are almost surely assigned, but may never have been 'resumed'.
  // In such a case, WaitFor would wait for a very, very long time...

  If Assigned (HLParser) then
  begin
    If not(hlparser.Suspended) then
    begin
      // --- Rolf
      HLParser.Terminate;
//    PostThreadMessage(HLParser.ThreadId, WM_Quit,0,0);
      HLParser.WaitFor;
    end;
    HLParser.Free;
  end;

  If Assigned (HLWriter) then
  begin
    If not(HLWriter.Suspended) then
    begin
      // --- Rolf
      HLWriter.Terminate;
//    PostThreadMessage(HLWriter.ThreadID, WM_Quit,0,0);
      HLWriter.WaitFor;
    end;
    HLWriter.Free;
  end;
  
  // Visual has FreeOnTerminate := true;
  If assigned(hlVisual) then
    hlVisual.Terminate;

  If Assigned(footer) then
    FreeAndNil(footer);
  If Assigned(header) then
    FreeAndNil(header);
  inherited;
end;

procedure THotLog.DisplayFeedBackInto (aMemo: TMemo);

// Enables (or re-enables) the showing of the logged strings into a memo.

// If Writer no longer runs, nothing to do...
// Otherwise : Instanciate FeedBacker if needed, give it the memo handle,
// give its threadID to writer (in order for him to now to whom to post
// visual feedBack datas), then tries to catch the FDoFeedBack protection
// mutex, and enable feedBack.

var tries: integer;
    done : boolean;
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  If (WaitForSingleObject(Self.hlWriter.Handle, 0) <> WAIT_TIMEOUT) then
    Exit;                                             // hlWriter terminated
  try
    If Not (assigned( hlVisual )) then
    begin
      hlVisual := THotLogVisual.Create;
      tries := -1; 
      done := false;
      while not(done) do
      begin
        Sleep(5); Inc(tries);
        Done := (hlVisual.FStarted) Or (tries > 9);
      end;
      If Not(hlVisual.FStarted) then Exit;

      hlWriter.FVisualTID := hlVisual.ThreadID;
      hlVisual.FfbMemo    := aMemo;
    end;
    
    If (WaitForSingleObject(Self.hlVisual.mxHLFeedBack,50) = WAIT_OBJECT_0) then
    begin
      hlVisual.FDoFeedBack := true;     
      hlWriter.FDoFeedBack := true;
      ReleaseMutex(Self.hlVisual.mxHLFeedBack);
    end;
  except
  end;
end;

function THotLog.StopVisualFeedBack:boolean;

// StopVisualFeedback doesn't stop the feedBack Thread, but says Writer to stop
// accessing that thread, and says "FeedBacker" to stop accessing the memo.
// The feedBack thread is still available, and can be launched again through a
// new call to DisplayFeedBackInto(aMemo:TMemo);

// The function returns true if it succeeds, false otherwise. You should
// ALLWAYS check this return before choosing wether closing or hidding the
// form upon which your log feedback memo is, if it isn't on your application's
// main Form.

begin
  Result := (WaitForSingleObject(Self.hlVisual.mxHLFeedBack,50) = WAIT_OBJECT_0);
  If result then                                      // hlVisual terminated
  begin
    hlVisual.FDoFeedBack := false;
    hlWriter.FDoFeedBack := false;
    ReleaseMutex(Self.hlVisual.mxHLFeedBack);
  end;
end;

procedure THotLog.ScrollMemo(doScroll:boolean);
begin
  If Not (assigned( hlVisual )) then Exit;
  hlVisual.FDoScroll := doScroll;
end;

procedure THotLog.SetMemoLimit(value: cardinal);              // Oleg
begin
  If Not (assigned( hlVisual )) then Exit;
  hlVisual.FfbMemoLimit := value;
end;

function THotLog.ModifyLineNumbering(doShow:boolean;
                                     alignment:TAlignment=taRightJustify;
                                     size:integer=3):boolean;

// Log's line numbers management. Can be called at any moment during run time.
// Notify changes to the parser through messages.
// If the parser can't be reached, the function returns false, otherwise it 
// returns true. 

// Line numbers will typically be added at the begining of any line, aligned right,
// on 3 positions (size:=3) , padded with space(s), and followed by one more space.

// Line numbers are NOT displayed by default; hlParser constructor sets line 
// numbering fields to : (doShow: false;  alignment: taRightJustify;  size:3);

// Line counting is allways maintained, be line numbers displayed or not.

// If lineNum representation grows above "size" (for example if you did set
// size to 3 and your log reaches 1000 lines (!), then the size parameter is
// ignored (unless you change it for something bigger);

begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  result := (WaitForSingleObject(self.hlParser.mxHLLineNum,50) = WAIT_OBJECT_0);
  If result then
  begin
    If size = 0 then Self.HLParser.FLnShowNum := false
                else Self.HLParser.FLnShowNum := doShow;
    self.HLParser.FLnAlign   := alignment;
    self.HLParser.FLnNumLen  := size;
    ReleaseMutex(self.hlParser.mxHLLineNum);
    //now notifies the parser to take these changes into account;
    PostThreadMessage(Self.HLParser.ThreadId,UM_HLSERVICEMSG,HL_LINENUMCHANGED,0);
  end;
end;

procedure THotLog.SetErrorCaption (value: string; surroundChar: string;
                                   jumpLines: boolean = true);
                                  
// Defines how errors will appear in the log file. "Value" is the string the
// error line will start with ; If surroundChar is provided, it will be expanded
// -> at least to the size of "value" and will be added above and bellow the error line ;
// -> up to at least 80 characters, if SetErrorViewStyle is set to vsExtended.
//    (see procedure SetErrorViewStyle below)
// If Jumpline is set, an empty line will be added too, before and after line(s).

begin
  With hlParser Do
  begin
    If value = '' then
    begin
      FErrJumpLines := false;
      SurroundCh    := '';
      ErrorCaption  := '';
    end
    else
    begin
      SurroundCh    := UTF8Copy(surroundChar,1,1);
      ErrorCaption  := value;
      FErrJumpLines := jumpLines;
    end
  end;
end;

procedure THotLog.SetErrorViewStyle(style:TVarRecStyle);

// vsBasic or vsExtended : Decides how wide the error surrounding line
// will be. If vsBasic, the suround line will be of the same length than
// the "ErrorCaption" you provided ; if extended, it will be expanded to
// at least 80 chars.
// Ignored if SetErrorCaption() above passed an empty 'Value'.

begin
  Self.hlParser.FErrViewStyle := style;
end;

procedure THotLog.StartLogging;
// Where everything begins...
begin
  If Self.FStarted then Exit;
// --- Rolf
  hlParser.Start;
  hlWriter.Start;
//  hlParser.Resume;
//  hlWriter.Resume;
  while (not hlWriter.FExecuting) do sleep(5);              // Sergueï
// --- Rolf
//if hlWriter.hlFileDef.UseFileSizeLimit then
//  LogFileTimer.Enabled := true;                           //rbecker

  Self.FStarted := true;  
end;

procedure THotLog.SetRulerLength(newLength:integer);
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  If newLength > 0 then
     PostThreadMessage(Self.hlParser.ThreadId,UM_HLSERVICEMSG,HL_RESETRULER,newLength);
end;

procedure THotLog.ResetHeapTable;
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  PostThreadMessage(Self.hlParser.ThreadID, UM_HLSERVICEMSG,
                    HL_RESETHEAPTABLE,0);
end;

// --- Rolf
function THotLog.HeapMonitor (hmExtra: string = ''; directOutput: boolean = false): THeapMRec;

//  -> hmExtra is a user defined string that may be added to the end of
//  the (first) line of outputed values ; As long as StandAlone tags are
//  parsed in one "pass" only, hmExtra cannot be an inline tag, but only a string.
//  -> If directOutput is set, values won't be stored. HeapTable will stay unchanged.
//  -> The result of the call is transmitted into result.hExtra, under the form
//  of a "boolean string", which can be read throught delphi function StrToBool();
//  However, testing (result.hblock=0) or (result.hbytes=0) would give the very
//  same information...

var
{$IFDEF FPC}
  hs:  TFPCHeapStatus;
{$ELSE}
  mms: TMemoryManagerState;
  i:   cardinal;
{$ENDIF}
begin
{$IFDEF FPC}
  Result.MaxHeapSize:=0;
  Result.MaxHeapUsed:=0;
  Result.CurrHeapSize:=0;
  Result.CurrHeapUsed:=0;
  Result.CurrHeapFree:=0;
{$ELSE}
  Result.AllocatedSmallBlockCount:=0;
  Result.TotalAllocatedSmallBlockSize:=0;
  Result.ReservedSmallBlockAddressSpace:=0;
  Result.AllocatedMediumBlockCount:=0;
  Result.TotalAllocatedMediumBlockSize:=0;
  Result.ReservedMediumBlockAddressSpace:=0;
  Result.AllocatedLargeBlockCount:=0;
  Result.TotalAllocatedLargeBlockSize:=0;
  Result.ReservedLargeBlockAddressSpace:=0;
{$ENDIF}
  Result.Extra:='0';                                  // StrToBool('0') -> false

  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  if (WaitForSingleObject(Self.hlParser.Handle, 0) <> WAIT_TIMEOUT) then
    exit;                                             // hlParser not terminated
  try
{$IFDEF FPC}
    hs:=GetFPCHeapStatus;
    Result.MaxHeapSize:=hs.MaxHeapSize;
    Result.MaxHeapUsed:=hs.MaxHeapUsed;
    Result.CurrHeapSize:=hs.CurrHeapSize;
    Result.CurrHeapUsed:=hs.CurrHeapUsed;
    Result.CurrHeapFree:=hs.CurrHeapFree;
{$ELSE}
    GetMemoryManagerState(mms);
    Result.AllocatedSmallBlockCount:=0;
    Result.TotalAllocatedSmallBlockSize:=0;
    Result.ReservedSmallBlockAddressSpace:=0;
    with Result, mms do
      for i:=Low(mms.SmallBlockTypeStates) to High(mms.SmallBlockTypeStates) do
      begin
        AllocatedSmallBlockCount:=AllocatedSmallBlockCount+
          SmallBlockTypeStates[i].AllocatedBlockCount;
        TotalAllocatedSmallBlockSize:=TotalAllocatedSmallBlockSize+
          SmallBlockTypeStates[i].InternalBlockSize*SmallBlockTypeStates[i].AllocatedBlockCount;
        ReservedSmallBlockAddressSpace:=ReservedSmallBlockAddressSpace+
          SmallBlockTypeStates[i].ReservedAddressSpace;
      end;
    Result.AllocatedMediumBlockCount:=mms.AllocatedMediumBlockCount;
    Result.TotalAllocatedMediumBlockSize:=mms.TotalAllocatedMediumBlockSize;
    Result.ReservedMediumBlockAddressSpace:=mms.ReservedMediumBlockAddressSpace;
    Result.AllocatedLargeBlockCount:=mms.AllocatedLargeBlockCount;
    Result.TotalAllocatedLargeBlockSize:=mms.TotalAllocatedLargeBlockSize;
    Result.ReservedLargeBlockAddressSpace:=mms.ReservedLargeBlockAddressSpace;
{$ENDIF}
    Result.Extra:='1';                                // StrToBool('1') -> true;

{$IFDEF FPC}
    with Result, THLHeapMonMsg.Create(MaxHeapSize,MaxHeapUsed,
                                      CurrHeapSize,CurrHeapUsed,CurrHeapFree,
                                      hmExtra) do
{$ELSE}
    with Result, THLHeapMonMsg.Create
      (AllocatedSmallBlockCount,TotalAllocatedSmallBlockSize,ReservedSmallBlockAddressSpace,
       AllocatedMediumBlockCount,TotalAllocatedMediumBlockSize,ReservedMediumBlockAddressSpace,
       AllocatedLargeBlockCount,TotalAllocatedLargeBlockSize,ReservedLargeBlockAddressSpace,
       hmExtra) do
{$ENDIF}
      Post(Self.hlParser.ThreadID,HL_HEAPMONITOR,directOutput);

  except
{$IFDEF FPC}
    Result.MaxHeapSize:=0;
    Result.MaxHeapUsed:=0;
    Result.CurrHeapSize:=0;
    Result.CurrHeapUsed:=0;
    Result.CurrHeapFree:=0;
{$ELSE}
    Result.AllocatedSmallBlockCount:=0;
    Result.TotalAllocatedSmallBlockSize:=0;
    Result.ReservedSmallBlockAddressSpace:=0;
    Result.AllocatedMediumBlockCount:=0;
    Result.TotalAllocatedMediumBlockSize:=0;
    Result.ReservedMediumBlockAddressSpace:=0;
    Result.AllocatedLargeBlockCount:=0;
    Result.TotalAllocatedLargeBlockSize:=0;
    Result.ReservedLargeBlockAddressSpace:=0;
{$ENDIF}
    Result.Extra:='0';                                // StrToBool('0') -> false
  end;
end;

procedure THotLog.ResetRamTable;
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  PostThreadMessage(Self.hlParser.ThreadID, UM_HLSERVICEMSG,
                    HL_RESETRAMTABLE,0);
end;

// --- Rolf: ...Ex
function THotLog.RamMonitor (hmExtra: string = ''; directOutput: boolean = false): THeapMRec;
var
  mse: TMemoryStatusEx;
begin
  try
    mse.dwLength := SizeOf(mse);
    GlobalMemoryStatusEx(mse);

    Result.MemoryLoad:=mse.dwMemoryLoad;
    Result.TotalPhysical:=mse.ullTotalPhys;
    Result.AvailPhysical:=mse.ullAvailPhys;
    Result.TotalPageFile:=mse.ullTotalPageFile;
    Result.AvailPageFile:=mse.ullAvailPageFile;
    Result.TotalVirtual:=mse.ullTotalVirtual;
    Result.AvailVirtual:=mse.ullAvailVirtual;
    Result.Extra:= '1';                                    // "true"

    // --- Rolf
    if not Self.FStarted then
      Self.StartLogging;

    with mse, THLHeapMonMsg.Create(dwMemoryLoad,
                                   ullTotalPhys,ullAvailPhys,
                                   ullTotalPageFile,ullAvailPageFile,
                                   ullTotalVirtual,ullAvailVirtual,
                                   hmExtra) do
      Post(Self.hlParser.ThreadID,HL_RAMMONITOR,directOutput);
  except
    Result.MemoryLoad:=0;
    Result.TotalPhysical:=0;
    Result.AvailPhysical:=0;
    Result.TotalPageFile:=0;
    Result.AvailPageFile:=0;
    Result.TotalVirtual:=0;
    Result.AvailVirtual:=0;
    result.Extra  := '0';                                  // "false"
  end;
end;

procedure THotLog.JumpLine (linesToJump: integer = 1);
//  writes "linesToJump" CRLF.
var
  i: integer;
begin
  if (linesToJump < 1) then
    exit
  else 
    if (WaitForSingleObject(Self.hlParser.Handle, 0) = WAIT_TIMEOUT) then      // hlParser not terminated ?
      PostThreadMessage(Self.hlParser.ThreadID, UM_HLACTIONMSG,HL_JUMPLINES, linesToJump)
    else
      for i:=0 to linesToJump -1 do
        Self.AddStr(#13#10);
end;

procedure THotLog.Add (aString: string);
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  if (WaitForSingleObject(Self.hlParser.Handle, 0) = WAIT_TIMEOUT) then
    with THLStringMsg.Create(aString) do              // hlParser not terminated
      Post(Self.hlParser.ThreadID,HL_PARSELINE);
end;

procedure THotLog.Add (aStringList: TStringList);
var
  sl: TStringList;
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  try
    sl := TStringList.Create;
    sl.Assign(aStringList);
    if (WaitForSingleObject(Self.hlParser.Handle, 0) = WAIT_TIMEOUT) then      // hlParser not terminated ?
      PostThreadMessage(Self.hlParser.ThreadId, UM_HLACTIONMSG,
                        HL_PARSELIST,LPARAM(sl));
  except
    with THLStringMsg.Create('#HL_Invalid_TSList') do
      Post(Self.hlParser.ThreadID, HL_SIMPLESTRING);
  end;
end;

procedure THotLog.Add (style: TVarRecStyle; aConstArray: array of Const);
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  with THLConstArrayMsg.Create(style) do
  begin
    FConstArray := Self.CreateConstArray(aConstArray);
    Post(Self.hlParser.ThreadID);
  end;
end;

procedure THotLog.AddStr (aString: string);
// aString will be written without parsing.
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  if (WaitForSingleObject(Self.hlParser.Handle, 0) = WAIT_TIMEOUT) then
    with THLStringMsg.Create(aString) do              // hlParser not terminated
      Post(Self.hlParser.ThreadID);
end;

procedure THotLog.AddException (ex: Exception; func: string; args : array of const;
                                err: integer = 0; freeAfterPost: boolean = false);
// Overloaded definition, used for transmission of routine names and parameters
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  try
    if (WaitForSingleObject(Self.hlParser.Handle, 0) = WAIT_TIMEOUT) then
    begin                                             // hlParser not terminated
      with THLErrMsg.Create do                        // Build message
      begin
        FEMsg    := ex.message;
        FlastErr := err;
        Ffull    := Length(args) > 0;
        FFunc    := func;
        FArgs    := Self.CreateConstArray(args);
        Post(Self.hlParser.ThreadID);
      end;
      if freeAfterPost then
        ex.Free;
    end;
  except
  end;
end;

procedure THotLog.AddException (ex: Exception; err: integer = 0;
                                freeAfterPost: boolean = false);
// Basic Exception storage and sending.
begin
  // --- Rolf
  if not Self.FStarted then
    Self.StartLogging;

  try
    if (WaitForSingleObject(Self.hlParser.Handle, 0) = WAIT_TIMEOUT) then
    begin                                             // hlParser not terminated
      with THLErrMsg.Create do
      begin
        FEMsg    := ex.message;
        FlastErr := err;
        Ffull    := false;
        Post(Self.hlParser.ThreadID);
      end;
      if freeAfterPost then ex.Free;
    end;
  except
  end;
end;

procedure THotLog.AddError (err: integer; func: string; args : array of const);
begin
  // --- Rolf: UTF8
  Self.AddException(Exception.Create(SysErrorMessageUTF8(err)),func,args,err,true);
end;

procedure THotLog.AddError(err:integer);
// basic procedure, to allow "exception formatting" for errors too;
begin
  // --- Rolf: UTF8
  Self.AddException(Exception.Create(SysErrorMessageUTF8(err)), err, true );
end;

// --- Rolf
//procedure THotLog.SetLogFileTimerInterval(Interval : cardinal);
// set time delay for log file timer - rbecker
//begin
//  LogFileTimer.Interval := Interval;
//end;

////////////////////////////////////////////////////////////////////////////////
// Handling TVarRec & TVarRecValue  (Main thread's side)
////////////////////////////////////////////////////////////////////////////////

function THotLog.CreateConstArray(const Elements: array of const): TConstArray;   // (c)Rudy Velthuis
var I: integer;
begin
  SetLength(Result, Length(Elements));
  For I := Low(Elements) to High(Elements) do
      Result[I] := CopyVarRec(Elements[I]);
end;

// --- Rolf
function THotLog.CopyVarRec (const Item: TVarRec): TVarRecValue;
var
  s: string;
begin
  Result.VType:=Item.VType;
  Result.Value:='';

{---Lazarus---------------------------------------------------------------------
  type TVarRec = record
    case VType: SizeInt of
      vtInteger: (
          VInteger: LongInt;	  	Integer value
        );
      vtBoolean: (
          VBoolean: Boolean;	  	Boolean value
        );
      vtChar: (
          VChar: Char;	  	        Character value
        );
      vtWideChar: (
          VWideChar: WideChar;	  	Widechar value
        );
      vtExtended: (
          VExtended: PExtended;	  	Extended value
        );
      vtString: (
          VString: PShortString;	String value
        );
      vtPointer: (
          VPointer: Pointer;	  	Pointer value
        );
      vtPChar: (
          VPChar: PChar;	  	PChar value (null-terminated string)
        );
      vtObject: (
          VObject: TObject;	  	Object value (instance pointer)
        );
      vtClass: (
          VClass: TClass;	  	Class pointer value (VMT pointer)
        );
      vtPWideChar: (
          VPWideChar: PWideChar;	Widechar value
        );
      vtAnsiString: (
          VAnsiString: Pointer;	  	Ansistring value
        );
      vtCurrency: (
          VCurrency: PCurrency;	  	Currency value
        );
      vtVariant: (
          VVariant: PVariant;	  	Variant value
        );
      vtInterface: (
          VInterface: Pointer;	  	Interface value
        );
      vtWideString: (
          VWideString: Pointer;	  	Widestring value
        );
      vtInt64: (
          VInt64: PInt64;	  	Int64 value
        );
      vtUnicodeString: (
          VUnicodeString: Pointer;	Unicode string
        );
      vtQWord: (
          VQWord: PQWord;	  	QWord value
        );
  end;
-------------------------------------------------------------------------------}
{---Delphi----------------------------------------------------------------------
  type TVarRec = record   do not pack this record; it is compiler-generated
    case Integer of
      0: (case Byte of
            vtInteger:       (VInteger: Integer);
            vtBoolean:       (VBoolean: Boolean);
 $IFNDEF NEXTGEN
            vtChar:          (VChar: _AnsiChr);
 $ENDIF !NEXTGEN
            vtExtended:      (VExtended: PExtended);
 $IFNDEF NEXTGEN
            vtString:        (VString: _PShortStr);
 $ENDIF !NEXTGEN
            vtPointer:       (VPointer: Pointer);
 $IFNDEF NEXTGEN
            vtPChar:         (VPChar: _PAnsiChr);
 $ENDIF !NEXTGEN
 $IFDEF AUTOREFCOUNT
            vtObject:        (VObject: Pointer);
 $ELSE
            vtObject:        (VObject: TObject);
 $ENDIF
            vtClass:         (VClass: TClass);
            vtWideChar:      (VWideChar: WideChar);
            vtPWideChar:     (VPWideChar: PWideChar);
 $IFNDEF NEXTGEN
            vtAnsiString:    (VAnsiString: Pointer);
 $ENDIF !NEXTGEN
            vtCurrency:      (VCurrency: PCurrency);
            vtVariant:       (VVariant: PVariant);
            vtInterface:     (VInterface: Pointer);
            vtWideString:    (VWideString: Pointer);
            vtInt64:         (VInt64: PInt64);
            vtUnicodeString: (VUnicodeString: Pointer);
         );
      1: (_Reserved1: NativeInt;
          VType:      Byte;
         );
  end;
-------------------------------------------------------------------------------}
try
  case Item.VType of
      vtInteger:       Str(Item.VInteger,Result.Value);
      vtBoolean:       Result.Value:=Bool2Str[Item.VBoolean];
      vtChar:          Result.Value:=Item.VChar;
      vtWideChar:      Result.Value:=UTF8Encode(Item.VWideChar);
      vtExtended:      begin
                         Str(Item.VExtended^,Result.Value);
                         Result.Value:=Trim(Result.Value);
                       end;
      vtString:        Result.Value:=Item.VString^;
      vtPointer:       if Item.VPointer=nil then
                         Result.Value:='^(NIL)'
                       else
                         Result.Value:=Format('^(%P)',[item.VPointer]);
      vtPChar:         Result.Value:=AnsiChar(Item.VPChar^);
      vtObject:        if not(Assigned(Item.VObject)) then
                         Result.Value:='(NIL)'
                       else
                       begin
                         try
                           s:=Item.VObject.classname;
                           if Item.VObject is TComponent then
                             s:=s+' (' +TComponent(Item.VObject).Name+')';;
                           if Item.VObject is TControl then
                             s:=s+' ('''+TButton(Item.VObject).Caption+''')';
                           if Item.VObject is TEdit then
                             s:=s+' ('''+TEdit(Item.VObject).Text+''')';
                           if (Item.VObject is TList) then
                             s:=s+' (capacity='+IntToStr(TList(Item.VObject).Capacity);
                           if (Item.VObject is TStringList) then
                             s:=s+' (capacity='+IntToStr(TStringList(Item.VObject).Capacity)
                                 +', count='+IntToStr(TStringList(Item.VObject).Count)+')';
                         except
                           s:=s+' !_DANGLING_POINTER_!';
                         end;
                         Result.Value:=s;
                       end;
//    vtClass:
      vtPWideChar:     Result.Value:=UTF8Encode(Item.VPWideChar^);
      vtAnsiString:    Result.Value:=AnsiString(Item.VAnsiString);
      vtCurrency:      begin
                         Str(Item.VCurrency^,Result.Value);
                         Result.Value:=Trim(Result.Value);
                       end;
      vtVariant:       Result.Value:=Item.VVariant^;
      vtInterface:     Result.Value:='(Interface object)';
      vtWideString:    Result.Value:=UTF8Encode(WideString(Item.VWideString));
      vtInt64:         Str(Item.VInt64^,Result.Value);
      vtUnicodeString: Result.Value:=UTF8Encode(UnicodeString(Item.VUnicodeString));
{$IFDEF FPC}
      vtQWord:         Str(Item.VQWord^,Result.Value);
{$ENDIF}
      else Result.Value:=Format('[#HLvrType(%s)]',[vTypeDesc[Item.VType]]);
    end;
  except
    Result.Value:=Result.Value+Format('[#HLvrValue(%s)]',[vTypeDesc[Item.VType]]);
  end;
end;


    {----------------------------------------------------------}
    {---                  Threads : Parser                     }
    {----------------------------------------------------------}
    {----------------------------------------------------------}

constructor THotLogParser.Create;
begin
  inherited create(true);
  Self.FpvLineCount:= 0;
  Self.FshowLineNum:= false;
  Self.FpvLnNumLen := 3;
  Self.FpvMaxValue := 999;
  Self.FpvLnAlign  := taRightJustify;
  mxHLLineNum := CreateMutexW(nil,false,pWideChar
    ('THLLineNumsMutex'+DateTimeToStr(Now)+
     IntToStr({$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF})));
  HMonitor := THeapMonitor.Create(mkHeap);
  RMonitor := THeapMonitor.Create(mkRam);
  Self.fRulerLength:= 80;
  Self.fRulerDots  := '';
  Self.fRulerNums  := '';
  Self.fRulerBuild := false;
  Self.FsRegSpeed  := '';
  Self.FStarted    := true;
end;

destructor THotLogParser.Destroy;
begin
  HMonitor.Free;
  RMonitor.Free;
  If mxHLLineNum>0 then
    CloseHandle(mxHLLineNum);
  inherited Destroy;
end;


////////////////////////////////////////////////////////////////////////////////
// internals
////////////////////////////////////////////////////////////////////////////////
procedure THotLogParser.UpdateLineNum;
begin
  If WaitForSingleObject(mxHLLineNum,50) = WAIT_OBJECT_0 then
  begin
    try
      FshowLineNum := FLnShowNum;
      FpvLnNumLen  := FLnNumLen;
      FpvMaxValue  := Trunc(Power(10,FLnNumLen))-1;
      FpvLnAlign   := FLnAlign;
      ReleaseMutex(mxHLLineNum);
    except
    end;
  end;
end;

procedure THotLogParser.SetFErrCaption (Value: string);
begin
  Self.FErrorCaption:=Value;
  if Self.FErrIsSurrounded and (Self.FErrSurroundChar<>'') then
// --- Rolf
    Self.FErrSurroundLine:=
      UTF8StringOfChar(Self.FErrSurroundChar,UTF8Length(Value));
//  for i:=0 To UTF8Length(value)-1 do
//    Self.FErrSurroundLine:=Self.FErrSurroundLine+Self.FErrSurroundChar;

  Self.FErrWideSurround:=
    UTF8StringOfChar(Self.FErrSurroundChar,80);
//Self.FErrWideSurround:=Self.FErrSurroundLine;
//repeat
//    FErrWideSurround:=FErrWideSurround+FErrSurroundLine;
//  until UTF8Length(FErrWideSurround)>=80;
end;

procedure THotLogParser.SetFErrSurroundChar(Value : string);
begin
  Self.FErrSurroundChar := Value;
  If Value = '' then Self.FErrIsSurrounded := false
  else begin
         Self.FErrIsSurrounded := true;
         If Self.FErrorCaption <> ''
            then SetFErrCaption(Self.FErrorCaption);  // (Re)build surround line's length
       end;
end;

function THotLogParser.GetLineNum: string;
var
  i, iPad: integer;
begin
  Result:='';
  try
    Result:=IntToStr(FpvLineCount);
    If FpvLineCount<=FpvMaxValue then
    begin
      iPad:=FpvLnNumLen-UTF8Length(Result);
      Case self.FLnAlign of
        taRightJustify: for i:=1 to iPad do Result:=' '+Result;
        taLeftJustify:  for i:=1 to iPad do Result:=Result+' ';
          //taCenter : ...
      end; {case}
    end;   {if}
    Result:=Result+' ';                               // final space
  except
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// TVarRecValue, again   (parser's side)
////////////////////////////////////////////////////////////////////////////////
// --- Rolf
function THotLogParser.VarRecToStr (vrv: TVarRecValue): string;
begin
  Result:=Format('%-13s:  %s',[vTypeAsSring[vrv.VType],vrv.Value]);
end;

// --- Rolf
procedure THotLogParser.FinalizeConstArray (var Arr: TConstArray);
var
  i: integer;
begin
  for i:=Low(Arr) to High(Arr) do
    Arr[i].Value:='';
  Arr:=nil;
end;

function THotLogParser.ConstArrayToTSList (const Elements: TConstArray): TStringList;
// -1-> Returns a TStringList, with items like 'vType : vValue[,details]'
//      Once done, items will be checked, to restitute their real type
//      (to handle the particular cases of Pointers and Objects, that we translated
//      to strings earlier).
var
  i: integer;
begin
  Result:=TStringList.create;
  try
    for i:=Low(Elements) to High(Elements) do
      Result.Add(VarRecToStr(Elements[i]));
    // --- Rolf
    {For i := 0 To result.Count -1 Do
        If Pos('$_H_',result.Strings[i]) > 0
        then result.Strings[i] := GetOriginalValue(result.Strings[i]);}
  except
    result.Clear;
    result.Add('[#HLvrConvert]');
  end;
end;

function THotLogParser.ConstArrayToString (const Elements: TConstArray;
                                           style: TVarRecStyle = vsBasic): string;
// -2-> Returns a string, surrounded by parenthesis : '(elts[0]; ...; elts[n-1]);'
//      ("Basic infos" only.)
var
  i:     integer;
  s,sep: string;
begin
// --- Rolf
  Result:='';
  try
    If style=vsBasic then
    begin
      Result:='(';
      sep:='; ';
    end
    else
      sep:='';

    for i:=Low(Elements) to High(Elements) do
    begin
      s:=VarRecToStr(Elements[i]);
      Result:=Result+GetBasicValue(s)+sep;
    end;

    if style=vsBasic then
      Result:=LeftStr(Result,length(result)-1)+')';   // replaces last " " by final ")".
  except
    Result:='[#HLvrConvert]';
  end;
end;

// --- Rolf
function THotLogParser.GetBasicValue (s: string): string;
var
  i: integer;
begin
  Result:=s;
  if s='' then
    exit;
  i:=pos(':  ',Result);
  if i<=0 then
    exit;
  System.delete(Result,1,i+2);
end;

// --- Rolf
{function THotLogParser.GetOriginalValue (s: string): string;
//  Called to remove the false 'UnicodeString :' assertion, for pointers and objects
var
  i: integer;
begin
  i:=pos(':  ',s);
  if i>0 then
    Result:=copy(s,i+3)
  else
    Result:=s;
  //Result:=RightStr(s,Length(s)-19);
end;}


////////////////////////////////////////////////////////////////////////////////
//  work
////////////////////////////////////////////////////////////////////////////////

procedure THotLogParser.Execute;
var
  tmpMsg: MSG;                                                              // messages packed record
  SlTmp:  TStringList;
  sTmp:   string;
  i:      integer;
begin
  tmpMsg.Message:=0;
  try {except}
// --- Rolf
    while GetMessage(tmpMsg,0,0,0) do
    begin
//      if PeekMessage(tmpMsg, 0, 0, 0, PM_REMOVE) then
      if Terminated then
        exit;
      case tmpMsg.message of
//      WM_QUIT         : Terminate;

        UM_HLSERVICEMSG : case(tmpMsg.wParam) of
                            HL_LINENUMCHANGED:  UpdateLineNum;
                            HL_RESETRULER:      ResetRuler(tmpMsg.lParam);
                            HL_RESETHEAPTABLE:  HMonitor.ResetMemTable;
                            HL_RESETRAMTABLE:   RMonitor.ResetMemTable;
                            HL_HEAPMONITOR:     begin
                                                  HMonitor.AddRec({%H-}PHLHeapMonMsg(tmpMsg.lParam)^.hmr);
                                                  THLHeapMonMsg(tmpMsg.lParam).free;
                                                end;
                            HL_RAMMONITOR:      begin
                                                  RMonitor.AddRec({%H-}PHLHeapMonMsg(tmpMsg.lParam)^.hmr);
                                                  THLHeapMonMsg(tmpMsg.lParam).free;
                                                end;
                          end; {case wParam}

        UM_HLACTIONMSG  : case(tmpMsg.wParam) of
                            HL_SIMPLESTRING:    begin
                                                  sTmp:=AddLineNum(THLStringMsg(tmpMsg.lParam).fHlMsg);
                                                  PostThreadMessage(Self.FWriterTID,UM_HLSIMPLESTRING,
                                                                    LPARAM(THLStringMsg.Create(sTmp)),0);
                                                  THLStringMsg(tmpMsg.lParam).Free;
                                                end;{HL_SIMPLESTRING}
                            HL_PARSELINE:       begin
                                                  slTmp:=ParseLine(THLStringMsg(tmpMsg.lParam).fHlMsg);
                                                  PostThreadMessage(Self.FWriterTID,UM_HLSTRINGLIST,
                                                                    LPARAM(slTmp),0);
                                                  THLStringMsg(tmpMsg.lParam).Free;
                                                end;{HL_PARSELINE}
                            HL_PARSELIST:       begin
                                                  slTmp:=ParseList(TStringList(tmpMsg.lParam));
                                                  PostThreadMessage(Self.FWriterTID,UM_HLSTRINGLIST,
                                                                    LPARAM(slTmp),0);
                                                  TStringList(tmpMsg.lParam).Free;
                                                 end;{HL_PARSELIST}
                            HL_PARSEARRAY:       begin
                                                   slTmp:=ParseArray(THLConstArrayMsg(tmpMsg.lParam));
                                                   PostThreadMessage(Self.FWriterTID,UM_HLSTRINGLIST,
                                                                     LPARAM(slTmp),0);
                                                   THLConstArrayMsg(tmpMsg.lParam).Free;
                                                 end;{HL_PARSEARRAY}
                            HL_PARSEEXCEPTION:   begin
                                                   slTmp:=ParseException(THLErrMsg(tmpMsg.lParam));
                                                   PostThreadMessage(Self.FWriterTID,UM_HLSTRINGLIST,
                                                                     LPARAM(slTmp),0);
                                                   THLErrMsg(tmpMsg.lParam).Free;
                                                 end;{HL_PARSEEXCEPTION}
                            HL_JUMPLINES:        begin
                                                   slTmp:=TStringList.Create;
                                                   for i:=0 to tmpMsg.lParam-1 do
                                                   slTmp.Add(AddLineNum(''));
                                                   PostThreadMessage(Self.FWriterTID,UM_HLSTRINGLIST,
                                                                     LPARAM(slTmp), 0);
                                                   // nothing to free here
                                                 end;{HL_JUMPLINES}
                            HL_HEAPMONITOR:      begin                       // direct output requested
                                                   // --- Rolf               // 0->Full result
                                                   slTmp:=GetHeap(@({%H-}PHLHeapMonMsg(tmpMsg.lParam)^.hmr),0);
                                                   AddLineNum(slTmp);
                                                   // slTmp sera libérée plus tard
                                                   PostThreadMessage(Self.FWriterTID,UM_HLSTRINGLISt,
                                                                     LPARAM(slTmp),0);
                                                   THLHeapMonMsg(tmpMsg.lParam).Free;
                                                 end;{HL_HEAPMONITOR}
                            HL_RAMMONITOR:       begin
                                                   // --- Rolf               // 0->Full result
                                                   slTmp:=GetRam(@({%H-}PHLHeapMonMsg(tmpMsg.lParam)^.hmr),0);
                                                   AddLineNum(slTmp);
                                                   PostThreadMessage(Self.FWriterTID,UM_HLSTRINGLISt,
                                                                     LPARAM(slTmp),0);
                                                   THLHeapMonMsg(tmpMsg.lParam).Free;
                                                 end; {HL_RAMMONITOR}
                          end; {Case wParam}
          // No else to come...
      end; {while GetMessage & case tmpMsg.message}
// --- Rolf
    end;
//      if not Terminated then
//        Sleep(50);
  except
// --- Rolf
    on E:Exception do
      DbgSendMsg('HotLog THLPE01: %s',[E.Message]);
  end;
end;

// --- Rolf
procedure THotLogParser.Terminate;
begin
  inherited Terminate;
  PostThreadMessage(ThreadID,WM_QUIT,0,0);
end;

procedure THotLogParser.ResetRuler (value: integer);
begin
  Self.fRulerLength:= value;
  Self.fRulerDots  := '';
  self.fRulerNums  := '';
  self.fRulerBuild := false;
end;

function THotLogParser.AddLineNum (line: string): string;
begin
  result := line;
  inc(Self.FpvLineCount);                             // Line counter is incremented anycase...
  if Self.FshowLineNum then
    Result:=Self.GetLineNum + line;                   // ... and then added, if needed;
end;

procedure THotLogParser.AddLineNum (var lst: TStringList);
var
  i:integer;
begin
  for i:=0 to lst.Count-1 do
  begin
    inc(Self.FpvLineCount);
    if Self.FshowLineNum then
      lst.Strings[i]:=Self.GetLineNum+lst.Strings[i];
  end;
end;

function THotLogParser.ParseException (HLErrMsg: THLErrMsg): TStringList;
var
  sTmp,spaces: string;
  slTmp:       TStringList;
  i,iPos:      integer;
begin
  Result:=TStringList.Create;
  if not Assigned(HLErrMsg) then
    exit;
  If Self.FErrJumpLines then
    Result.Add('');
  case Self.FErrViewStyle of
    vsBasic:    begin
                  if Self.FErrIsSurrounded then
                    Result.Add(Self.FErrSurroundLine);
                  sTmp:=Self.ErrorCaption+' ';
                  if HLErrMsg.FlastErr<>0 then
                    sTmp:=sTmp+ '('+IntToStr(HLErrMsg.FlastErr)+') ';
                  if HLErrMsg.FEMsg <> '' then
                    sTmp:=sTmp+'"'+HLErrMsg.FEMsg+'" ';
                  if HLErrMsg.FFunc<>'' then
                    sTmp:=sTmp+' in '+HLErrMsg.FFunc;
                  if HLErrMsg.Ffull then
                  begin
                    sTmp:=sTmp+ConstArrayToString(HLErrMsg.Fargs);
                    FinalizeConstArray({%H-}HLErrMsg.Fargs);
                  end;
                  Result.Add(sTmp);
                  if Self.FErrIsSurrounded then
                    Result.Add(Self.FErrSurroundLine);
                end;
    vsExtended: begin
                  if Self.FErrIsSurrounded then
                    Result.Add(Self.FErrWideSurround);
                  sTmp:=Self.ErrorCaption+' ';
                  if HLErrMsg.FlastErr<>0 then
                    sTmp:=sTmp+'('+IntToStr(HLErrMsg.FlastErr)+') ';
                  if HLErrMsg.FEMsg<>'' then
                    sTmp:=sTmp+'"'+HLErrMsg.FEMsg+'"';

                  if HLErrMsg.Ffull then
                  begin
                    sTmp:=sTmp+' in :';
                      Result.Add(sTmp);                                   // New line
                    iPos:= UTF8Pos('"',sTmp)-
                      UTF8Length(Self.FErrSurroundLine);                  // 1 : trouver le 1er 'buttoir'

                    spaces:='';
                    for i:=0 to iPos-1 do
                      spaces:=spaces+' ';
                    sTmp:=FErrSurroundLine;
                    sTmp:=sTmp+spaces+HLErrMsg.FFunc;
                    spaces:='';

                    //now waiting for the first "arg"
                    slTmp:=ConstArrayToTSList(HLErrMsg.Fargs);            // 2 : alimenter tslist tmp
                    if slTmp.Count<=0 then
                      sTmp:=sTmp+'()'
                    else
                    begin
                      sTmp:=sTmp+'( ';
                      iPos:=UTF8Length(sTmp);                             // 3 : trouver le 'buttoir'
                      sTmp:=sTmp+slTmp.Strings[0];
                      Result.Add(sTmp);
                      slTmp.Delete(0);
                      for i:=0 to iPos-1 do
                        spaces:=spaces+' ';
                      for i:=0 to slTmp.Count-1 do                        // 4 : insérer les espaces j-> buttoir
                      slTmp.Strings[i]:=spaces+slTmp.Strings[i];
                      i:=slTmp.Count-1;
                      slTmp.Strings[i]:=slTmp.Strings[i]+' );';
                      Result.AddStrings(slTmp);                           // Construire le résultat
                      slTmp.Free;                                         // slTmp was created in ConstArrayToStr.
                      FinalizeConstArray(HLErrMsg.Fargs);
                    end;
                  end
                  else
                  begin
                    if HLErrMsg.FFunc<>'' then
                      sTmp:=sTmp+' in '+HLErrMsg.FFunc;
                    if HLErrMsg.Ffull then
                      sTmp:=sTmp+ConstArrayToString(HLErrMsg.Fargs);
                    Result.Add(sTmp);
                    FinalizeConstArray(HLErrMsg.Fargs);
                  end;

                  if Self.FErrIsSurrounded then
                    Result.Add(Self.FErrWideSurround);
                end;
  end;{case}
  if Self.FErrJumpLines then
    Result.Add('');
  AddLineNum(Result);
end;

function THotLogParser.ParseArray (HLConstArrayMsg: THLConstArrayMsg): TStringList;
begin
  Result:=nil;
  if not Assigned(HLConstArrayMsg) then
  begin
    Result:=TStringList.Create;
    exit;
  end;
  try
    case HLConstArrayMsg.FOutputStyle of
      vsNone, vsBasic:
        Result:=ParseLine(ConstArrayToString(HLConstArrayMsg.FConstArray,
                                             HLConstArrayMsg.FOutputStyle));
      vsExtended:
        Result:=ParseList(ConstArrayToTSList(HLConstArrayMsg.FConstArray));
    end;{case}
  finally
    FinalizeConstArray(HLConstArrayMsg.FConstArray );
  end;
end;

function THotLogParser.ParseLine (source: string): TStringList;
// Main function for parsing strings (appart from errors) ;
var
  pil:         TList;                                 // Parse(d) Item List
  i:           integer;
  tmpOmitLNum: boolean;                               // line numbers eventual override;
begin                                                 // Check de '' fait dans LineToList
  Result:=TStringList.Create;                         // pil := TList.Create; fait dans LineToList
  tmpOmitLNum:=false;
  try
    try
      pil:=LineToList(Source);                        // LineToList will create the List (and fill it)
      PrepareParsing(pil);                            // retourne un type (inLine ou standAlone) et la liste "packée";
      TranslateTags(pil,tmpOmitLNum);
      for i:=0 to pil.count-1 do
        Result.Add(PParseItem(pil[i])^.sValue);       // original or converted...
    except
      Result.Add('#HLParserSTR('+source+')');
      exit;
    end;
  finally
    if tmpOmitLNum then
    begin
      FShowLNTemp:=FShowLineNum;
      FShowLineNum:=false;
      AddlineNum(Result);
      FShowLineNum:=FShowLNTemp;
    end
    else
      AddlineNum(Result);
    try
      FreeParsedList(pil);
      pil.Free;
    except
    end;
  end;
end;

// --- Rolf
function THotLogParser.ParseList (StringList: TStringList): TStringList;
// stringsLists parsing;
var
  pil:         TList;                                 // Parse(d) Item List
  i,j:         integer;
  tmpOmitLNum: boolean;                               // line numbers eventual override;
begin                                                 // Check de '' fait dans LineToList
  Result:=TStringList.Create;                         // pil := TList.Create; -> fait dans LineToList
  if not Assigned(StringList) then
    exit;
  tmpOmitLNum:=false;
  try
    try
      for j:=0 to StringList.count-1 do
      begin
        pil:=LineToList(StringList.Strings[j]);
        PrepareParsing(pil);
        TranslateTags(pil,tmpOmitLNum);
        for i:=0 to pil.count-1 do
            Result.Add(PParseItem(pil[i])^.sValue);
        try
          FreeParsedList(pil);
          pil.Free;
        except
        end;
      end;
    except
      Result.Add('#HLParserTSL.Strings)');
    end;

  finally
    if tmpOmitLNum then
    begin
      FShowLNTemp:=FShowLineNum;
      FShowLineNum:=false;
      AddlineNum(Result);
      FShowLineNum:=FShowLNTemp;
    end
    else
      AddlineNum(Result);
  end;
end;

function THotLogParser.LineToList (line: string): TList;
// instancie une TLsit, et la renvoi alimentée sous la forme
// d'une entrée PParseItem par string/tag lu ;
var
  aPitem:  PParseItem;
  sRemain: string;
  done:    boolean;
begin
  Result:=TList.Create;
  sRemain:=line;
  try
    If sRemain='' then                                // empty line
    begin
      new(aPitem);
      aPitem^.kind:=ikStr;
      aPitem^.sValue:=#13#10;
      aPitem^.isSingle:= false;
      Result.Add(aPitem);
    end
    else                                              // Loops searching for tags
    begin                                             // (and removing their brackets)
    //1 remove $d$a
    // --- Rolf
      while (sRemain<>'') and CharInSet(sRemain[length(sRemain)],[#13,#10]) do
        System.Delete(sRemain,length(sRemain),1);
      done:=false;
      while not done do
      begin
        New(aPitem);
        done:=ExtractItem(sRemain,aPitem);
        Result.Add(aPitem);
      end;
    end;
  except
    on E:Exception do
    begin
      new(aPitem);
      aPitem^.kind:=ikStr;
      aPitem^.sValue:='#HLParsing('+E.Message+')with:('+sRemain+')';
      aPitem^.isSingle:= false;
      Result.Add(aPitem);
    end;
  end;
end;

function THotLogParser.ExtractItem (var source: string; var itm: PParseItem): boolean;
// "itm" will be either a tag (without brackets) or a string.
//  Returns true (+last item) once there's nothing more to extract;
var
  op,
  cl:   integer;
  extr: string;
begin
  try {finally}
    try {except}
      // ExtractItem is called only if length(source) > 0 ;
      // We'll first check whether there is a tag in source or not;
      op:=Pos('{',source);                            // ASCII: UTF8 isn't an issue
      cl:=Pos('}',source);                            // ASCII: UTF8 isn't an issue
      if (op<=0) or (cl<=0) or (cl<op) then           // No valid Tag. Checked at each call, because ...
      begin                                           // ... a line could embedd valid AND invalid tags
        itm^.kind:=ikStr;
        itm^.sValue:=source;
        itm^.isSingle:=false;
        source:='';                                   // Nothing more to do
        Result:=true;                                 // Avoiding Compiler warning,...
        exit;                                         // ...emitted despite the finally Block
      end;

      //op may be =1 or more ;
      if op>1 then                                    // we got a string, easiest case;
      begin
        itm^.kind:=ikStr;
        itm^.sValue:=Copy(source,1,op-1);
        itm^.isSingle:=false;
        source:=Copy(source,op,HL_MAX_INT);           // removes the string
        Result:=(source='');                          // ...Compiler warning...
      exit;
      end;

      // "op" neither 0 nor > 1 => op=1  ;-)   The leading bloc is a tag.
      // We'll first search for '{{}' '{}}' (2nd form being uncovered by cl=pos('}') which returns "2",
      // whereas we'd need "3"), then empty tag '{}'; We will also have to check for 2 charcters strings;
      // len=1 and not a tag : implicitely handeled by first section
      // len=2 : cl can then only be = 2, and these two chars can only be an empty tag ...

      if UTF8Length(source)=2 then                    // ...as long as we arrive there ...
      begin                                           // ...(first section would have detect (op>0 & no close))
        itm^.kind:=ikStr;
        itm^.sValue:='';                              // gets the tag, as string; (can't use source
        itm^.isSingle:=false;
        source:='';                                   // now removes the tag
        Result:=true;                                 // ...Compiler warning...
        exit;
      end;

      //len>=3
      if UTF8Copy(source,3,1)='}' then                // closing mark, we got sthg
      begin                                           // like {{}, {}},{&} or {x}
        if (UTF8Copy(source,2,1)='&') or
           (UTF8Copy(source,2,1)='/') then
          itm^.kind:=ikTag                            // gets the value as tag
        else
          itm^.kind:=ikStr;                           // gets the value as string;
        itm^.sValue:=UTF8Copy(source,2,1);
        itm^.isSingle:=false;
        source:=UTF8Copy(source,4,HL_MAX_INT);        // now removes the tag
      end
      else
        if UTF8Copy(source,2,1)='}' then              // Empty tags are place holders
        begin
          source:=Copy(source,cl+1,HL_MAX_INT);       // no UTF8: cl is the byte position of the closing }
          itm^.kind:=ikStr;
          itm^.sValue:='';                            // gets the tag, as string;
          itm^.isSingle:=false;
        end
        else                                          // Tag found
        begin                                         // open allways = 1, we don't keep opening or closing brackets
          extr:=Copy(source,2,cl-2);                  // no UTF8: cl is the byte position of the closing }
          itm^.kind:=ikTag;
          itm^.sValue:=extr;                          // gets the tag, as string;
          itm^.isSingle:=false;
          source:=Copy(source,cl+1,HL_MAX_INT);       // no UTF8 as above; now removes it
        end;
    except
      itm^.kind:=ikStr;
      itm^.sValue:='#HLTag('+source+')';
      itm^.isSingle:=false;
      source:='';
    end;
  finally
    Result:=(source='');
  end;
end;

procedure THotLogParser.PrepareParsing(var lst:Tlist);
// Converts tags (actually strings) to "unique identifiers" of
// kind integer, to make them checkable later in a case statement;

// If a standAlone tag is met at first position, removes anything
// that would follow it, otherwise, eliminates eventualy embedded
// standAlone Tags not alone in this line;

    function IsStandAloneTag (c: cardinal): boolean;
    begin
      Result := (c = HLT_RULER)
             or (c = HLT_RULER1)
             or (c = HLT_TIMERS)
             or (c = HLT_RAM)
             // --- Rolf
             or (c = HLT_APP_PRM)
             or (c = HLT_HEAP)
             or (c = HLT_DISK)
             or (c = HLT_CPUI)
             or (c = HLT_MEM)
             or (c = HLT_OSVI);
    end;

    {--------------     Local     --------------}
     function HLCheckSum (s: string): cardinal;
     // kinda personnal checksum, to convert tags to single numerical
     // identifiers, in order to check them later in a case statement.
     var c1,c2,
         c3,c4: integer;
         mdl,lng,der: integer;
     begin
       result := 0; If s = '' then exit;
       s   := UpperCase(s);
       mdl := Length(s) mod 4;
       lng := Length(s)-mdl;
       der := lng+mdl;
       c1 := 0; c2 := 0;
       c3 := 0; c4 := 0;

       If lng > 0 then
       Repeat
         c1  := c1 + Byte(s[lng-3]);
         c2  := c2 + Byte(s[lng-2]) SHL 8;
         c3  := c3 + Byte(s[lng-1]);
         c4  := c4 + Byte(s[lng])  SHL 8;
         lng := lng-4;
       Until lng = 0;

       Case mdl of
       //0 : nothing left;
         1 : begin c1 := c1 + Byte(s[der]);  end;
         2 : begin c1 := c1 + Byte(s[der-1]);
                   c2 := c2 + Byte(s[der]) SHL 8;
             end;
         3 : begin c1 := c1 + Byte(s[der-2]);
                   c2 := c2 + Byte(s[der-1]) SHL 8;
                   c3 := c3 + Byte(s[der]);
             end;
       end; //case
       result := (c1+c2) + (c3+c4);
     end;
    {------------      \Local\      ------------}


var i : integer;
    aPitem:PParseItem;
begin
  try
  For i := 0 To lst.count -1 Do
  begin
    aPitem := PParseItem(lst[i]);
    With aPitem^ Do
    If kind = ikTag then
    begin
      // 1-special cases : "@" and "*"
      If Length(sValue)> 0 then
      begin
        If (sValue[1] = '@') Or (sValue[1] = '*') Or (sValue[1] = '/')
             then iValue := HLCheckSum(sValue[1])
        else if (sValue[length(sValue)] = '@')
             then iValue := HLT_AT                            //HLCheckSum('@')
      // 2-other tags
             else iValue := HLCheckSum(UpperCase(sValue));
      end;
      isSingle := IsStandAloneTag(iValue);
    end; {with...if}
  end; {for}
  lst.Pack;                                                 // applies deletions;
  // -2- Eliminates what shoudn't be there (no standalone + inline tags mix...);
  If lst.Count > 1 then
  begin
    If (PParseItem(lst.Items[0])^.kind = ikTag) And (PParseItem(lst.items[0])^.isSingle) then
    For i := 1 {0 soon checked} To lst.Count -1 Do        // Empties the list, keeping only the first item.
    begin
      aPitem := PParseItem(lst.Items[i]);
      Dispose(aPitem);
      lst[i] := Nil;
    end else
    For i := 1 {0 soon checked} To lst.Count -1 Do
    If  (PParseItem(lst.Items[i])^.kind = ikTag) And (PParseItem(lst.Items[i])^.isSingle) then
    begin                                                 // StandAlone tags no longer accepted
      aPitem := PParseItem(lst.Items[i]);
      Dispose(aPitem);
      lst[i] := Nil;
    end;
  lst.Pack;                                               // applies deletions;
  end;{With PParseItem(lst)}
  except
  end;
end;

procedure THotLogParser.TranslateTags (var lst: Tlist; var omitLNum: boolean);
// Recieves either a standalone tag, or inline ones
// Returns lst with updated, added or removed entries
// converted to their definitive values.

// Tags '*' and '@' receive a special treatement : Their original values
// (minus the brackets) are kept in sValue, and they work on the sValue
// of the preceding item('*') or of the following item. then, '@' (which
// work on absolute position) are converted lastly, whereas all others are
// converted when met.

// --- Rolf: UTF8
var
  charCount,i:   integer;
  concatenating: boolean;
  s, catVal:     string;
  aPitem:        PParseItem;
begin
  try
  // Standalone tags => overriding item[0], and adding as many items as needed
    if PParseItem(lst.items[0])^.isSingle then
    begin
      case PParseItem(lst.items[0])^.iValue of
       HLT_MEM    : GetMemoryStatus(lst);
       // --- Rolf
       HLT_RAM:     GetRamOrHeapAsPitem(nil,HL_RAMMONITOR,0,0,lst);            // ram, full info
       HLT_HEAP:    GetRamOrHeapAsPitem(nil,HL_HEAPMONITOR,0,0,lst);           // heap, full info
       HLT_DISK:    GetRamOrHeapAsPitem(nil,HL_DSKMONITOR,0,0,lst);            // disk, full info
       HLT_CPUI:    GetCPUInfoAsPitem(lst);
       HLT_OSVI:    GetOSVIAsPitem(lst);
       HLT_TIMERS:  GetTimerResolutionsAsPitem(lst);
       HLT_RULER:   GetRuler(lst,false);                                       // ruler
       HLT_RULER1:  GetRuler(lst,true);                                        // ruler+
       HLT_APP_PRM: GetParams(lst);
      end;{case}
    end
    else
  // InLine tags
    begin
      charCount:=0;                                                            // current length of future line
      for i:=0 to lst.Count-1 do
      begin
        if PParseItem(lst[i])^.kind=ikTag then
          case PParseItem(lst[i])^.iValue of
            HLT_PAD:      begin  {*}
                            // --- Rolf
                            PParseItem(lst[i])^.sValue:=
                              PadString('',PParseItem(lst[i])^.sValue,charCount);
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
//                          if (i=0) or (lst[i-1]=nil) then
//                            PParseItem(lst[i])^.sValue:=
//                              PadString('',PParseItem(lst[i])^.sValue,byteCount)
//                          else
//                          begin
//                            PParseItem(lst[i-1])^.sValue:=
//                              PadString(PParseItem(lst[i-1])^.sValue,
//                              PParseItem(lst[i])^.sValue, byteCount);
//                            aPitem := PParseItem(lst[i]);                    // item no longer needed => free it.
//                            Dispose(aPitem);
//                            lst[i] := Nil;
//                          end;
                          end;{case '*'}

            // --- Rolf: maintain charCount here!
            HLT_AT:       begin  {@}                                           // will be translated later. PlaceHolder,
                            s:=PadString('',PParseItem(lst[i])^.sValue,charCount);
                            inc(charCount,UTF8Length(s));
                          end;
            HLT_DHG:      begin
                            PParseItem(lst[i])^.sValue:=
                              DateTimeToStr(Now)+
                                IntToStr({$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF});
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_DTE:      begin
                            PParseItem(lst[i])^.sValue:=DateTimeToStr(Now);
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_GTC:      begin
                            PParseItem(lst[i])^.sValue:=
                              IntToStr({$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF});
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_HMS:      begin
                            PParseItem(lst[i])^.sValue:=DateTimeToStr(Now);
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_NOW:      begin
                            PParseItem(lst[i])^.sValue:=DateTimeToStr(Now);
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_LNUM:     begin
                            PParseItem(lst[i])^.sValue:=IntToStr(self.FpvLineCount+1);
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_CRLF:     begin
                            PParseItem(lst[i])^.sValue:='';
                            charCount:=0;
                          end;
            HLT_RAM1:     begin                                                // Ram-
                            GetRamOrHeapAsPitem(nil,HL_RAMMONITOR,1,i,lst);    // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_RAM2:     begin                                                // Ram--
                            GetRamOrHeapAsPitem(nil,HL_RAMMONITOR,2,i,lst);    // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_RAM3:     begin                                                // Ram---
                            GetRamOrHeapAsPitem(nil,HL_RAMMONITOR,3,i,lst);    // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_HEAP1:    begin                                                // Heap-
                            GetRamOrHeapAsPitem(nil,HL_HEAPMONITOR,1,i,lst);   // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_HEAP2:    begin                                                // heap--
                            GetRamOrHeapAsPitem(nil,HL_HEAPMONITOR,2,i,lst);   // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_HEAP3:    begin                                                // heap---
                            GetRamOrHeapAsPitem(nil,HL_HEAPMONITOR,3,i,lst);   // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_DISK1:    begin                                                // Disk-
                            GetRamOrHeapAsPitem(nil,HL_DSKMONITOR,1,i,lst);    // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_DISK2:    begin                                                // Disk--
                            GetRamOrHeapAsPitem(nil,HL_DSKMONITOR,2,i,lst);    // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_DISK3:    begin                                                // Disk---
                            GetRamOrHeapAsPitem(nil,HL_DSKMONITOR,3,i,lst);    // --- Rolf
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                  end;
            HLT_LNUMOFF:  begin
                            omitLNum:=true;
                            aPitem:=PParseItem(lst[i]);
                            Dispose(aPitem);
                            lst[i]:=nil;
                          end;
            HLT_APP_LFN:  begin
                            PParseItem(lst[i])^.sValue:=Paramstr(0);
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_APP_VER:  begin
                            PParseItem(lst[i])^.sValue:=GetVersionAsText;
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
// --- Rolf
//          HLT_APP_PRM:  begin                                                // app_prm, one line by parameter, line counting is loosed
//                          PParseItem(lst[i])^.sValue:=GetParams(false);
//                          charCount:=0;
//                        end;
            APP_PRM_LN:   begin                                                // app_prm- one line only, whatever be it's length
                            // --- Rolf
                            PParseItem(lst[i])^.sValue:=GetParams;
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_APP_NAME: begin
                            PParseItem(lst[i])^.sValue:=ExtractFileName(Paramstr(0));
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            HLT_APP_PATH: begin
                            PParseItem(lst[i])^.sValue:=ExtractFilePath(Paramstr(0));
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
            else          begin
                            PParseItem(lst[i])^.sValue:='#HLTag('+
                              PParseItem(lst[i])^.sValue + ')';
                            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
                          end;
           end{case}
        else
          inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));               // takes strings lengths into account
      end;{for}
      lst.Pack;                                                                // because of '*' deletions.

      // 2° loop : takes "&" block markers into account :
      concatenating:=false;
      catVal:='';
      for i:=0 to lst.Count-1 do
      If concatenating then
      begin
      // -1- search for a closing "&" or stores the value
        if (PParseItem(lst[i])^.kind=ikTag) and
           (PParseItem(lst[i])^.iValue=HLT_AND) then
        begin
          concatenating:=false;
          PParseItem(lst[i])^.kind:=ikStr;                                     // item will now be used to store the cat string
          PParseItem(lst[i])^.sValue:=catVal;
          catVal:='';
        end
        else
        begin
          catVal:=catVal+PParseItem(lst[i])^.sValue;
          aPitem:=PParseItem(lst[i]);                                          // item no longer needed => free.
          Dispose(aPitem);
          lst[i]:=nil;
        end;
      end
      else
        if (PParseItem(lst[i])^.kind=ikTag) and
           (PParseItem(lst[i])^.iValue=HLT_AND) then
        begin                                                                  // not yet concatenating -> block open marker
          concatenating:=true;
          aPitem:=PParseItem(lst[i]);                                          // "&" tag no longer needed => free its item.
          Dispose(aPitem);
          lst[i]:=nil;
        end;{If cat and ! cat}
      if concatenating then                                                    // we're done, but still "concatenating" ...
      begin                                                                    // ... -> closing {&} is missing, we will ...
        New(aPitem);                                                           // ... 'emulate' it, forcing the closing.
        aPitem^.kind:=ikStr;
        aPitem^.sValue:=catVal;
        aPitem^.isSingle:=false;
        catVal := '';
        lst.Add(aPitem);
      end;
      lst.Pack;

      // 3° loop, for '@' tags (64);
      charCount:=0;
      for i:=0 to lst.Count-2 do                                               // -1 -> nothing to do (and... index array out of bounds with lst[i+1])
        if (PParseItem(lst[i])^.kind=ikTag) and
           (PParseItem(lst[i])^.iValue=HLT_AT) then
        begin
          PParseItem(lst[i+1])^.sValue:=
            PadString(PParseItem(lst[i+1])^.sValue,PParseItem(lst[i])^.sValue,charCount);
          aPitem:=PParseItem(lst[i]);                                          // item no longer needed => free.
          Dispose(aPitem);
          lst[i]:=nil;
        end
        else
          if (PParseItem(lst[i])^.kind=ikTag) and
             (PParseItem(lst[i])^.iValue=HLT_CRLF) then                        // crlf    295
           charCount:=0
          else
            inc(charCount,UTF8Length(PParseItem(lst[i])^.sValue));
      // Checks if last entry is an '@'.If yes, removes it.
      // (care for index out of bounds :
      if (lst.Count>0) and
         (PParseItem(lst[lst.Count-1])^.kind=ikTag) and
         (PParseItem(lst[lst.Count-1])^.iValue=HLT_AT ) then
      begin
        aPitem:=PParseItem(lst[lst.Count-1]);
        Dispose(aPitem);
        lst[lst.Count-1]:=nil;
      end;
      lst.Pack;
      s:='';
      for i:=0 to lst.Count-1 do
      begin
        if (PParseItem(lst[i])^.kind=ikTag) and
           (PParseItem(lst[i])^.iValue=HLT_CRLF) then                          // crlf
        begin
          s := s + PParseItem(lst[i])^.sValue;
          PParseItem(lst[i])^.sValue := s;
          PParseItem(lst[i])^.kind   := ikStr;
          PParseItem(lst[i])^.isSingle := false;
          s := '';
        end
        else
        begin
          s := s + PParseItem(lst[i])^.sValue;
          aPitem := PParseItem(lst[i]);
          Dispose(aPitem);
          lst[i] := Nil;
        end;
      end;
      new(aPitem);
      aPitem^.kind:=ikStr;
      aPitem^.sValue:=s;
      aPitem^.isSingle:=false;
      lst.Add(aPitem);
    end;{with}
  finally
    lst.Pack;
  end;
end;

function THotLogParser.PadString (src, tag: string; posPtr: integer): string;
// posPtr reflects the actual position in bytes in the line being build.
// --- Rolf: UTF8 for Lazarus
var
  iTagVal, iBefore, needed:   integer;
  bAlLeft, bCenter, bAlRight: boolean;
  sTagVal:                    string;
{$IFDEF FPC}
  padChar:                    TUTF8Char;
{$ELSE}
  padChar:                    string;
{$ENDIF}
begin
  Result  := src;                                                    // in case invalid tag;
  sTagVal := ' ';

  try{except}
    // "*" : Pad src with char "padChar", up to nnn;
    If (tag[1] = '*') then
    begin
      padChar := UTF8Copy(tag,UTF8Length(tag),1);
      sTagVal := UTF8LeftStr(tag,UTF8Length(tag)-1);
      sTagVal := UTF8RightStr(sTagVal,UTF8Length(sTagVal)-1);
      iTagVal := StrToInt(sTagVal);
      needed  := iTagVal - UTF8Length(src) - posPtr;
      Result  := src + UTF8StringOfChar(padChar,needed);
    end

    // "@"  : Alignment
    // "What FOLLOWS the tag wil start at position "nnn", if possible.
    // "If possible" means that if "what follows" actually starts at a position
    // greater than the wanted one, the tag is ignored (but a leading space is
    // added to the new string, in order to distinguish it from the preceding one);
    else begin
      bAlLeft  := (tag[1]   = '@');
      bAlRight := (tag[length(tag)] = '@');
      bCenter  :=  bAlLeft And bAlRight;

      If bCenter then
      begin
        sTagVal := UTF8LeftStr(tag,UTF8Length(tag)-1);
        sTagVal := UTF8RightStr(sTagVal,UTF8Length(sTagVal)-1);
        iTagVal := StrToInt(sTagVal);
        needed  := iTagVal - UTF8Length(src);
        iBefore := needed SHR 1;                                     // ie Length(src) div 2
        If needed <= 0 then result := src
        else Result := StringOfChar(' ', iBefore) + src
                     + StringOfChar(' ', iBefore  + (needed Mod 2)); // Adds 1 if odds;
      end else
      begin
        If bAlLeft then                                              // value starts at pos n -> spaces first, the value;
        begin
          iTagVal := StrToInt(UTF8RightStr(tag,UTF8Length(tag)-1));
          needed  := iTagVal - posPtr -1;                            // current position
        end else
        begin                                                        // value ends at pos n;
          iTagVal := StrToInt(UTF8LeftStr(tag,UTF8Length(tag)-1));
          needed  := iTagVal - (posPtr + UTF8Length(src));
        end;
        If needed <= 0
           then result := ' ' + src
           else result  := StringOfChar(' ', needed) + src ;
      end;{left or right}
    end; {@}
  except
    result := '#HLTag(' + tag +') '+ result; end;
end;

procedure THotLogParser.FreeParsedList(var l:TList);
var i:integer;
    aPitem:PParseItem;
begin
  try
    For i := 0 To l.Count -1 Do
    begin
      aPitem := PParseItem(l[i]);
      Dispose(aPitem);
      l[i] := Nil;
    end;
  except
  end;
end;

// --- Rolf
function THotLogParser.GetParams: string;
var
//  splitter: string;
  i:        integer;
begin
//  if singleLine then
//    splitter:=', '
//  else
//  begin
//    splitter:=#13#10;
//     if Self.FshowLineNum then
//       for i:=0 to Self.FpvLnNumLen do                // to len instead of "-1", ...
//         splitter:=splitter+' ';                      // ...to add the needed extra space
//        end;
  if ParamCount<1 then
    Result:='(No parameters)'
  else
  begin
    Result:=ParamStr(1);
    for i:=2 to ParamCount do
      Result:=Result+', '+ParamStr(i);
//    Result:=Result+splitter+ParamStr(i);
  end;
end;

// --- Rolf
function THotLogParser.GetParams  (var lst: TList): string;
var
  aPitem: PParseItem;
  sl:     TStringList;
  i:      integer;
begin
  Result:='';
  sl:=TStringList.Create;
  try
    sl.Add('');
    sl.Add(  '==================================  Parameter  ====================================');
    if ParamCount<=0 then
      sl.Add('(No parameters)')
    else
      for i:=1 to ParamCount do
      begin
        sl.Add(''''+ParamStr(i)+'''');
        if Result='' then
          Result:=ParamStr(i)
        else
          Result:=Result+#13#10+ParamStr(i);
      end;
    sl.Add(  '===================================================================================');
    sl.Add('');

    for i:=0 to sl.Count-1 do
    begin
      if i=0 then
        aPitem:=PParseItem(lst[0])
      else
        New(aPitem);
      aPitem^.kind:=ikStr;
      aPitem^.sValue:=sl.Strings[i];
      aPitem^.isSingle:=false;
      if i>0 then
        lst.Add(aPitem);
    end;
    FreeAndNil(sl);
  except
    if Assigned(sl) then
      sl.Free;
  end;
end;

function THotLogParser.GetVersionAsText: string;
var
  nVer: TInt4Array;
begin
  nVer   := CurrentNumVer;
  Result := IntToStr(nVer[0]) + '.' + IntToStr(nVer[1]) + '.'
          + IntToStr(nVer[2]) + '.' + IntToStr(nVer[3]);
end;

function THotLogParser.CurrentNumVer: TInt4Array;
// Retourne les N° de version majeur et mineur de l'appli, le N° de révision, et
// le Build ;
var
  vInfoSize,
  aDword,
  vValueSize: dword;
  vInfo:      pointer;
  vValue:     PVSFixedFileInfo;
begin
  ZeroMemory(@Result,SizeOf(Result));
  aDword:=0;
  vValueSize:=0;
  vValue:=nil;
  try
    vInfoSize:=GetFileVersionInfoSizeW(pWideChar(UTF8Decode(ParamStr(0))),aDword);
    if vInfoSize=0 then
      exit;

    GetMem(vInfo,vInfoSize);
    GetFileVersionInfoW(pWideChar(UTF8Decode(ParamStr(0))),0,vInfoSize,vInfo);
    VerQueryValueW(vInfo,'\',pointer(vValue),vValueSize);
    Result[0]:=vValue^.dwFileVersionMS shr 16;
    Result[1]:=vValue^.dwFileVersionMS and $FFFF;
    Result[2]:=vValue^.dwFileVersionLS shr 16;
    Result[3]:=vValue^.dwFileVersionLS and $FFFF ;
    FreeMem(vInfo,vInfoSize);
  except
    Result[0]:=-1;
  end;
end;

// --- Rolf
procedure THotLogParser.GetRamOrHeapAsPitem
  (PHeapRec: PHeapMRec; RamOrHeap, outFmt, ptr: integer; var lst: Tlist);
//procedure THotLogParser.GetRamOrHeapAsPitem (RamOrHeap, HeapRec, outFmt, ptr: integer; var hLst: Tlist);

// ramOrHeap : HL_HEAPMONITOR or HL_RAMMONITOR
// Receives both the list we're working on, and the index of the element to be
// filled. The index will be used to add a single line for all "single line
// output" versions, the whole list itself will be used (expanded) to return
// all heap monitor full formated values.

var
  sl:     TStringList;                                // will be created by GetHeap(); but freed here
  aPitem: PParseItem;
  i:      integer;
begin
  case ramOrHeap of
    HL_HEAPMONITOR : sl := GetHeap(PHeapRec,outFmt);
    HL_RAMMONITOR  : sl := GetRam (PHeapRec,outFmt);
    else             sl := GetDisk(outFmt);           // HL_DSKMONITOR
  end;{case}

  // -1- The first entry, that is needed any case (extended or inline) :
  PParseItem(lst[ptr])^.kind     := ikStr;
  PParseItem(lst[ptr])^.sValue   := sl.Strings[0];
  PParseItem(lst[ptr])^.isSingle := false;
  try
  // -2- The rest if Standalone tag used
    for i :=1 to sl.Count-1 do                        // "0" soon managed
    begin
      new(aPitem);
      aPitem^.kind     := ikStr;
      aPitem^.sValue   := sl.Strings[i];
      aPitem^.isSingle := false;
      lst.Add(aPitem);
    end;
    sl.Free;
  except
     try
       new(aPitem);
       aPitem^.kind   := ikStr;
       aPitem^.sValue := '#HLMem';
       lst.Add(aPitem);
    except
    end;
  end;
end;

// --- Rolf
function THotLogParser.GetHeap (PHeapRec: PHeapMRec; outFmt: integer): TStringList;
//function THotLogParser.GetHeap (HeapRec,outFmt: integer): TStringList;

// May be called either for formatting "direct" values, or for formatting
// previously stored ones.
// outFmt values: 0=Heap, 1=Heap-, 2=Heap--, 3=Heap---
// If PHeapRec <> nil => to be used. Otherwise => heapMonitor table

var
{$IFDEF FPC}
  mhs, mhu,
  chs, chu, chf:       PtrUInt;
{$ELSE}
  asbc, ambc, albc:    cardinal;
  tasbs, tambs, talbs,
  rsbas, rmbas, rlbas: NativeUInt;
{$ENDIF}
  sEx:                 string;
  hr:                  THeapMRec;
begin
  Result:=TStringList.Create;
  try
    // utilise heapRec transtypé si alimenté, sinon GetFirstEntry
    if PHeapRec<>nil then
    begin
{$IFDEF FPC}
      mhs := PHeapRec^.MaxHeapSize;
      mhu := PHeapRec^.MaxHeapUsed;
      chs := PHeapRec^.CurrHeapSize;
      chu := PHeapRec^.CurrHeapUsed;
      chf := PHeapRec^.CurrHeapFree;
{$ELSE}
      asbc  := PHeapRec^.AllocatedSmallBlockCount;
      tasbs := PHeapRec^.TotalAllocatedSmallBlockSize;
      rsbas := PHeapRec^.ReservedSmallBlockAddressSpace;
      ambc  := PHeapRec^.AllocatedMediumBlockCount;
      tambs := PHeapRec^.TotalAllocatedMediumBlockSize;
      rmbas := PHeapRec^.ReservedMediumBlockAddressSpace;
      albc  := PHeapRec^.AllocatedLargeBlockCount;
      talbs := PHeapRec^.TotalAllocatedLargeBlockSize;
      rlbas := PHeapRec^.ReservedLargeBlockAddressSpace;
{$ENDIF}
      sEx   := PHeapRec^.Extra;
    end
    else
    begin
      hr:=Self.HMonitor.GetFirstEntry;
{$IFDEF FPC}
      mhs := hr.MaxHeapSize;
      mhu := hr.MaxHeapUsed;
      chs := hr.CurrHeapSize;
      chu := hr.CurrHeapUsed;
      chf := hr.CurrHeapFree;
{$ELSE}
      asbc  := hr.AllocatedSmallBlockCount;
      tasbs := hr.TotalAllocatedSmallBlockSize;
      rsbas := hr.ReservedSmallBlockAddressSpace;
      ambc  := hr.AllocatedMediumBlockCount;
      tambs := hr.TotalAllocatedMediumBlockSize;
      rmbas := hr.ReservedMediumBlockAddressSpace;
      albc  := hr.AllocatedLargeBlockCount;
      talbs := hr.TotalAllocatedLargeBlockSize;
      rlbas := hr.ReservedLargeBlockAddressSpace;
{$ENDIF}
      sEx   := hr.Extra;
    end;

    case outFmt of
      0 : begin
            if sEx<>'' then
              sEx:=' '+sEx;
            Result.Add('');
            Result.Add  (       '===========  Heap Monitor  ============'+sEx);
{$IFDEF FPC}
            if mhs<10*mb then
              Result.Add(Format('  Maximum heap size        : %.0n',[mhs/1]))
            else
              Result.Add(Format('  Maximum heap size        : %.0n MB',[mhs/mb]));
            if mhu<10*mb then
              Result.Add(Format('  Maximum used heap size   : %.0n',[mhu/1]))
            else
              Result.Add(Format('  Maximum used heap size   : %.0n MB',[mhu/mb]));
            if chs<10*mb then
              Result.Add(Format('  Current heap size        : %.0n',[chs/1]))
            else
              Result.Add(Format('  Current heap size        : %.0n MB',[chs/mb]));
            if chu<10*mb then
              Result.Add(Format('  Currently used heap size : %.0n',[chu/1]))
            else
              Result.Add(Format('  Currently used heap size : %.0n MB',[chu/mb]));
            if chf<10*mb then
              Result.Add(Format('  Currently free on heap   : %.0n',[chf/1]))
            else
              Result.Add(Format('  Currently free on heap   : %.0n MB',[chf/mb]));
{$ELSE}
            Result.Add(       '  Small Blocks:');
            Result.Add(Format('   AllocatedBlockCount       : %.0n',[asbc/1]));
            Result.Add(Format('   TotalAllocatedBlockSize   : %.0n MB',[tasbs/mb]));
            Result.Add(Format('   ReservedBlockAddressSpace : %.0n MB',[rsbas/mb]));
            Result.Add('');
            Result.Add(       '  Medium Blocks:');
            Result.Add(Format('   AllocatedBlockCount       : %.0n',[ambc/1]));
            Result.Add(Format('   TotalAllocatedBlockSize   : %.0n MB',[tambs/mb]));
            Result.Add(Format('   ReservedBlockAddressSpace : %.0n MB',[rmbas/mb]));
            Result.Add('');
            Result.Add(       '  Large Blocks:');
            Result.Add(Format('   AllocatedBlockCount       : %.0n',[albc/1]));
            Result.Add(Format('   TotalAllocatedBlockSize   : %.0n MB',[talbs/mb]));
            Result.Add(Format('   ReservedBlockAddressSpace : %.0n MB',[rlbas/mb]));
{$ENDIF}
            Result.Add('=======================================');
            Result.Add('');
          end;
      1 : begin
            if sEx<>'' then
              sEx:=', '+sEx;
{$IFDEF FPC}
            Result.Add(Format('[Heap monitor]: Maximum heap size=%.0n, Maximum used heap size=%.0n, Current heap size=%.0n, Currently used heap size=%.0n, Currently free on heap=%.0n%s',
                              [mhs/1,mhu/1,chs/1,chu/1,chf/1,sEx]));
{$ELSE}
            Result.Add(Format('[Heap monitor]: Small Blocks=%.0n, Size=%.0n MB, Reserved=%.0n MB, Medium Blocks=%.0n, Size=%.0n MB, Reserved=%.0n MB, Large Blocks=%.0n, Size=%.0n MB, Reserved=%.0n MB%s',
                              [asbc/1,tasbs/mb,rsbas/mb,ambc/1,tambs/mb,rmbas/mb,albc/1,talbs/mb,rlbas/mb,sEx]));
{$ENDIF}
          end;
      2 : begin
            if sEx<>'' then
              sEx:=', '+sEx;
{$IFDEF FPC}
            Result.Add(Format('[Heap monitor]: (%.0n, %.0n, %.0n, %.0n, %.0n%s)',
                              [mhs/1,mhu/1,chs/1,chu/1,chf/1,sEx]));
{$ELSE}
            Result.Add(Format('[Heap monitor]: (Small Blocks: %.0n, %.0n MB, %.0n MB, Medium Blocks: %.0n, %.0n, %.0n, Large Blocks: %.0n, %.0n, %.0n%s)',
                              [asbc/1,tasbs/mb,rsbas/mb,ambc/1,tambs/mb,rmbas/mb,albc/1,talbs/mb,rlbas/mb,sEx]));
{$ENDIF}
          end;
      3 : begin
            if sEx<>'' then
              sEx:=', '+sEx;
{$IFDEF FPC}
            Result.Add(Format('(%.0n, %.0n, %.0n, %.0n, %.0n%s)',
                              [mhs/1,mhu/1,chs/1,chu/1,chf/1,sEx]));
{$ELSE}
            Result.Add(Format('((%.0n, %.0n MB, %.0n MB), (%.0n, %.0n MB, %.0n MB), (%.0n, %.0n MB, %.0n MB)%s)',
                              [asbc/1,tasbs/mb,rsbas/mb,ambc/1,tambs/mb,rmbas/mb,albc/1,talbs/mb,rlbas/mb,sEx]));
{$ENDIF}
          end;
    end;{case}
  except
    result.Add('#HLHeap');
  end;
end;

// --- Rolf
function THotLogParser.GetRam (PHeapRec: PHeapMRec; outFmt: integer): TStringList;
//function THotLogParser.GetRam (HeapRec, outFmt : integer): TStringList;

// outFmt values: 0=Ram, 1=Ram-, 2=Ram--, 3=Ram---
// If HeapRec <> nil => to be used. Otherwise => heapMonitor table

var
  ML:      cardinal;
  TP,AP,
  TPF,APF,
  TV,AV:   UInt64;
  s:       string;
  hr:      THeapMRec;
begin
  Result:=TStringList.Create;
  try
    if PHeapRec<>nil then
    begin
      ML  := PHeapRec^.MemoryLoad;
      TP  := PHeapRec^.TotalPhysical;
      AP  := PHeapRec^.AvailPhysical;
      TPF := PHeapRec^.TotalPageFile;
      APF := PHeapRec^.AvailPageFile;
      TV  := PHeapRec^.TotalVirtual;
      AV  := PHeapRec^.AvailVirtual;
      s   := PHeapRec^.Extra;
    end
    else
    begin
      hr  := Self.RMonitor.GetFirstEntry;
      ML  := hr.MemoryLoad;
      TP  := hr.TotalPhysical;
      AP  := hr.AvailPhysical;
      TPF := hr.TotalPageFile;
      APF := hr.AvailPageFile;
      TV  := hr.TotalVirtual;
      AV  := hr.AvailVirtual;
      s   := hr.Extra;
    end;

    case outFmt of
      0 : begin
            if s<>'' then
              s:=' '+s;
            result.Add('');
            result.Add(       '============  RAM Monitor  ============'+s);
            result.Add(Format('  Total Physical      : %.0n MB',[TP/mb]));
            result.Add(Format('  Available Physical  : %.0n MB',[AP/mb]));
            result.Add(Format('  Total Page File     : %.0n MB',[TPF/mb]));
            result.Add(Format('  Available Page File : %.0n MB',[APF/mb]));
            result.Add(Format('  Total Virtual       : %.0n MB',[TV/mb]));
            result.Add(Format('  Availabe Virtual    : %.0n MB',[AV/mb]));
            result.Add(Format('================(%2d%%)==================',[ML]));
            result.Add('');
          end;
      1 : begin
            if s<>'' then
              s:=', '+s;
            result.Add(Format('[RAM  monitor]: Total physical=%.0n MB, Available=%.0n MB, Total Page File=%.0n MB, Available=%.0n MB, Total Virtual=%.0n MB, Available=%.0n MB, Load=%2d%%%s',
                              [TP/mb,AP/mb,TPF/mb,APF/mb,TV/mb,AV/mb,ML,s]));
          end;
      2 : begin
            if s<>'' then
              s:=', '+s;
            result.Add(Format('[RAM  monitor]: (Physical %.0n MB, %.0n MB, Page File: %.0n MB, %.0n MB, Virtual: %.0n MB, %.0n MB, %2d%%%s)',
                              [TP/mb,AP/mb,TPF/mb,APF/mb,TV/mb,AV/mb,ML,s]));
          end;
      3 : begin
            if s<>'' then
              s:=', '+s;
            result.Add(Format('((%.0n MB, %.0n MB), (%.0n MB, %.0n MB), (%.0n MB, %.0n MB), %2d%%%s)',
                              [TP/mb,AP/mb,TPF/mb,APF/mb,TV/mb,AV/mb,ML,s]));
          end;
    end;{case}
  except
    result.Add('#HLRam');
  end;
end;

// --- Rolf
procedure GetDriveLetters (AList: TStrings);
var
  vDrivesSize: cardinal;
  vDrives:     array[0..128] of char;
  vDrive:      pChar;
begin
  AList.BeginUpdate;
  try
    AList.Clear;        // clear the list from possible leftover from prior operations
    vDrivesSize:=GetLogicalDriveStrings(SizeOf(vDrives),vDrives);
    if vDrivesSize=0 then
      exit;             // no drive found, no further processing needed

    vDrive:=vDrives;
    while vDrive^<>#0 do
    begin
      AList.Add(StrPas(vDrive));
      inc(vDrive,Length(vDrive)+1);
    end;
  finally
    AList.EndUpdate;
  end;
end;

// --- Rolf
function GetVolumeLabel (DriveChar: char): string;
var
  NotUsed:            DWORD;
  VolumeFlags:        DWORD;
  VolumeSerialNumber: DWORD;
  Buf:                array [0..MAX_PATH] of char;
begin
  Result:='';
  NotUsed:=0;
  VolumeFlags:=0;
  GetVolumeInformation(pChar(DriveChar + ':\'),
                       Buf{%H-},
                       {$IFDEF FPC}{%H-}{$ENDIF}SizeOf(Buf),
                       @VolumeSerialNumber,
                       NotUsed,
                       VolumeFlags,
                       nil,
                       0);
  SetString(Result,Buf,StrLen(Buf));
end;


// --- Rolf
function THotLogParser.GetDisk (outFmt: integer): TStringList;
// outFmt values: 0=disk, 1=disk-, 2=disk--, 3=disc---
// Unlike GetRam & GetHeap, GetDisk only uses directly read values,
// and provides no way to link a message to it's first line.

var
{$IFDEF FPC}
  bytesA,bytesT,bytesF: ULARGE_INTEGER;
{$ELSE}
  bytesA,bytesT,bytesF: int64;
{$ENDIF}
  dl: TStringList;
  i:  integer;
begin
  Result:=TStringList.Create;
{$IFDEF FPC}
  bytesA.QuadPart:=0;
  bytesT.QuadPart:=0;
  bytesF.QuadPart:=0;
{$ENDIF}

  dl:=TStringList.Create;
  GetDriveLetters(dl);

  if dl.Count<=0 then
  begin
    case outFmt of
      0:    begin
              Result.Add('');
              Result.Add('============  Disk Monitor  ===========');
              Result.Add('  No disks found');
              Result.Add('=======================================');
              Result.Add('');
            end;
      1, 2: Result.Add('[Disk monitor]: No disks found');
      3:    Result.Add('No disks found');
    end;
    dl.Free;
    exit;
  end;

  try
    case outFmt of
      0: begin
           Result.Add('');
           Result.Add           ('============  Disk Monitor  ===========');
           for i:=0 to dl.Count-1 do
             if GetDiskFreeSpaceEx(pChar(dl[i]),bytesA,bytesT,@bytesF) then    // bytesF shall be PLargeInteger !
             begin
               Result.Add(Format('Volume: %s  %s',[Copy(dl[i],1,2),GetVolumeLabel(dl[i][1])]));
{$IFDEF FPC}
               Result.Add(Format('  Free for User  : %12.0n MB',[bytesA.QuadPart/mb]));
               Result.Add(Format('  Total for User : %12.0n MB',[bytesT.QuadPart/mb]));
               Result.Add(Format('  Free on Disk   : %12.0n MB',[bytesF.QuadPart/mb]));
{$ELSE}
               Result.Add(Format('  Free for User  : %12.0n MB',[bytesA/mb]));
               Result.Add(Format('  Total for User : %12.0n MB',[bytesT/mb]));
               Result.Add(Format('  Free on Disk   : %12.0n MB',[bytesF/mb]));
{$ENDIF}
             end;
           Result.Add           ('=======================================');
           Result.Add('');
         end;
      1: begin
           Result.Add('[Disk monitor]:');                                      // Will be concatinated to one line
           for i:=0 to dl.Count-1 do
             if GetDiskFreeSpaceEx(pChar(dl[i]),bytesA,bytesT,@bytesF) then    // bytesF shall be PLargeInteger !
             begin
               if Result.Count=1 then
                 Result.Add(' ')
               else
                 Result.Add(', ');
               Result.Add(Format('%s (Free-User=%.0n MB, Total-User=%.0n MB, Free-Disk=%.0n MB)',
{$IFDEF FPC}
                                 [Copy(dl[i],1,2),bytesA.QuadPart/mb,bytesT.QuadPart/mb,bytesF.QuadPart/mb]));
{$ELSE}
                                 [Copy(dl[i],1,2),bytesA/mb,bytesT/mb,bytesF/mb]));
{$ENDIF}
             end;
         end;
      2: begin
           Result.Add('[Disk monitor]:');                                      // Will be concatinated to one line
           for i:=0 to dl.Count-1 do
             if GetDiskFreeSpaceEx(pChar(dl[i]),bytesA,bytesT,@bytesF) then    // bytesF shall be PLargeInteger !
             begin
               if Result.Count=1 then
                 Result.Add(' ')
               else
                 Result.Add(', ');
               Result.Add(Format('%s (%.0n MB, %.0n MB, %.0n MB)',
{$IFDEF FPC}
                                 [Copy(dl[i],1,2),bytesA.QuadPart/mb,bytesT.QuadPart/mb,bytesF.QuadPart/mb]));
{$ELSE}
                                 [Copy(dl[i],1,2),bytesA/mb,bytesT/mb,bytesF/mb]));
{$ENDIF}
             end;
         end;
      3: begin
           for i:=0 to dl.Count-1 do
             if GetDiskFreeSpaceEx(pChar(dl[i]),bytesA,bytesT,@bytesF) then    // bytesF shall be PLargeInteger !
             begin
               if Result.Count<=0 then
                 Result.Add('(');
               if Result.Count>1 then
                 Result.Add(', ');
               Result.Add(Format('(%s %.0n MB, %.0n MB, %.0n MB)',             // Will be concatinated to one line
{$IFDEF FPC}
                                 [Copy(dl[i],1,2),bytesA.QuadPart/mb,bytesT.QuadPart/mb,bytesF.QuadPart/mb]));
{$ELSE}
                                 [Copy(dl[i],1,2),bytesA/mb,bytesT/mb,bytesF/mb]));
{$ENDIF}
             end;
           Result.Add(')');
         end;
    end; {case}
  except
    Result.Add('#HLDisk');
  end; {try}

  if Assigned(dl) then
    dl.Free;
end;

procedure THotLogParser.GetCPUInfoAsPitem (var lst: Tlist);
// Fills directly the list with CPU info values formatted;
var sl:      TStringList;
    aPitem:  PParseItem;
    i:       integer;
    SysInfo: TSystemInfo;
    sTmp,pTmp,{tTmp,}
    vendor,regSpeed : string;
begin
  sl := TStringList.Create;
  SysInfo.wProcessorArchitecture:=0;
  try
    GetSystemInfo(SysInfo);
    Case SysInfo.wProcessorArchitecture of
      0 : begin
            sTmp := 'PROCESSOR_ARCHITECTURE_INTEL';
            pTmp := '(x86 Family '  + IntToStr(SysInfo.wProcessorLevel)
                    + ' Model '     + IntToStr(Hi(SysInfo.wProcessorRevision))
                    + ' Stepping '  + IntToStr(Lo(SysInfo.wProcessorRevision))+')';
          end;
      1 :  sTmp := 'PROCESSOR_ARCHITECTURE_MIPS';
      2 :  sTmp := 'PROCESSOR_ARCHITECTURE_ALPHA';
      3 :  sTmp := 'PROCESSOR_ARCHITECTURE_PPC';
      4 :  sTmp := 'PROCESSOR_ARCHITECTURE_SHX';
      5 :  sTmp := 'PROCESSOR_ARCHITECTURE_ARM';
      6 :  sTmp := 'PROCESSOR_ARCHITECTURE_IA64';
      7 :  sTmp := 'PROCESSOR_ARCHITECTURE_ALPHA64';
      8 :  sTmp := 'PROCESSOR_ARCHITECTURE_MSIL';
      9 :  sTmp := 'PROCESSOR_ARCHITECTURE_AMD64';
      else sTmp := 'PROCESSOR_ARCHITECTURE_UNKNOWN';
    end;{case wProcessorArchitecture }
    // --- Rolf: obsolte
{    Case SysInfo.dwProcessorType Of
       86 : tTmp := 'Intel 386';
      486 : tTmp := 'Intel 486';
      586 : tTmp := 'Intel Pentium';
      601 : tTmp := 'PPC 601';
      603 : tTmp := 'PPC 603';
      604 : tTmp := 'PPC 604';
      620 : tTmp := 'PPC 620';
      860 : tTmp := 'Intel 860';
     2000 : tTmp := 'MIPS R2000';
     3000 : tTmp := 'MIPS R3000';
     4000 : tTmp := 'MIPS R4000';
    21064 : tTmp := 'ALPHA 21064';
      else  tTmp := '';
    end;} {case dwProcessorType}
  // Registry stored values
    regSpeed := GetRegValue(HKEY_LOCAL_MACHINE,
                   'Hardware\Description\System\CentralProcessor\0',
                   '~MHz');
    If regSpeed <> '' then regSpeed := regSpeed + ' MHz'
                      else regSpeed := 'n/a';
    vendor :=   GetRegValue(HKEY_LOCAL_MACHINE,
                   'Hardware\Description\System\CentralProcessor\0',
                   'VendorIdentifier');
    If vendor <> '' then vendor := IntToStr(SysInfo.dwOemId) + ' (' + vendor + ')'
                    else vendor := IntToStr(SysInfo.dwOemId);
  except
    If sTmp = '' then sTmp := 'n/a';
//    If tTmp = '' then tTmp := 'n/a';
    If regSpeed = '' then regSpeed :=  'n/a';
    If vendor = '' then vendor :=  'n/a';
  end;

  try
  // -1-  Build result as TstringList ;
  With sl, SysInfo do
  begin
    Add('');
    Add('==================================  CPU info  =====================================');
    Add('  Architecture                : ' + IntToStr(wProcessorArchitecture) + ' (' + sTmp + ')');
    Add('  OEM ID (vendor)             : ' + vendor );
    Add('  Active processor mask       : ' + IntToStr(dwActiveProcessorMask));
    Add('  Number of processors        : ' + IntToStr(dwNumberOfProcessors));
//    Add('  Processor type              : ' + IntToStr(dwProcessorType) + ' (' + tTmp + ')');
    // --- Rolf: outdated
{    Case wProcessorLevel of
      3: sTmp := ' (Intel 80386) ';
      4: sTmp := ' (Intel 80486) ';
      5: sTmp := ' (Intel Pentium) ';
    else sTmp := ' ';
    end;}
    Add('  Processor level             : ' + IntToStr(wProcessorLevel) + pTmp);
    Add('  Speed (registry)            : ' + regSpeed);
    Add('  VMem allocation granularity : ' + FloatToStr(dwAllocationGranularity / 1024) + ' KB');
    Add('  ProcessorRevision           : ' + IntToStr(wProcessorRevision));
    Add('  Page size                   : ' + IntToStr(Round(dwPageSize / 1024)) + ' KB');
    Add(Format('  Minimum application address : %p',[lpMinimumApplicationAddress]));
    Add(Format('  Maximum application address : %p',[lpMaximumApplicationAddress]));

    Add('===================================================================================');
    Add('');
  end;

  // -2- Converts results -> Tlist of PParseItem.
  //--- 1st one soon exists, override it.
    PParseItem(lst[0])^.kind    := ikStr;
    PParseItem(lst[0])^.sValue  := sl.Strings[0];
    PParseItem(lst[0])^.isSingle:= false;
  //--- Next lines. Items to be created.
    For i :=1 To sl.Count-1 Do                            // "0" soon managed
    begin
      new(aPitem);
      aPitem^.kind    := ikStr;
      aPitem^.sValue  := sl.Strings[i];
      aPitem^.isSingle:= false;
      lst.Add(aPitem);
    end;
    sl.Free;
  except
     try new(aPitem);
         aPitem^.kind    := ikStr;
         aPitem^.sValue  := '#HLCPUInfo';
         lst.Add(aPitem);
     except
     end;
  end;
end;

// --- Rolf
procedure THotLogParser.GetOSVIAsPitem (var lst: Tlist);
{$IFDEF FPC}
const
  VER_SUITE_WH_SERVER = $00008000;
{$ENDIF}
var
  OSName:       string;
  OSVI:         TOSVersionInfoExW;
  SI:           SYSTEM_INFO;
  Architecture: string;
  FullOSName:   string;
  aPitem:       PParseItem;
  sl:           TStringList;
  i:            integer;
begin
  sl:=TStringList.Create;
  SI.wProcessorArchitecture:=0;

  try
    OSVI.dwOSVersionInfoSize:=SizeOf(OSVI);
    if not GetVersionExW({$IFDEF FPC}@OSVI{$ELSE}OSVI{$ENDIF}) then
      Abort;
    OSName:='';
    case OSVI.dwMajorVersion of
      10: if OSVI.wProductType=VER_NT_WORKSTATION then
            OSname:='Windows 10'
          else
            OSName:='Windows Server 2016 Technical Preview';
       6: case OSVI.dwMinorVersion of
            3: if OSVI.wProductType=VER_NT_WORKSTATION then
                 OSName:='Windows 8.1'
               else
                 OSName:='Windows Server 2012 R2';
            2: if OSVI.wProductType=VER_NT_WORKSTATION then
                 OSName:='Windows 8'
               else
                 OSName:='Windows Server 2012';
            1: if OSVI.wProductType=VER_NT_WORKSTATION then
                 OSName:='Windows 7'
               else
                 OSName:='Windows Server 2008 R2';
            0: if OSVI.wProductType=VER_NT_WORKSTATION then
                 OSName:='Windows Vista'
               else
                 OSName:='Windows Server 2008';
          end;
       5: case OSVI.dwMinorVersion of
            2: if GetSystemMetrics(SM_SERVERR2)<>0 then
                 OSName:='Windows Server 2003 R2'
               else
                 if (OSVI.wSuiteMask and VER_SUITE_WH_SERVER)<>0 then
                   OSName:='Windows Home Server'
                 else
                   if GetSystemMetrics(SM_SERVERR2)=0 then
                     OSname:='Windows Server 2003'
                   else
                     if (OSVI.wProductType=VER_NT_WORKSTATION) and
                        (SI.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64) then
                       OSName:='Windows XP Professional x64 Edition';
            1: OSName:='Windows XP';
            0: OSName:='Windows 2000';
          end;
    end;

    FillChar(SI,SizeOf(SI),0);
    GetNativeSystemInfo({$IFDEF FPC}@SI{$ELSE}SI{$ENDIF});
    case SI.wProcessorArchitecture of
      PROCESSOR_ARCHITECTURE_AMD64: Architecture:='x64';
      PROCESSOR_ARCHITECTURE_INTEL: Architecture:='x86';
      else Architecture:='';
    end;

    FullOSName:=OSName;
    if pos(copy(Architecture,2,MaxInt),FullOSName)=0 then
      FullOSName:=FullOSName+' '+Architecture;
    if OSVI.dwMajorVersion>0 then
    begin
      FullOSName:=FullOSName+Format(' Version %d',[OSVI.dwMajorVersion]);
      if OSVI.dwMinorVersion>0 then
        FullOSName:=FullOSName+Format('.%d',[OSVI.dwMinorVersion]);
    end;
    if OSVI.wServicePackMajor>0 then
    begin
      FullOSName:=FullOSName+Format(' Service Pack %d',[OSVI.wServicePackMajor]);
      if OSVI.wServicePackMinor>0 then
        FullOSName:=FullOSName+Format('.%d',[OSVI.wServicePackMinor]);
    end;
    if OSVI.dwBuildNumber>0 then
      FullOSName:=FullOSName+Format(' Build %d',[OSVI.dwBuildNumber]);

    with sl do
    begin
      Add('');
      Add(         '===============================  OS version info  =================================');
      if FullOSname<>'' then
        Add(       '  '+FullOSName);
      Add('');
      Add(Format(  '  Version      : %d.%d',[OSVI.dwMajorVersion,OSVI.dwMinorVersion]));
      Add(Format(  '  Build        : %d',   [OSVI.dwBuildNumber]));
      if OSVI.wServicePackMinor>0 then
        Add(Format('  Servicepack  : %d',[OSVI.wServicePackMajor]))
      else
        Add(Format('  Servicepack  : %d.%d',[OSVI.wServicePackMajor,OSVI.wServicePackMinor]));
      Add(Format(  '  Architecture : %s',[Architecture]));
      Add(         '===================================================================================');
      Add('');
    end;

  // -2- Converts results -> Tlist of PParseItem.
    PParseItem(lst[0])^.kind:=ikStr;
    PParseItem(lst[0])^.sValue:=sl.Strings[0];
    PParseItem(lst[0])^.isSingle:=false;
    for i:=1 to sl.Count-1 do
    begin
      new(aPitem);
      aPitem^.kind:=ikStr;
      aPitem^.sValue:=sl.Strings[i];
      aPitem^.isSingle:=false;
      lst.Add(aPitem);
    end;
    sl.Free;
  except
     try
       new(aPitem);
       aPitem^.kind:=ikStr;
       aPitem^.sValue:='#HLOSVersionInfo';
       lst.Add(aPitem);
     except
     end;
  end;
end;

procedure THotLogParser.GetTimerResolutionsAsPitem(var lst:Tlist);
// Formats local timer general measures, if available.
const HL_NS = '      n/s      ';
      HL_NA = '      n/a      ';
      sHdr1 = '============================================';
      sHdr2 = '  Timers resolution  ';
      sHdr3 = '                          |                Min resolution                 |       Api \ call overhead';
      sHdr4 = '          Timers          |-----------------------------------------------|----------------------------------';
      sHdr5 = '                          |       ms      :       µs      :      ns       |     native    :    optimal';
      sHdr6 = '--------------------------|-----------------------------------------------|----------------------------------';
// --- Rolf
//    sHdr7 = '==================';
      sLft1 = '  GetTickCount            |';
      sLft2 = '  QueryPerformanceCounter |';
      sLft3 = '  ReaDTimeStampCounter    |';
      sCmt1 = '(n/a: not available, n/s : not significant)';
      sCmt2 = 'QueryPerformanceFrequency returned : %d (native units)';
      sCmt3 = '(RDTSC min resolution from registry ; Overhead from asm call)';

var gtcIn, gtcOut: integer;         //GetTickCount
    aTimer : TQTimer;
    aTimeScale: TQTimeScale;
    aReal:real; i:integer;
    sGtcMinResMls,
    sQpcMinResMls, sQpcMinResMic, sQpcMinResNan,
    sRdtMinResMls, sRdtMinResMic, sRdtMinResNan,
    sQpcApiOvdNat, sQpcApiOvdOpt,
    sRdtApiOvdNat, sRdtApiOvdOpt: string;
    sl:TStringList;
    aPitem : PParseItem;


    {---------------  Local  ------------------}
    function FormatOutput (value: string; upTo: integer = 14): string;
    var i:integer;
    begin
      result := value + ' ';
      For i := Length(result) To upTo Do result := ' ' + result;
    end;
    {------------------------------------------}

begin
  sl := TStringList.Create;
  aTimer := TQTimer.Create;
  aTimeScale:=tsSeconds;

  try {finally}
// -1- GTC. Should be 15 to 16 ms;
  gtcIn := {$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF};
  Repeat
    gtcOut := {$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF};
  Until gtcIn <> gtcOut;
  sGtcMinResMls := '  ' + IntToStr(gtcOut-gtcIn) + ',---       ';

  try
// -2- QueryPerfCounter resolution = 1 / QueryPerfFrq(qpfFrq);
  If aTimer.FQpcFrq <= 0 then
  begin
    sQpcMinResMls := HL_NS;
    sQpcMinResMic := HL_NS;
    sQpcMinResNan := HL_NS;
    sQpcApiOvdNat := HL_NA;
    sQpcApiOvdOpt := HL_NA;
  end else
  begin
    sQpcMinResMls := FormatOutput(Format('%3.9n',[aTimer.getTimeMeasureAs(tsMilliSec,1,tkQPC)]));
    sQpcMinResMic := FormatOutput(Format('%3.9n',[aTimer.getTimeMeasureAs(tsMicroSec,1,tkQPC)]));
    sQpcMinResNan := FormatOutput(Format('%3.9n',[aTimer.getTimeMeasureAs(tsNanosSec,1,tkQPC)]));
    sQpcApiOvdNat := FormatOutput(IntToStr(aTimer.FQpcOverhead)+' units ');
    aReal := aTimer.GetOptimalMeasure(aTimer.FQpcOverhead, tkQPC, aTimeScale);
    sQpcApiOvdOpt := Format('%s (%3.9n)',
                     [TimeScale2Str[Ord(aTimeScale)], aReal ]);
  end;

// -3- ReadTimeStampCounter resolution = 1 / (registry's frq -> hrz);
  If aTimer.FRegFrq > 0 then
  begin
    sRdtMinResMls := FormatOutput(Format('%3.9n',[aTimer.getTimeMeasureAs(tsMilliSec,1,tkRDT)]));
    sRdtMinResMic := FormatOutput(Format('%3.9n',[aTimer.getTimeMeasureAs(tsMicroSec,1,tkRDT)]));
    sRdtMinResNan := FormatOutput(Format('%3.9n',[aTimer.getTimeMeasureAs(tsNanosSec,1,tkRDT)]));
    If aTimer.FRDTOverhead > 0 then
    begin
      sRdtApiOvdNat := FormatOutput(IntToStr(aTimer.FRDTOverhead)+' cycles');
      aReal := aTimer.GetOptimalMeasure(aTimer.FRDTOverhead, tkRDT, aTimeScale);
      sRdtApiOvdOpt := Format('%s (%3.9n)',
                              [TimeScale2Str[Ord(aTimeScale)], aReal ]);
    end else
    begin
      sRdtApiOvdNat := HL_NA;
      sRdtApiOvdOpt := HL_NA;
    end;
  end else
  begin
    sRdtMinResMls := HL_NA;
    sRdtMinResMic := HL_NA;
    sRdtMinResNan := HL_NA;
  end;

// -4- Résultats :
  With sl Do
  begin
    Add('');
    Add(sHdr1 + sHdr2 + sHdr1); Add(sHdr3);Add(sHdr4);Add(sHdr5);Add(sHdr6);
    Add(sLft1 + sGtcMinResMls + ':' + HL_NS + ':' + HL_NS + '|      0 ticks  : ms or s.' );
    Add(sLft2 + sQpcMinResMls + ':' + sQpcMinResMic + ':' + sQpcMinResNan + '|' + sQpcApiOvdNat + ': ' + sQpcApiOvdOpt);
    Add(sLft3 + sRdtMinResMls + ':' + sRdtMinResMic + ':' + sRdtMinResNan + '|' + sRdtApiOvdNat + ': ' + sRdtApiOvdOpt);
    // --- Rolf
    Add(StringOfChar('=',109));
//    Add(sHdr7 + sHdr7 + sHdr7 + sHdr7 + sHdr7 + sHdr7);
    Add(FormatOutput(sCmt1,107));
    Add(FormatOutput(Format(sCmt2,[aTimer.FQpcFrq]),107));
    Add(FormatOutput(sCmt3,107));
    // --- Rolf
    Add(StringOfChar('=',109));
//    Add(sHdr7 + sHdr7 + sHdr7 + sHdr7 + sHdr7 + sHdr7);
    Add('');
  end;

// -5- Converts results -> Tlist of PParseItem.
    PParseItem(lst[0])^.kind    := ikStr;
    PParseItem(lst[0])^.sValue  := sl.Strings[0];
    PParseItem(lst[0])^.isSingle:= false;
    For i :=1 To sl.Count-1 Do
    begin
      new(aPitem);
      aPitem^.kind    := ikStr;
      aPitem^.sValue  := sl.Strings[i];
      aPitem^.isSingle:= false;
      lst.Add(aPitem);
    end;
    sl.Free;
  except
    try
      new(aPitem);
      aPitem^.kind    := ikStr;
      aPitem^.sValue  := '#HLTimersResolution';
      lst.Add(aPitem);
    except
    end;
  end;
  finally aTimer.Free;
  end;
end;

procedure THotLogParser.GetMemoryStatus(var lst:TList);
var mSS: TMemoryStatusEx;
    dkFree,
    dkTotal: int64;
    sl : TStringList;
    i: integer;
    aPitem: PParseItem;
begin
  dkFree  := DiskFree(0);                                  // drive 0 = current;
  dkTotal := DiskSize(0);
  try
    mSS.dwLength := SizeOf(mSS);
    GlobalMemoryStatusEx(mSS);

    sl := TStringList.Create;
    With mSS,sl Do begin
      Add('');
      Add('=================================  Memory status  =================================');
      // --- Rolf
      Add(Format('  Memory (Total)             : %12.0n MB',[mSS.ullTotalPhys/mb]));
      Add(Format('  Memory (Available)         : %12.0n MB  => Memory load : %d%%',
                 [mSS.ullAvailPhys/mb,mSS.dwMemoryLoad]));
      Add('');
      Add(Format('  Page(Swap)File (Total)     : %12.0n MB',[ullTotalPageFile/mb]));
      Add(Format('  Page(Swap)File (Available) : %12.0n MB',[ullAvailPageFile/mb]));
      Add(Format('  Virtual Memory (Total)     : %12.0n MB',[ullTotalVirtual/mb]));
      Add(Format('  Virtual Memory (Available) : %12.0n MB',[ullAvailVirtual/mb]));
      Add('');
      Add(Format('  Current Disk (Total)       : %12.0n MB',[dkTotal/mb]));
      Add(Format('  Current disk (Available)   : %12.0n MB  => Free space  : %f%%',
                 [dkFree/mb,(dkFree/dkTotal)*100]));
      Add('===================================================================================');
      Add('');
    end;

  // -2- Converts results -> Tlist of PParseItem.
    PParseItem(lst[0])^.kind    := ikStr;
    PParseItem(lst[0])^.sValue  := sl.Strings[0];
    PParseItem(lst[0])^.isSingle:= false;
    For i :=1 To sl.Count-1 Do
    begin
      new(aPitem);
      aPitem^.kind    := ikStr;
      aPitem^.sValue  := sl.Strings[i];
      aPitem^.isSingle:= false;
      lst.Add(aPitem);
    end;
    sl.Free;
  except
     try
       new(aPitem);
       aPitem^.kind    := ikStr;
       aPitem^.sValue  := '#HLOSMemoryStatus';
       lst.Add(aPitem);
     except
     end;
  end;

end;


// 90   95  100  105  110  115  120
// |....|....|....|....|....|....|
procedure THotLogParser.GetRuler (var lst: TList; showNums: boolean = true);
//  Builds ruler lines; based upon stored definitions for ruler.
//  Returns the current lst, updated ; More usefull at design time
//  than at runTime...
var i, lmt : integer;
    rl : string;
    aPitem : PParseItem;

     {-------------  local  --------------}
      function RulerBuildNumLine (current, upTo: integer; pad: string): string;
      begin
        lmt := min(upTo,fRulerLength);
        result := '';
        while (current <= lmt) Do
        begin
          result := result + IntToStr(current) + pad;
          inc(current, 5);
        end;
      end;
     {-----------  \ local \  ------------}

begin
  try
  If Not(fRulerBuild) or (fRulerBuild
                          and  showNums
                          and (fRulerNums = '') ) then
  begin
    // Numbers
    If showNums then
    begin
      fRulerNums := '    5    ';
      fRulerNums := fRulerNums + RulerBuildNumLine(10, 95,'   ');
      If Self.fRulerLength > 95 then
      begin
        fRulerNums := Copy(fRulerNums, 1, Length(fRulerNums)-1)  + '100  ';
        fRulerNums := fRulerNums + RulerBuildNumLine(105, 995,'  ');
        If fRulerLength > 995 then
        begin
          fRulerNums := fRulerNums + '1000';
          fRulerNums := fRulerNums + RulerBuildNumLine(1005, 10000,' ');
        end;       
      end;
    end;{showNums}
    // dots & pipes
    rl := '....|....|';
    fRulerDots := '';
    For i := 1 to (fRulerLength div 10) Do
        fRulerDots :=  fRulerDots + rl;        
  fRulerBuild := true;
  end;{!fRulerBuild}
  //result
  If showNums then
  begin
    PParseItem(lst[0])^.kind   := ikStr;
    PParseItem(lst[0])^.sValue := fRulerNums;
    PParseItem(lst[0])^.isSingle := false;
    New(aPitem);
    aPitem^.kind     := ikStr;
    aPitem^.sValue   := fRulerDots;
    aPitem^.isSingle := false;
    lst.Add(aPitem);
  end else
  begin
    PParseItem(lst[0])^.kind     := ikStr;
    PParseItem(lst[0])^.sValue   := fRulerDots;
    PParseItem(lst[0])^.isSingle := false;
  end;
  except
    PParseItem(lst[0])^.kind     := ikStr;
    PParseItem(lst[0])^.sValue   := '#HLRuler(n/a)';
    PParseItem(lst[0])^.isSingle := false;
  end;    
end;



////////////////////////////////////////////////////////////////////////////////
//  U T I L S
////////////////////////////////////////////////////////////////////////////////


function THotLogParser.GetRegValue (root: HKey; key, wanted: string): string;
var Reg: TRegistry;
    i,sz:integer;
    Buf : array of Byte;
begin
  result := '';
  Reg := TRegistry.Create;
  try
    try
      Reg.RootKey := root;
      Reg.Access  := KEY_READ;
      If Reg.OpenKeyReadOnly(key) then
      begin
        Case reg.GetDataType(wanted) of
          rdString,
          rdExpandString : result := Reg.ReadString(wanted);
          rdInteger : result := IntToStr(Reg.ReadInteger(wanted));
          rdBinary,rdUnknown :
            begin
              sz := Reg.GetDataSize(wanted);
              If sz > 0 then
              begin
                SetLength(Buf,sz);
                Reg.ReadBinaryData(wanted,buf[0],sz);     
                Result:='';                               
                For i:=0 to sz-2 do                       //-2 -> ignore final '  '
                    If Buf[i]=0 then result := result + '  '
                    else result := result + Char(Buf[i]);
              end;
              Buf:=Nil;                  
              while (result[length(result)]=' ') Do
                    result := Copy(result,1,Length(result)-1);
              result := ' (' + result + ')';
            end;{rdBinary}
          else result := '';
        end;{case}
        Reg.CloseKey;
      end;{if openKey}
    except
      result := '';
    end;
  finally
    Reg.Free;
  end;
end;


{--------------------------------------------------------------------}
{---                      Threads : Writer                        ---}
{--------------------------------------------------------------------}

constructor THotLogWriter.Create;
begin
  inherited Create(true);
  Self.FDoFeedBack := false;
  Self.hlFileDef := THLFileDef.Create;
  Self.FStarted  := true;
end;

destructor THotLogWriter.Destroy;
begin
  self.hlFileDef.Free;
  inherited;
end;

// --- Rolf: Used in THotLogWriter.Execute
// TFileList.Objects[i] is a pointer to UInt64(TSeachRec.FindData.ftCreationTime)
type
  TFileList = class(TStringList)
  public
    function   AddFile (const S: string; const Time: Windows.FILETIME): integer; virtual;
    procedure  Delete  (Index: integer); override;
    destructor Destroy; override;
  end;

  function TFileList.AddFile (const S: string; const Time: Windows.FILETIME): integer;
  var
    p: PUInt64;
  begin
    New(p);
    p^:=UInt64(Time);
    Result:=AddObject(s,TObject(p));
  end;

  procedure TFileList.Delete (Index: integer);
  begin
    Dispose(PUInt64(Objects[Index]));
    inherited Delete(Index);
  end;

  destructor TFileList.Destroy;
  var
    i: integer;
  begin
    for i:=0 to Count-1 do
      Dispose(PUInt64(Objects[i]));
    inherited;
  end;

  function SortByAge (List: TStringList; Index1, Index2: Integer): Integer;
  begin
    if PUInt64(List.Objects[Index1])^ = PUInt64(List.Objects[Index2])^ then
    begin
      Result:=0;
      exit;
    end;
    if PUInt64(List.Objects[Index1])^ < PUInt64(List.Objects[Index2])^ then
    begin
      Result:=-1;
      Exit;
    end;
    Result:=1;
  end;


procedure THotLogWriter.Execute;
// Luis Gonzalo Constantini Von Rickel said :
// "I did a Little (but I think a useful) change to the HotLog, as you know the
// delphi FileCreate function creates a file with the flag fmShareExclusive flag
// on no matters the share flag that you use, that cause that when you create a
// file for the first time, no body can open the file until you close the file
// and reopend it; the i did this change you the Excute Method of the THotLogWriter
// Thread:"

var
  tmpMsg:     MSG;
  F:          TFileStreamUTF8;
{$IFDEF FPC}
  u8 :        string;        // Rolf: UTF8
{$ELSE}
  u8 :        UTF8String;    // Rolf: UTF8
{$ENDIF}
  b:          TBytes;
  uSize:      cardinal;      // Rolf: Size of F

// --- Rolf: Redesign of Gonzalo's code
  procedure OpenLogFile;
  begin
    try
      F:=TFileStreamUTF8.Create(hlFileDef.fileName,hlFileDef.OpMode);
{$IFDEF DEBUG}
      DbgSendMsg('HotLog THLWE01: fileName: %s',[hlFileDef.fileName]);
{$ENDIF}

      if hlFileDef.append then
      begin
{$IFDEF FPC}
        //append and uSize
        uSize := F.Seek(0, soEnd);
{$ELSE}
{$IFDEF VER130}
        F.Seek(0, soFromEnd);                         // Modification DELPHI5
        uSize := F.Position;
{$ELSE}
        uSize := F.Seek(0, soEnd);
{$ENDIF}
{$ENDIF}
      end
      else
      begin
        F.Size:=0;
        //UFF8 Encoding and uSize
        b:=TEncoding.UTF8.GetPreamble;
        F.WriteBuffer(b[0],Length(b));
        uSize:=Length(b);
      end;
    except
      on E: Exception do
      begin
        DbgSendMsg('HotLog THLWE02: %s; %s (x%.8x)',
                   [E.Message,SysErrorMessageUTF8(GetLastError),GetLastError]);
        FStarted := false;
        Terminate;
        Exit;                                         // File can't be used;
      end;
    end;
  end;

  procedure SwitchLogFile;
  var
    l:          longint;
    TempGdgMax: word;          // rbecker
    SR:         TSearchRec;    // Rolf: Delete unwanted old log files
    FList:      TFileList;     // Rolf: Delete unwanted old log files
  begin
    FreeAndNil(F);                                    // close the current log file
    if hlFileDef.UseSafeFilenames then
      hlFileDef.BuildSafeFilename
    else
    begin
      while hlFileDef.FileInUse(hlFileDef.fileName) do
        Sleep(50);                                   // let everything flush to disk (we hope!) before renaming files
      TempGdgMax := hlFileDef.GdgMax;                // calling SetExt below resets GdgMax to 0!
      hlFileDef.SetExt(ExtractFileExt(hlFileDef.BuildFileName));
      hlFileDef.GdgMax := TempGdgMax;
    end;

    hlFileDef.Append:=False;
    OpenLogFile;

    (* Now see if there are any old log files we need to delete.
       This only needs to be done with safe file names as the original code
       to manage generations works just fine for those files *)
    if hlFileDef.UseSafeFilenames and (hlFileDef.SafeGdgMax>0) then
    begin
      FList := TFileList.Create;
      try
        // --- Rolf
        // SList.Sorted := true;
        with hlFileDef do
          if FSafePrefix = '*' then
            l := FindFirstUTF8(path + '*' + ext, faAnyfile, SR)
          else
            l := FindFirstUTF8(path + FSafePrefix+'-*' + ext, faAnyfile, SR);
        if l = 0 then
          repeat
            FList.AddFile(SR.Name,SR.FindData.ftCreationTime);
          until FindNextUTF8(SR) <> 0;
        FindCloseUTF8(SR);

        FList.CustomSort(@SortByAge);

        while FList.Count > hlFileDef.SafeGdgMax do
        begin
          // --- Rolf
          DeleteFileUTF8(hlFileDef.path + FList[0]);
          FList.Delete(0);
        end;
        // --- Rolf
      except
        FindCloseUTF8(SR);
      end;
      FreeAndNil(FList);
    end;
  end;

begin
  // --- Rolf
  OpenLogFile;

  tmpMsg.Message:=0;
  try {finally}
    try {except}
      PeekMessage(tmpMsg,0,0,0,PM_NOREMOVE);          // Rolf: Force establishing the message queue
      FExecuting := true;                             // Sergueï
      tmpMsg.Message:=0;
// --- Rolf
      while GetMessage(tmpMsg,0,0,0) do
      begin
        if Terminated then
          exit;
// --- Rolf
// Check if we need a new logfile
        if (hlFileDef.LogFileMaxSize>0) and
           (uSize>=hlFileDef.LogFileMaxSize) then
          SwitchLogFile;

        case tmpMsg.Message of
          UM_HLSIMPLESTRING:
            with THLStringMsg(tmpMsg.wParam) do
            begin
              u8:=pChar(fHlMsg);                      // Rolf: UTF8 (Delphi)
              if Length(u8)>0 then
              begin
                F.WriteBuffer(u8[1],Length(u8));
                uSize:=uSize+Length(u8);              // Rolf: uSize
              end;
              if Self.FDoFeedBack then
                PostThreadMessage(Self.FVisualTID,UM_HLSIMPLESTRING,tmpMsg.wParam,0)
              else
                Free;
            end;

          UM_HLSTRINGLIST:
            begin
              u8:=TStringList(tmpMsg.wParam).Text;    // Rolf: UTF8 (Delphi)
              if length(u8)>0 then
              begin
                F.WriteBuffer(u8[1],Length(u8));
                uSize:=uSize+Length(u8);              // Rolf: uSize
              end;
              if Self.FDoFeedBack then
                PostThreadMessage(Self.FVisualTID,UM_HLSTRINGLIST,integer(tmpMsg.wParam),0)
              else
                TStringList(tmpMsg.wParam).Free;
            end;

          // --- Rolf: UM_STARTNEWLOG section deleted
        end; {case tmpMsg.Message}
      end;   {while GetMessage(tmpMsg,0,0,0) do}
    except
      on EInOutError do
      begin
        FStarted := false;
        Terminate;                                    // File can't be used;
      end;
      else                                            // hides error and continues
    end;                                              // (if it was not a FatalException...)
  finally
    FreeAndNil(F);
  end;
end;

// --- Rolf
procedure THotLogWriter.Terminate;
begin
  inherited Terminate;
  PostThreadMessage(ThreadID,WM_QUIT,0,0);
end;


{--------------------------------------------------------------------}
{---                   Threads:Visual feedback                       }
{--------------------------------------------------------------------}

// FeedBack adds everything to the memo you gave hLog, provided the
// FDoFeedBack boolean is set to true. This Bool is accesed throught
// the mutex hLog uses to update it, thus the memo is guaranted to be
// accessible (as long as you use hlog.StopVisualFeedBack if you want
// the feedBackThread to stop accessing that memo).

{$ifndef USE_SLOW_METHOD}
procedure THotLogVisual.ClearExtraLines;
const
  EmptyStr: pWideChar = '';

var
  se: integer;
  nl: integer;

begin
  If FfbMemoLimit<=0 then
    exit;

  nl:=Self.FfbMemo.Lines.Count;

  if nl<=integer(FfbMemoLimit) then
    exit;

  Dec(nl,FfbMemoLimit);

  se:=SendMessage(Self.FfbMemo.Handle,EM_LINEINDEX,nl-1,0);
  if (se<0) or (nl=0) then
    se:=SendMessage(Self.FfbMemo.Handle,EM_LINELENGTH,0,0);
  SendMessage(Self.FfbMemo.Handle, EM_SETSEL,0,se);
  SendMessage(Self.FfbMemo.Handle, EM_REPLACESEL,0,{%H-}Longint(EmptyStr));
end;

procedure THotLogVisual.DoTheScroll;
var
  ln: integer;
begin
  If Not FDoScroll then
    Exit;

  ln := SendMessage(Self.FfbMemo.Handle, EM_GETLINECOUNT, 0, 0);
  SendMessage(Self.FfbMemo.Handle, EM_LINESCROLL, 0, ln);
end;
{$endif}

constructor THotLogVisual.Create;
begin
  Inherited Create(false);
  Self.sl := TStringList.Create;
  mxHLFeedBack := CreateMutex(nil, false, pChar('THLVisualFBMutex'
                + DateTimeToStr(Now) + IntToStr({$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF})));
  Self.FStarted := true;
end;

destructor THotLogVisual.Destroy;
begin
// --- Rolf: sl is handled in THotLogVisual.Execute
//if Assigned(sl) then
//  sl.Free;
  if mxHLFeedBack<>INVALID_HANDLE_VALUE then
    CloseHandle(mxHLFeedBack);
  inherited;
end;

procedure THotLogVisual.Execute;
var
  tmpMsg: MSG;
begin
  try {except}
    FreeOnTerminate := true;
    tmpMsg.Message:=0;

// --- Rolf
    while GetMessage(tmpMsg,0,0,0) do
    begin
//      If PeekMessage(tmpMsg, 0, 0, 0, PM_REMOVE) then
      if Terminated then
        exit;
      if (WaitForSingleObject(mxHLFeedBack,INFINITE) = WAIT_OBJECT_0) then
        if (Self.fDoFeedBack) then
        begin
          case tmpMsg.message of
            UM_HLSIMPLESTRING:
              begin
                Self.s := THLStringMsg(tmpMsg.wParam).fHlMsg;
                RemoveCRLF(FROM_LINE);
                Synchronize({$IFDEF FPC}@{$ENDIF}DisplayLine);
                ReleaseMutex(mxHLFeedBack);
                THLStringMsg(tmpMsg.wParam).Free;
                Self.s := '';
              end;
            UM_HLSTRINGLIST:
              begin
                Self.sl := TStringList(tmpMsg.wParam);
                RemoveCRLF(FROM_LIST);
                Synchronize({$IFDEF FPC}@{$ENDIF}DisplayList);
                ReleaseMutex(mxHLFeedBack);
                TStringList(tmpMsg.wParam).Free;
              end;
            else ReleaseMutex(mxHLFeedBack);
          end;{Case}
        end {FDoFeedBack}
        else
          ReleaseMutex(mxHLFeedBack);    // we did catch it (but feedBack no longer requested)
      //else -> Didn't obtaine the mutex

// --- Rolf
    end;
//      if not Terminated then
//        Sleep(50); //le tout dernier
  except
// --- Rolf
    on E:Exception do
      DbgSendMsg('HotLog THLVE01: %s',[E.Message]);
  end;
end;

// --- Rolf
procedure THotLogVisual.Terminate;
begin
  inherited Terminate;
  PostThreadMessage(ThreadID,WM_QUIT,0,0);
end;

procedure THotLogVisual.RemoveCRLF(fromWhat:boolean);
// Removes one pair CRLF (or "LFCR", as it seems to potentially
// exist on some "hand made" PC ... )
var
  i : integer;
begin
  try
    if fromWhat=FROM_LINE then
    begin
      if Self.s<>'' then
      begin
        if CharInSet(Self.s[Length(s)],[#10,#13]) then
          s:=LeftStr(s,Length(s)-1);
        if CharInSet(Self.s[Length(s)],[#10,#13]) then
          s:=LeftStr(s,Length(s)-1);
      end;
    end
    else //-> list
    begin
      for i:=0 to Self.sl.Count -1 do
      begin
        s:=self.sl[i];
        if Self.s<>'' then
        begin
          if CharInSet(s[Length(s)],[#10,#13]) then
            s:=LeftStr(s,Length(s)-1);
          if CharInSet(s[Length(s)],[#10,#13]) then
            s:=LeftStr(s,Length(s)-1);
          self.sl[i]:=s;
        end;
      end;
    end;
  except
  end;
end;


// Below : Memo updating executes into the main thread (Throught the
// Synchronise mechanism) as long as GUI (so Canvas) operations are involved.

// --- Oleg modified version begins Here ---
procedure THotLogVisual.DisplayLine;
begin
  try
    Self.FfbMemo.Lines.BeginUpdate;
{$ifdef USE_SLOW_METHOD}
    Self.FfbMemo.Lines.Add(Self.s);
    if FfbMemoLimit > 0 then
      while FfbMemo.Lines.Count > integer(FfbMemoLimit) do
        FfbMemo.Lines.Delete(0);
    (* rbecker start *)
    If FDoScroll then
      SendMessage(self.FfbMemo.Handle,EM_LINESCROLL,0,1);
    (* rbecker end *)
{$else}
    // Stops Repainting Temporaly, is faster and the effect it is the same
    SendMessage(Self.FfbMemo.Handle, WM_SETREDRAW, integer(false), 0);
    try
      Self.FfbMemo.Lines.Add(Self.s);
    finally
      ClearExtraLines; // Clear the lines that exceed FfbMemoLimit
      DoTheScroll;    // Do the Memo Scrolling;
      SendMessage(Self.FfbMemo.Handle, WM_SETREDRAW, integer(true), 0);
      Self.FfbMemo.Refresh;
    end;
{$endif}
    Self.FfbMemo.Lines.EndUpdate;
    except
    end;
end;

procedure THotLogVisual.DisplayList;
{$ifdef USE_SLOW_METHOD}
var ln : integer;
{$endif}
begin
  try
    Self.FfbMemo.Lines.BeginUpdate;
{$ifdef USE_SLOW_METHOD}
    Self.FfbMemo.Lines.AddStrings(Self.sl);
    if FfbMemoLimit > 0 then
      while FfbMemo.Lines.Count > FfbMemoLimit do
        FfbMemo.Lines.Delete(0);
    If FDoScroll then
    begin
//      ln := SendMessage(self.FfbMemo.Handle,EM_GETLINECOUNT,0,0);
      ln := Self.sl.Count; //rbecker
      SendMessage(self.FfbMemo.Handle,EM_LINESCROLL,0,ln);
    end;
{$else}
    // Stops Repainting Temporaly, is faster and the effect it is the same
    SendMessage(Self.FfbMemo.Handle, WM_SETREDRAW, integer(false), 0);
    try
      Self.FfbMemo.Lines.AddStrings(Self.sl);
    finally
      ClearExtraLines; // Clear the lines that exceed FfbMemoLimit
      DoTheScroll;    // Do the Memo Scrolling;
      SendMessage(Self.FfbMemo.Handle, WM_SETREDRAW, integer(true), 0);
      Self.FfbMemo.Refresh;
    end;
{$endif}
    Self.FfbMemo.Lines.EndUpdate;
    except
    end;
end;
// --- Oleg modified version Ends Here ---


////////////////////////////////////////////////////////////////////////////////
// Others
////////////////////////////////////////////////////////////////////////////////

constructor THeapMonitor.Create (asKind: TMonitorKind);
begin
  Inherited Create;
  Self.FKindOfVals := asKind;
end;

destructor THeapMonitor.Destroy;
begin
  Self.ResetMemTable;                         // cleaning mem
  Inherited;
end;

// --- Rolf
procedure THeapMonitor.AddRec (rec: THeapMRec);
var
  ix : integer;
begin
  ix:=length(HeapRecords);
  if (ix > High(integer)-1) then
    exit;

  SetLength(HeapRecords,ix+1);
  case Self.FKindOfVals of
    mkHeap: begin
{$IFDEF FPC}
              HeapRecords[ix].MaxHeapSize:=rec.MaxHeapSize;
              HeapRecords[ix].MaxHeapUsed:=rec.MaxHeapUsed;
              HeapRecords[ix].CurrHeapSize:=rec.CurrHeapSize;
              HeapRecords[ix].CurrHeapUsed:=rec.CurrHeapUsed;
              HeapRecords[ix].CurrHeapFree:=rec.CurrHeapFree;
{$ELSE}
              HeapRecords[ix].AllocatedSmallBlockCount:=
                rec.AllocatedSmallBlockCount;
              HeapRecords[ix].TotalAllocatedSmallBlockSize:=
                rec.TotalAllocatedSmallBlockSize;
              HeapRecords[ix].ReservedSmallBlockAddressSpace:=
                rec.ReservedSmallBlockAddressSpace;
              HeapRecords[ix].AllocatedMediumBlockCount:=
                rec.AllocatedMediumBlockCount;
              HeapRecords[ix].TotalAllocatedMediumBlockSize:=
                rec.TotalAllocatedMediumBlockSize;
              HeapRecords[ix].ReservedMediumBlockAddressSpace:=
                rec.ReservedMediumBlockAddressSpace;
              HeapRecords[ix].AllocatedLargeBlockCount:=
                rec.AllocatedLargeBlockCount;
              HeapRecords[ix].TotalAllocatedLargeBlockSize:=
                rec.TotalAllocatedLargeBlockSize;
              HeapRecords[ix].ReservedLargeBlockAddressSpace:=
                rec.ReservedLargeBlockAddressSpace;
{$ENDIF}
            end;
    mkRam:  begin
              HeapRecords[ix].MemoryLoad:=rec.MemoryLoad;
              HeapRecords[ix].TotalPhysical:=rec.TotalPhysical;
              HeapRecords[ix].AvailPhysical:=rec.AvailPhysical;
              HeapRecords[ix].TotalPageFile:=rec.TotalPageFile;
              HeapRecords[ix].AvailPageFile:=rec.AvailPageFile;
              HeapRecords[ix].TotalVirtual:=rec.TotalVirtual;
              HeapRecords[ix].AvailVirtual:=rec.AvailVirtual;
            end;
  end;
  HeapRecords[ix].Extra := rec.Extra;
end;

// --- Rolf
function THeapMonitor.GetFirstEntry: THeapMRec;
var
  sz:  integer;
  mse: TMemoryStatusEx;
{$IFDEF FPC}
  hs:  TFPCHeapStatus;
{$ELSE}
  mms: TMemoryManagerState;
  i:   integer;
{$ENDIF}
begin
  sz:=length(HeapRecords)-1;
  If sz<0 then
  begin                                        // None stored -> get values (thus not realy accurate...)
    case Self.FKindOfVals of
       mkHeap: begin
{$IFDEF FPC}
                 hs:=GetFPCHeapStatus;
                 Result.MaxHeapSize:=hs.MaxHeapSize;
                 Result.MaxHeapUsed:=hs.MaxHeapUsed;
                 Result.CurrHeapSize:=hs.CurrHeapSize;
                 Result.CurrHeapUsed:=hs.CurrHeapUsed;
                 Result.CurrHeapFree:=hs.CurrHeapFree;
{$ELSE}
                 GetMemoryManagerState(mms);
                 Result.AllocatedSmallBlockCount:=0;
                 Result.TotalAllocatedSmallBlockSize:=0;
                 Result.ReservedSmallBlockAddressSpace:=0;
                 with Result, mms do
                   for i:=Low(mms.SmallBlockTypeStates) to High(mms.SmallBlockTypeStates) do
                   begin
                     AllocatedSmallBlockCount:=AllocatedSmallBlockCount+
                       SmallBlockTypeStates[i].AllocatedBlockCount;
                     TotalAllocatedSmallBlockSize:=TotalAllocatedSmallBlockSize+
                       SmallBlockTypeStates[i].InternalBlockSize*AllocatedSmallBlockCount;
                     ReservedSmallBlockAddressSpace:=ReservedSmallBlockAddressSpace+
                       SmallBlockTypeStates[i].ReservedAddressSpace;
                   end;
                 Result.AllocatedMediumBlockCount:=mms.AllocatedMediumBlockCount;
                 Result.TotalAllocatedMediumBlockSize:=mms.TotalAllocatedMediumBlockSize;
                 Result.ReservedMediumBlockAddressSpace:=mms.TotalAllocatedMediumBlockSize;
                 Result.AllocatedLargeBlockCount:=mms.AllocatedLargeBlockCount;
                 Result.TotalAllocatedLargeBlockSize:=mms.TotalAllocatedLargeBlockSize;
                 Result.ReservedLargeBlockAddressSpace:=mms.TotalAllocatedLargeBlockSize;
{$ENDIF}
               end;
       mkRam:  begin
                 mse.dwLength:=SizeOf(mse);
                 GlobalMemoryStatusEx(mse);
                 Result.MemoryLoad:=mse.dwMemoryLoad;
                 Result.TotalPhysical:=mse.ullTotalPhys;
                 Result.AvailPhysical:=mse.ullAvailPhys;
                 Result.TotalPageFile:=mse.ullTotalPageFile;
                 Result.AvailPageFile:=mse.ullAvailPageFile;
                 Result.TotalVirtual:=mse.ullTotalVirtual;
                 Result.AvailVirtual:=mse.ullAvailVirtual;
               end;
    end;{case}
    Result.Extra:='';
    exit;
  end;

  // Returns older stored entry
  case Self.FKindOfVals of
    mkHeap: begin
{$IFDEF FPC}
              Result.MaxHeapSize:=HeapRecords[0].MaxHeapSize;
              Result.MaxHeapUsed:=HeapRecords[0].MaxHeapUsed;
              Result.CurrHeapSize:=HeapRecords[0].CurrHeapSize;
              Result.CurrHeapUsed:=HeapRecords[0].CurrHeapUsed;
              Result.CurrHeapFree:=HeapRecords[0].CurrHeapFree;
 {$ELSE}
              Result.AllocatedSmallBlockCount:=HeapRecords[0].AllocatedSmallBlockCount;
              Result.TotalAllocatedSmallBlockSize:=HeapRecords[0].TotalAllocatedSmallBlockSize;
              Result.ReservedSmallBlockAddressSpace:=HeapRecords[0].TotalAllocatedSmallBlockSize;
              Result.AllocatedMediumBlockCount:=HeapRecords[0].AllocatedMediumBlockCount;
              Result.TotalAllocatedMediumBlockSize:=HeapRecords[0].TotalAllocatedMediumBlockSize;
              Result.ReservedMediumBlockAddressSpace:=HeapRecords[0].TotalAllocatedMediumBlockSize;
              Result.AllocatedLargeBlockCount:=HeapRecords[0].AllocatedLargeBlockCount;
              Result.TotalAllocatedLargeBlockSize:=HeapRecords[0].TotalAllocatedLargeBlockSize;
              Result.ReservedLargeBlockAddressSpace:=HeapRecords[0].TotalAllocatedLargeBlockSize;
{$ENDIF}
              Result.Extra:=HeapRecords[0].Extra;
            end;
    mkRam:  begin
              Result.MemoryLoad:=HeapRecords[0].MemoryLoad;
              Result.TotalPhysical:=HeapRecords[0].TotalPhysical;
              Result.AvailPhysical:=HeapRecords[0].AvailPhysical;
              Result.TotalPageFile:=HeapRecords[0].TotalPageFile;
              Result.AvailPageFile:=HeapRecords[0].AvailPageFile;
              Result.TotalVirtual:=HeapRecords[0].TotalVirtual;
              Result.AvailVirtual:=HeapRecords[0].AvailVirtual;
              Result.Extra:=HeapRecords[0].Extra;
            end;
  end;{case}
  HeapRecords:=Copy(HeapRecords,1,sz);
end;

// --- Rolf
procedure THeapMonitor.ResetMemTable;
var
  sz, ix: integer;
begin
  sz:=length(HeapRecords);
  If sz<=0 then
    exit;

  for ix := 0 to sz-1 do
    case Self.FKindOfVals of
      mkHeap: begin
{$IFDEF FPC}
                HeapRecords[ix].MaxHeapSize:=0;
                HeapRecords[ix].MaxHeapUsed:=0;
                HeapRecords[ix].CurrHeapSize:=0;
                HeapRecords[ix].CurrHeapUsed:=0;
                HeapRecords[ix].CurrHeapFree:=0;
{$ELSE}
                HeapRecords[ix].AllocatedSmallBlockCount:=0;
                HeapRecords[ix].TotalAllocatedSmallBlockSize:=0;
                HeapRecords[ix].ReservedSmallBlockAddressSpace:=0;
                HeapRecords[ix].AllocatedMediumBlockCount:=0;
                HeapRecords[ix].TotalAllocatedMediumBlockSize:=0;
                HeapRecords[ix].ReservedMediumBlockAddressSpace:=0;
                HeapRecords[ix].AllocatedLargeBlockCount:=0;
                HeapRecords[ix].TotalAllocatedLargeBlockSize:=0;
                HeapRecords[ix].ReservedLargeBlockAddressSpace:=0;
{$ENDIF}
                HeapRecords[ix].Extra:='';
              end;
      mkRam:  begin
                HeapRecords[ix].MemoryLoad:=0;
                HeapRecords[ix].TotalPhysical:=0;
                HeapRecords[ix].AvailPhysical:=0;
                HeapRecords[ix].TotalPageFile:=0;
                HeapRecords[ix].AvailPageFile:=0;
                HeapRecords[ix].TotalVirtual:=0;
                HeapRecords[ix].AvailVirtual:=0;
                HeapRecords[ix].Extra:='';
              end;
    end;
  SetLength(HeapRecords,0);
end;

constructor THLStringMsg.Create (s: string);
begin
  inherited Create;
  if s='' then
    s := #13#10
  else
    if not CharInSet(s[Length(s)],[#10,#13]) then
      s:=s+#13#10;
  fHlMsg:=s;
end;

destructor THLStringMsg.Destroy;
begin
  inherited;
end;

procedure THLStringMsg.Post(toThread:THandle; kindOfString:integer=HL_SIMPLESTRING);
begin
  PostThreadMessage(toThread, UM_HLACTIONMSG,
                    kindOfString,LPARAM(Self));
end;

constructor THLConstArrayMsg.Create(outputStyle:TVarRecStyle);
// outputStyle := vsBasic, vsExtended
begin
  Inherited create;
  Self.FOutputStyle := outputStyle;
end;

destructor THLConstArrayMsg.Destroy;
begin
  Inherited;  // the array has soon been 'finalized'
end;

procedure THLConstArrayMsg.Post(toThread:THandle);
begin
  PostThreadMessage(toThread, UM_HLACTIONMSG, HL_PARSEARRAY,LPARAM(Self));
end;

constructor THLErrMsg.Create;
begin
  inherited Create;
end;

destructor THLErrMsg.Destroy;
begin
  inherited;
end;

procedure THLErrMsg.Post(toThread:THandle);
begin
   PostThreadMessage(toThread, UM_HLACTIONMSG,
                         HL_PARSEEXCEPTION,LPARAM(Self));
end;

// --- Rolf
{$IFDEF FPC}
constructor THLHeapMonMsg.Create (ML: cardinal; TP,AP,TPF,APF,TV,AV: System.UInt64; ex: string);
begin
  inherited Create;
  hmr.MemoryLoad:=ML;
  hmr.TotalPhysical:=TP;
  hmr.AvailPhysical:=AP;
  hmr.TotalPageFile:=TPF;
  hmr.AvailPageFile:=APF;
  hmr.TotalVirtual:=TV;
  hmr.AvailVirtual:=AV;
  hmr.Extra:=ex;
end;

{$ELSE}

constructor THLHeapMonMsg.Create (ML: cardinal; TP,AP,TPF,APF,TV,AV: UInt64; ex: string);
begin
  inherited Create;
  hmr.MemoryLoad:=ML;
  hmr.TotalPhysical:=TP;
  hmr.AvailPhysical:=AP;
  hmr.TotalPageFile:=TPF;
  hmr.AvailPageFile:=APF;
  hmr.TotalVirtual:=TV;
  hmr.AvailVirtual:=AV;
  hmr.Extra:=ex;
end;
{$ENDIF}


// --- Rolf
{$IFDEF FPC}
constructor THLHeapMonMsg.Create (MHS, MHU, CHS, CHU, CHF: PtrUInt; ex: string);
begin
  inherited Create;
  hmr.MaxHeapSize:=MHS;
  hmr.MaxHeapUsed:=MHU;
  hmr.CurrHeapSize:=CHS;
  hmr.CurrHeapUsed:=CHU;
  hmr.CurrHeapFree:=CHF;
  hmr.Extra:=ex;
end;

{$ELSE}

constructor THLHeapMonMsg.Create (ASBC: cardinal; TASBS, RSBAS: NativeUInt;
                                  AMBC: cardinal; TAMBS, RMBAS: NativeUInt;
                                  ALBC: cardinal; TALBS, RLBAS: NativeUInt;
                                  ex: string);
begin
  inherited Create;
  hmr.AllocatedSmallBlockCount:=ASBC;
  hmr.TotalAllocatedSmallBlockSize:=TASBS;
  hmr.ReservedSmallBlockAddressSpace:=RSBAS;
  hmr.AllocatedMediumBlockCount:=AMBC;
  hmr.TotalAllocatedMediumBlockSize:=TAMBS;
  hmr.ReservedMediumBlockAddressSpace:=RMBAS;
  hmr.AllocatedLargeBlockCount:=ALBC;
  hmr.TotalAllocatedLargeBlockSize:=TALBS;
  hmr.ReservedLargeBlockAddressSpace:=RLBAS;
  hmr.Extra:=ex;
end;
{$ENDIF}

// --- Rolf
destructor THLHeapMonMsg.Destroy;
begin
  hmr.Extra:='';
  inherited Destroy;
end;

procedure THLHeapMonMsg.Post(toThread:THandle;RamOrHeap:integer;dirOutput:boolean=false);
// RamOrHeap = HL_HEAPMONITOR or HL_RAMMONITOR
// dirOutput = Request storage or output ;
begin
  Case dirOutput of
    true  : PostThreadMessage(toThread, UM_HLACTIONMSG, RamOrHeap,LPARAM(Self));
    false : PostThreadMessage(toThread, UM_HLSERVICEMSG,RamOrHeap,LPARAM(Self));
  end;{case}
end;


    {---------------------------------------------------------------------}
    {---                  Threads : Main(VCL) : TQTimer                ---}
    {---------------------------------------------------------------------}
    {                                                                     }
    {---------------------------------------------------------------------}

Constructor TQTimer.Create;
// Default output for timer deltas would give sthg like
// "12554092 units, 21.98452 ms"
begin
  Inherited Create;
  fmtOutput        := '%3.9n';
  TimeUnitsWanted  := [];
  removeOverHead   := false;
  deltaShowNatives := true;                                // Measure's native units
  deltaShowOptimal := true;                                // Use optimal time unit
  FIsFinalized     := false;
  InitTimerEnvir;
end;

Destructor TQTimer.Destroy;
begin
  fmtOutput := '';
  If Count > 0 then Reset;
  Inherited;
end;

procedure TQTimer.Reset;
begin
  SetLength(FEntry,0);
end;

function TQTimer.GetCount:integer;
begin
  result := Length(FEntry);
end;

function TQTimer.GetStartAsInt(ix:integer):int64;
begin
  result := GetEntry(ix,twStart);
end;

function TQTimer.GetStopAsInt(ix:integer):int64;
begin
  result := GetEntry(ix,twStop);
end;

function TQTimer.GetDeltaAsInt(ix:integer):int64;
begin
  result := GetEntry(ix,twDelta);
end;


function TQTimer.GetEntry(ix:integer;wanted:TQTimerWanted):int64;
begin
  result := -1;
  ix := Abs(ix);
  try
    If ix > (Count -1) then exit
    else Case wanted of
           twDelta : result := FEntry[ix].tStop - FEntry[ix].tStart - Overhead(ix);
           twStart : result := FEntry[ix].tStart;
         else result := FEntry[ix].tStop;
         end;{case}
  except
  end;
end;


function TQTimer.Overhead (ix: integer): integer;
begin
  result := 0;
  If Not(FIsFinalized) or Not(removeOverHead) then Exit;
  try
    Case FEntry[ix].kind of
      tkQPC : result := FQpcOverhead;
      tkRDT : result := FRDTOverhead;
    else {hms, gtc : overhead=0};
    end; {case}
  except
  end;
end;

function TQTimer.GetStartAsStr (ix: integer): string;
begin
  result := IntToStr(GetStartAsInt(ix));
end;

function TQTimer.GetStopAsStr (ix: integer): string;
begin
  result := IntToStr(GetStopAsInt(ix));
end;

function TQTimer.GetDeltaAsStr (ix: integer): string;
begin
  result := IntToStr(GetDeltaAsInt(ix));
end;

function TQTimer.GetFormatedDeltaAsExt(ix:integer):Extended;
var dummy : TQTimeScale;
begin
  result := 0.00;
  dummy:=tsSeconds;

  try
  If Not(FIsFinalized) or Not(deltaShowOptimal)
     then result := ( GetDeltaAsInt(ix) / 1 )       // Makes it an extended...
     else result := GetOptimalMeasure(GetDeltaAsInt(ix), FEntry[ix].kind, dummy);
  except
  end;
end;

function TQTimer.GetFormatedDeltaAsStr (ix: integer): string;
var tmScale  : TQTimeScale;
    split  : string;
    counter: TQTimerKind;
begin
  result := '';
  split  := '';
  tmScale:=tsSeconds;

  try
  If Not(FIsFinalized) then result := GetDeltaAsStr(ix) + ' ' + TimerUnit2Str[ Ord(FEntry[ix].kind) ]
  else begin
         counter := FEntry[ix].kind;
         If counter = tkHMS then                      // hms is a special case
         begin
           result := Int64ToTime(GetDeltaAsInt(ix));
           Exit;
         end;
         If deltaShowNatives then
         begin
           result := GetDeltaAsStr(ix) + ' ' + TimerUnit2Str[ Ord(counter) ];
           split  := ', ';
         end;
         If tsSeconds  In timeUnitsWanted then
         begin
           result := result + split + Format(fmtOutput,[getTimeMeasureAs(tsSeconds, GetDeltaAsInt(ix),counter)])
                                    + ' s.' ;         //+ TimeScale2Str[ord(tsSecondes)];
           split  := ', ';
         end;
         If tsMilliSec  In timeUnitsWanted then
         begin
           result := result + split + Format(fmtOutput,[getTimeMeasureAs(tsMilliSec,GetDeltaAsInt(ix),counter)])
                                    + ' ms';
           split  := ', ';
         end;
         If tsMicroSec  In timeUnitsWanted then
         begin
           result := result + split + Format(fmtOutput,[getTimeMeasureAs(tsMicroSec,GetDeltaAsInt(ix),counter)])
                                    + ' µs';
           split  := ', ';
         end;
         If tsNanosSec   In timeUnitsWanted then
         begin
           result := result + split + Format(fmtOutput,[getTimeMeasureAs(tsNanosSec,GetDeltaAsInt(ix),counter)])
                                    + ' ns';
           split  := ', ';
         end;
         If deltaShowOptimal
            then result := result + split + Format(fmtOutput,[GetOptimalMeasure(GetDeltaAsInt(ix),counter, tmScale)])
                                  + ' '   + TimeScale2Str[ord(tmScale)];
       end;
  except;
  end;
end;

procedure TQTimer.Store(value:int64;inOrOut:TQTimerAction;timerKind:TQTimerKind);
var ix:integer;
begin
  try
  Case inOrOut of
    taStop  :  For ix := (Length(FEntry)-1) DownTo 0 Do
                   If FEntry[ix].tStop = 0 then
                      begin
                        FEntry[ix].tStop := value;
                        break;
                      end;

    taStart : begin
                ix := Length(FEntry);
                If (ix = Pred(High(integer))) then exit;
                SetLength(FEntry,ix+1);
                FEntry[ix].tStart := value;
                FEntry[ix].tStop := 0;
                FEntry[ix].kind := timerKind;
              end;
  end;{case}
  except
  end;
end;

function TQTimer.Int64ToTime (value: int64): string;
// Converts an int64 back to hms
var Hour, Min, Sec,
    MSec, rmdM : Word;
begin
  try
    hour := value Div 3600;
    rmdM := value - (hour * 3600);
    Min  := rmdM div 60;
    Sec  := rmdM mod 60;
    Msec := 0;
    result := TimeToStr(EncodeTime(Hour, Min, Sec, MSec));
  except
    result := IntToStr(value);
  end;
end;

function  TQTimer.HMS(startOrStop:TQTimerAction=taStart):TDateTime;
var Hour, Min, Sec, MSec: Word;
    dte64 :int64;
begin
  result := Now;
  DecodeTime(result, Hour, Min, Sec, MSec);
  dte64 := ( (hour * 3600)
         +   (Min  * 60)
         +   (sec ) );
  Store(dte64,startOrStop,tkHMS);
end;

function  TQTimer.GTC (startOrStop: TQTimerAction = taStart): integer;
begin
  result := GetTickCount;
  Store(int64(result),startOrStop,tkGTC);
end;

function  TQTimer.QPC (startOrStop: TQTimerAction = taStart): int64;
begin
  Result:=-1;

  try
    QueryPerformanceCounter(result);
    Store(result,startOrStop,tkQPC);
  except
    result := -1;
  end;
end;

function  TQTimer.RDT(startOrStop:TQTimerAction=taStart):int64;
begin
  try
    result := RDTSC;
    Store(result,startOrStop,tkRDT);
  except
    result := -1;
  end;
end;

procedure TQTimer.GlobalStop;
// GlobalStop will read the first non-zero stop entry in the storage table,
// (starting from the end (ie. the most recent one)), and copy it in any
// not yet filled entry.
// If no stop has been emitted before, it does nothing...
// Speed is not an issue : We copy sthg that soon exists, we don't make a measure.

// globalStop is usefull if ALL stored entries not yet stopped come from the
// same counter ; Otherwise it is useless, and will output erroneous measures.

var ix:integer;
    gStop:int64;
begin
  gStop := 0;
  try
    For ix := (Length(FEntry)-1) downTo 0 Do
        If FEntry[ix].tStop <> 0 then begin
                                    gStop := FEntry[ix].tStop;
                                    break;
                                  end;
    If gStop <> 0 then
       For ix := (Length(FEntry)-1) downTo 0 Do
           If FEntry[ix].tStop = 0 then FEntry[ix].tStop := gStop;
  except
  end;
end;

function TQTimer.GetTimeMeasureAs(timeScale:TQTimeScale;
                          measure:int64;
                          counter:TQTimerKind):real;
// converts a reading (delta) in a time measure corresponding to the timescale wanted;
const scaler : array[0..3] of int64  = (1,1000,1000000,1000000000);
//                        (tsSecondes, tsMilliSec, tsMicroSec, tsNanosSec);
begin
  Result := measure/1;
  If Not(FIsFinalized) then Exit;
  try
  Case counter of
    tkQPC : begin
              If FQpcFrq > 0 then
                 result := (measure / FQpcFrq) * scaler[ord(timeScale)];
            end;
    tkRDT : begin
              If FRegFrq > 0 then
                 result := (measure / FRegFrq) * scaler[ord(timeScale)];
            end;
    tkGTC : begin
              If FRegFrq > 0 then
                 result := (measure / 1000) * scaler[ord(timeScale)];
            end;
  // else {hms : nothing to do};
  end;{case}
  except;end;
end;

function TQTimer.GetOptimalMeasure(measure:int64; counter:TQTimerKind; var tScale:TQTimeScale):real;
var i:TQTimeScale;
    r:Real;
begin
  try
    // hms as integer is simply returned as real :
    If counter = tkHMS then
    begin
      result := measure / 1;
      tScale := tsSeconds;
      Exit;
    end;

    For i := low(TQTimeScale) to high(TQTimeScale) Do
    begin
      r := getTimeMeasureAs(i, measure, counter);
      If r > 100 then break
      else If (r < 99.9) And (r > 0.1) then Break;
    end;
    result := r;
    tScale := i;
  except;
    result := measure/1;
    // timeScale would be loosed...
  end;
end;

procedure TQTimer.InitTimerEnvir;
// Retrieves & stores timers speeds ; Sets overhead, not resolutions.
var
  i64In,i64Out,i64Sum: int64;
  i:                   integer;
  reg:TRegistry;
begin
  i64In:=-1;
  i64Out:=-1;

// queryPerformanceCounter
  if not ( QueryPerformanceFrequency(FQpcFrq)) then
  begin
    FQpcFrq := -1;
    FqpcOverHead := -1;
  end else
  begin
    i64Sum := 0;
    For i  := 0 To 19 Do            // try to average, despite not convincing...
    begin
      QueryPerformanceCounter(i64In);
      QueryPerformanceCounter(i64Out);
      i64Sum := i64Sum +(i64Out-i64In);
    end;
    FqpcOverHead   := i64Sum div 20;
  end;
// Registry's stored frequency
  Reg := TRegistry.Create;
  FregFrq := -1;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.Access  := KEY_READ;
    If Reg.OpenKeyReadOnly('Hardware\Description\System\CentralProcessor\0') then
    begin
      FRegFrq := Reg.ReadInteger('~MHz');
      FRegFrq := FRegFrq * 1000000;
      Reg.CloseKey;
    end;
  except
  end;
// ReaDTimeStampCounter
  try
    i64In        := RDTSC;
    i64Out       := RDTSC;
    FRdtOverhead := i64Out - i64In;
    i64In        := RDTSC;
    i64Out       := RDTSC;
    FRdtOverhead := Min((i64Out - i64In),FRdtOverhead);
    i64In        := RDTSC;
    i64Out       := RDTSC;
    FRdtOverhead := Min((i64Out - i64In),FRdtOverhead);
  except
    FRdtOverhead := -1;
  end;
    Self.FIsFinalized := (FqpcOverHead > -1) And (FRdtOverhead > -1);
end;


    {--------------------------------------------------------------------}
    {---                initialization & finalization                    }
    {--------------------------------------------------------------------}

initialization
  hLog := THotLog.Create;

finalization
  try
    hLog.Free;
  except
  end;
end.

////////////////////////////////////////////////////////////////////////////////




