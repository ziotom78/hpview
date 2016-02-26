{ -*- mode: delphi -*- }

unit CFitsIo;

{$mode objfpc}{$h+}

interface

uses SysUtils,
     Ctypes;

type
    TFitsFile = Pointer;

    EFitsError = class(Exception)
    end;

    TIOMode = (ReadOnly, ReadAndWrite);

    THduType = (ImageHdu, AsciiTable, BinaryTable, AnyHdu);

    TTableType = AsciiTable .. BinaryTable;

    TFitsType = (FitsTypeBit,
                 FitsTypeLogical,
                 FitsTypeString,
                 FitsTypeShort,
                 FitsTypeLong,
                 FitsTypeLonglong,
                 FitsTypeInt,
                 FitsTypeFloat,
                 FitsTypeDouble,
                 FitsTypeComplex,
                 FitsTypeDoubleComplex,
                 FitsTypeSignedByte,
                 FitsTypeUnsignedInt,
                 FitsTypeUnsignedShort,
                 FitsTypeUnsignedLong,
                 FitsTypeByte);

    TColumn = record
        Name : String;
        Count : Integer;
        DataType : TFitsType;
        UnitStr : String;
    end;

    TOverwriteMode = (Overwrite, DoNotOverwrite);

function OpenFile(FileName : String; Mode : TIOMode) : TFitsFile;
function OpenDiskFile(FileName : String; Mode : TIOMode) : TFitsFile;
function OpenData(FileName : String; Mode : TIOMode) : TFitsFile;
function OpenTable(FileName : String; Mode : TIOMode) : TFitsFile;
function OpenImage(FileName : String; Mode : TIOMode) : TFitsFile;

function CreateFile(FileName : String;
                    OverwriteMode : TOverwriteMode) : TFitsFile;
function CreateDiskFile(FileName : String;
                        OverwriteMode : TOverwriteMode) : TFitsFile;

procedure CloseFile(var F : TFitsFile);
procedure DeleteFile(var F : TFitsFile);

function GetFileName(const F : TFitsFile) : String;
function GetFileMode(const F : TFitsFile) : TIoMode;
function GetUrlType(const F : TFitsFile) : String;

function MoveAbsHdu(var F : TFitsFile; HduNum : Integer) : THduType;
function MoveRelHdu(var F : TFitsFile; HduNum : Integer) : THduType;
procedure MoveNamedHdu(var F : TFitsFile;
                      HduType : THduType;
                      ExtName : String;
                      ExtVer : Integer);

function GetNumberOfHdus(const F : TFitsFile) : Cardinal;

function ReadKeyAsString(const F : TFitsFile; KeyName : String) : String;
function ReadKeyAsBoolean(const F : TFitsFile; KeyName : String) : Boolean;
function ReadKeyAsByte(const F : TFitsFile; KeyName : String) : Byte;
function ReadKeyAsShortint(const F : TFitsFile; KeyName : String) : Shortint;
function ReadKeyAsWord(const F : TFitsFile; KeyName : String) : Word;
function ReadKeyAsInteger(const F : TFitsFile; KeyName : String) : Integer;
function ReadKeyAsLongint(const F : TFitsFile; KeyName : String) : Longint;
function ReadKeyAsCardinal(const F : TFitsFile; KeyName : String) : Cardinal;
function ReadKeyAsInt64(const F : TFitsFile; KeyName : String) : Int64;
function ReadKeyAsSingle(const F : TFitsFile; KeyName : String) : Single;
function ReadKeyAsDouble(const F : TFitsFile; KeyName : String) : Double;

procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : String;
                   Comment : String);
procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Boolean;
                   Comment : String);
procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Cardinal;
                   Comment : String);
procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Int64;
                   Comment : String);
procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Double;
                   Comment : String);

procedure UpdateKey(var F : TFitsFile;
                    KeyName : String;
                    KeyValue : String;
                    Comment : String);
procedure UpdateKey(var F : TFitsFile;
                    KeyName : String;
                    KeyValue : Boolean;
                    Comment : String);
procedure UpdateKey(var F : TFitsFile;
                    KeyName : String;
                    KeyValue : Cardinal;
                    Comment : String);
procedure UpdateKey(var F : TFitsFile;
                    KeyName : String;
                    KeyValue : Int64;
                    Comment : String);
procedure UpdateKey(var F : TFitsFile;
                    KeyName : String;
                    KeyValue : Double;
                    Comment : String);

procedure WriteComment(var F : TFitsFile; Comment : String);
procedure WriteHistory(var F : TFitsFile; History : String);
procedure WriteDate(var F : TFitsFile);
procedure WriteKeyUnit(var F : TFitsFile; KeyName : String; KeyUnit : String);

procedure CreateTable(var F : TFitsFile;
                      TableType : TTableType;
                      NumOfRows : Int64;
                      Columns : Array of TColumn;
                      ExtName : String);

function GetNumberOfRows(const F : TFitsFile) : Int64;
function GetNumberOfColumns(const F : TFitsFile) : Integer;
function GetRowSize(const F : TFitsFile) : Cardinal;
procedure GetColumnType(const F : TFitsFile;
                        ColumnNumber : Integer;
                        out FitsType : TFitsType;
                        out RepeatCount : Int64;
                        out Width : Int64);

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of PChar);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of String);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Boolean);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Uint8);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Int8);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Uint16);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Int16);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Cuint);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Cint);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Int64);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Single);
procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Double);

procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of String);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Boolean);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Uint8);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Int8);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Uint16);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Int16);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Cuint);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Cint);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Int64);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Single);
procedure ReadColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      var Elements : Array of Double);

implementation

const
    cfitsio_lib = 'cfitsio';

const
    FITSIO_DATATYPE_BIT        = 1;
    FITSIO_DATATYPE_BYTE       = 11;
    FITSIO_DATATYPE_LOGICAL    = 14;
    FITSIO_DATATYPE_STRING     = 16;
    FITSIO_DATATYPE_SHORT      = 21;
    FITSIO_DATATYPE_LONG       = 41;
    FITSIO_DATATYPE_LONGLONG   = 81;
    FITSIO_DATATYPE_FLOAT      = 42;
    FITSIO_DATATYPE_DOUBLE     = 82;
    FITSIO_DATATYPE_COMPLEX    = 83;
    FITSIO_DATATYPE_DBLCOMPLEX = 163;
    FITSIO_DATATYPE_INT        = 31;
    FITSIO_DATATYPE_SBYTE      = 12;
    FITSIO_DATATYPE_UINT       = 30;
    FITSIO_DATATYPE_USHORT     = 20;
    FITSIO_DATATYPE_ULONG      = 40;

procedure AllocateAndCopy(out Dest : PChar; Src: String);
begin
    Dest := StrAlloc(Length(Src) + 1);
    StrPCopy(Dest, Src);
end;

{------------------------------------------------------------------------------}

{ The function GetFitsioErrorMessage is used internally by the
  routine. Error conditions are always signaled to the outside world
  through a EFitsError exception. }

function ffgerr(Status : Cint; ErrMsg : PChar) : Integer; cdecl; external cfitsio_lib;
function ffgmsg(ErrMsg : PChar) : Integer; cdecl; external cfitsio_lib;

function GetFitsioErrorMessage(Status : Cint) : String;
var
    Buf : Array[0..30] of Cchar;
    Buf2 : Array[0..80] of Cchar;
    Code : Cint;
    AdditionalInfo : String;

begin
    ffgerr(Status, @Buf[0]);
    Result := StrPas(@Buf[0]);

    AdditionalInfo := '';
    repeat
        Code := ffgmsg(@Buf2[0]);
        if Code <> 0 then
        begin
            if AdditionalInfo <> '' then
                AdditionalInfo := AdditionalInfo + ' -> ';

            AdditionalInfo := AdditionalInfo + StrPas(@Buf2[0]);
        end;
    until Code = 0;

    if AdditionalInfo <> '' then
        Result := Result + ': ' + AdditionalInfo;
end;

{------------------------------------------------------------------------------}

function FitsTypeFromTypeCode(TypeCode : Cint) : TFitsType;
begin
    case TypeCode of
    FITSIO_DATATYPE_BIT: Exit(FitsTypeBit);
    FITSIO_DATATYPE_BYTE: Exit(FitsTypeByte);
    FITSIO_DATATYPE_LOGICAL: Exit(FitsTypeLogical);
    FITSIO_DATATYPE_STRING: Exit(FitsTypeString);
    FITSIO_DATATYPE_SHORT: Exit(FitsTypeShort);
    FITSIO_DATATYPE_LONG: Exit(FitsTypeLong);
    FITSIO_DATATYPE_LONGLONG: Exit(FitsTypeLonglong);
    FITSIO_DATATYPE_FLOAT: Exit(FitsTypeFloat);
    FITSIO_DATATYPE_DOUBLE: Exit(FitsTypeDouble);
    FITSIO_DATATYPE_COMPLEX: Exit(FitsTypeComplex);
    FITSIO_DATATYPE_DBLCOMPLEX: Exit(FitsTypeDoubleComplex);
    FITSIO_DATATYPE_INT: Exit(FitsTypeInt);
    FITSIO_DATATYPE_SBYTE: Exit(FitsTypeSignedByte);
    FITSIO_DATATYPE_UINT: Exit(FitsTypeUnsignedInt);
    FITSIO_DATATYPE_USHORT: Exit(FitsTypeUnsignedShort);
    FITSIO_DATATYPE_ULONG: Exit(FitsTypeUnsignedLong);
    else raise EFitsError.Create(Format('Type code %d not supported by this ' +
                                        'version of the Cfitsio bindings',
                                        [TypeCode]));
    end;
end;

{------------------------------------------------------------------------------}

{ As functions OpenFile, OpenDiskFile, OpenData, OpenTable, and
  OpenImage are very similar, we avoid too much cut-and-paste by
  implementing a "OpenGeneric" function which accepts a procedural
  parameter as one of its arguments. }

type
    TOpenFunction = function(out Ptr : Pointer;
                             FileName : PChar;
                             IoMode : CInt;
                             var Status : Cint) : Cint; cdecl;

function ffopen(out Ptr : Pointer;
                FileName : PChar;
                IoMode : CInt;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffdkopn(out Ptr : Pointer;
                 FileName : PChar;
                 IoMode : CInt;
                 var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffdopn(out Ptr : Pointer;
                FileName : PChar;
                IoMode : CInt;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function fftopn(out Ptr : Pointer;
                FileName : PChar;
                IoMode : CInt;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffiopn(out Ptr : Pointer;
                FileName : PChar;
                IoMode : CInt;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function OpenGeneric(FileName : String;
                     Mode : TIOMode;
                     Fn : TOpenFunction) : TFitsFile;
var
    FileNameAsciiz : PChar;
    Status : Cint = 0;
    IoModeCode : Cint;

begin
    FileNameAsciiz := StrAlloc(Length(FileName) + 1);
    StrPCopy(FileNameAsciiz, FileName);

    case Mode of
    ReadOnly: IoModeCode := 0;
    ReadAndWrite: IoModeCode := 1;
    end;

    Fn(Result, FileNameAsciiz, IoModeCode, Status);
    StrDispose(FileNameAsciiz);

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to open file "%s", %s',
                                   [FileName, GetFitsioErrorMessage(Status)]);
end;

function OpenFile(FileName : String; Mode : TIOMode) : TFitsFile;
begin
    Result := OpenGeneric(FileName, Mode, @ffopen);
end;

function OpenDiskFile(FileName : String; Mode : TIOMode) : TFitsFile;
begin
    Result := OpenGeneric(FileName, Mode, @ffdkopn);
end;

function OpenData(FileName : String; Mode : TIOMode) : TFitsFile;
begin
    Result := OpenGeneric(FileName, Mode, @ffdopn);
end;

function OpenTable(FileName : String; Mode : TIOMode) : TFitsFile;
begin
    Result := OpenGeneric(FileName, Mode, @fftopn);
end;

function OpenImage(FileName : String; Mode : TIOMode) : TFitsFile;
begin
    Result := OpenGeneric(FileName, Mode, @ffiopn);
end;

{------------------------------------------------------------------------------}

type
    TCreateFunction = function(out Ptr : Pointer;
                               FileName : PChar;
                               var Status : Cint) : Cint; cdecl;

function ffinit(out Ptr : Pointer;
                FileName : PChar;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffdkinit(out Ptr : Pointer;
                  FileName : PChar;
                  var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function CreateGeneric(FileName : String;
                       OverwriteMode : TOverwriteMode;
                       Fn : TCreateFunction) : TFitsFile;
var
    RealFileName : String;
    FileNameAsciiz : PChar;
    Status : Cint = 0;

begin
    case OverwriteMode of
    Overwrite: RealFileName := '!' + FileName;
    DoNotOverwrite: RealFileName := FileName;
    end;

    FileNameAsciiz := StrAlloc(Length(RealFileName) + 1);
    StrPCopy(FileNameAsciiz, RealFileName);

    Fn(Result, FileNameAsciiz, Status);
    StrDispose(FileNameAsciiz);

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to create file "%s", %s',
                                   [FileName, GetFitsioErrorMessage(Status)]);
end;

function CreateFile(FileName : String;
                    OverwriteMode : TOverwriteMode) : TFitsFile;
begin
    Result := CreateGeneric(FileName, OverwriteMode, @ffinit);
end;

function CreateDiskFile(FileName : String;
                        OverwriteMode : TOverwriteMode) : TFitsFile;
begin
    Result := CreateGeneric(FileName, OverwriteMode, @ffdkinit);
end;

{------------------------------------------------------------------------------}

function ffclos(Ptr : Pointer; var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure CloseFile(var F : TFitsFile);
var
    Status : Cint = 0;
begin
    ffclos(F, Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));
    F := nil;
end;

{------------------------------------------------------------------------------}

function ffdelt(Ptr : Pointer; var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure DeleteFile(var F : TFitsFile);
var
    Status : Cint = 0;
begin
    ffdelt(F, Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));
    F := nil;
end;

{------------------------------------------------------------------------------}

function ffflnm(Ptr : Pointer;
                FileName : PChar;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function GetFileName(const F : TFitsFile) : String;
var
    FileNameAsciiz : Array[0..1025] of Cchar;
    Status : Cint = 0;

begin
    ffflnm(F, @(FileNameAsciiz[0]), Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));

    Result := StrPas(@(FileNameAsciiz[0]));
end;

{------------------------------------------------------------------------------}

function ffflmd(Ptr : Pointer;
                IoModeCode : PCint;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function GetFileMode(const F : TFitsFile) : TIoMode;
var
    IoModeCode : Cint;
    Status : Cint = 0;

begin
    ffflmd(F, @IoModeCode, Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));

    if IoModeCode = 0 then
        Result := ReadOnly
    else
        Result := ReadAndWrite;
end;

{------------------------------------------------------------------------------}

function ffurlt(Ptr : Pointer;
                UrlType : PChar;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function GetUrlType(const F : TFitsFile) : String;
var
    UrlTypeAsciiz : Array[0..1025] of Cchar;
    Status : Cint = 0;

begin
    ffurlt(F, @(UrlTypeAsciiz[0]), Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));

    Result := StrPas(@(UrlTypeAsciiz[0]));
end;

{------------------------------------------------------------------------------}

function ffmahd(Ptr : Pointer;
                HduNum : Cint;
                out HduType : Cint;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffmrhd(Ptr : Pointer;
                HduNum : Cint;
                out HduType : Cint;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffmnhd(Ptr : Pointer;
                HduType : Cint;
                ExtName : PChar;
                ExtVer : Cint;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function HduTypeFromCode(Code : Cint) : THduType;
begin
    case Code of
    0: Result := ImageHdu;
    1: Result := AsciiTable;
    2: Result := BinaryTable;
    else Result := AnyHdu;
    end;
end;

function HduCode(HduType : THduType) : Cint;
begin
    case HduType of
    ImageHdu: Result := 0;
    AsciiTable: Result := 1;
    BinaryTable: Result := 2;
    AnyHdu: Result := -1;
    end;
end;

function MoveAbsHdu(var F : TFitsFile; HduNum : Integer) : THduType;
var
    HduTypeCode : Cint;
    Status : Cint = 0;

begin
    ffmahd(F, Cint(HduNum), HduTypeCode, Status);
    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to move to HDU #%d, %s',
                                   [HduNum, GetFitsioErrorMessage(Status)]);

    Result := HduTypeFromCode(HduTypeCode);
end;

function MoveRelHdu(var F : TFitsFile; HduNum : Integer) : THduType;
var
    HduTypeCode : Cint;
    Status : Cint = 0;

begin
    ffmrhd(F, Cint(HduNum), HduTypeCode, Status);
    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to move by %d HDUs, %s',
                                   [HduNum, GetFitsioErrorMessage(Status)]);

    Result := HduTypeFromCode(HduTypeCode);
end;

procedure MoveNamedHdu(var F : TFitsFile;
                      HduType : THduType;
                      ExtName : String;
                      ExtVer : Integer);
var
    ExtNameAsciiz : PChar;
    Status : Cint = 0;

begin
    ExtNameAsciiz := StrAlloc(Length(ExtName) + 1);
    StrPCopy(ExtNameAsciiz, ExtName);
    ffmnhd(F, HduCode(HduType), ExtNameAsciiz, ExtVer, Status);
    StrDispose(ExtNameAsciiz);

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to move to HDU "%s", %s',
                                   [ExtName, GetFitsioErrorMessage(Status)]);
end;

{------------------------------------------------------------------------------}

function ffthdu(Ptr : Pointer;
                HduNum : PCint;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function GetNumberOfHdus(const F : TFitsFile) : Cardinal;
var
    HduNum : Cint;
    Status : Cint = 0;

begin
    ffthdu(F, @HduNum, Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));

    Result := Cardinal(HduNum);
end;

{------------------------------------------------------------------------------}

function ffghdn(Ptr : Pointer;
                HduNum : PCint) : Cint; cdecl; external cfitsio_lib;
function ffghdt(Ptr : Pointer;
                HduTypeCode : PCint;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function GetCurrentHduNumber(const F : TFitsFile) : Cardinal;
var
    HduNum : Cint;

begin
    ffghdn(F, @HduNum);
    Result := Cardinal(HduNum);
end;

function GetCurrentHduType(const F : TFitsFile) : THduType;
var
    HduTypeCode : Cint;
    Status : Cint = 0;

begin
    ffghdt(F, @HduTypeCode, Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));

    Result := HduTypeFromCode(HduTypeCode);
end;

{------------------------------------------------------------------------------}

function ffgky(Ptr : Pointer;
               Datatype : Cint;
               KeyName : PChar;
               Dest : Pointer;
               Comment : PChar;
               var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure ReadKeyGeneric(const F : TFitsFile;
                         KeyName : String;
                         Datatype : Integer;
                         Dest : Pointer);
var
    KeyNameAsciiz : PChar;
    Status : Cint = 0;

begin
    KeyNameAsciiz := StrAlloc(Length(KeyName) + 1);
    StrPCopy(KeyNameAsciiz, KeyName);
    ffgky(F, Datatype, KeyNameAsciiz, Dest, nil, Status);
    StrDispose(KeyNameAsciiz);

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to read key "%s", %s',
                                   [KeyName, GetFitsioErrorMessage(Status)]);
end;

function ReadKeyAsString(const F : TFitsFile; KeyName : String) : String;
var
    KeyValue : Array [0..72] of Cchar;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_STRING, @(KeyValue[0]));
    Result := StrPas(@KeyValue[0]);
end;

function ReadKeyAsBoolean(const F : TFitsFile; KeyName : String) : Boolean;
var
    KeyValue : Cint;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_LOGICAL, @KeyValue);
    Result := KeyValue <> 0;
end;

function ReadKeyAsByte(const F : TFitsFile; KeyName : String) : Byte;
var
    KeyValue : Cchar;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_BYTE, @KeyValue);
    Result := Byte(KeyValue);
end;

function ReadKeyAsShortint(const F : TFitsFile; KeyName : String) : Shortint;
var
    KeyValue : Cchar;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_BYTE, @KeyValue);
    Result := Shortint(KeyValue);
end;

function ReadKeyAsWord(const F : TFitsFile; KeyName : String) : Word;
var
    KeyValue : Cushort;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_USHORT, @KeyValue);
    Result := Word(KeyValue);
end;

function ReadKeyAsInteger(const F : TFitsFile; KeyName : String) : Integer;
var
    KeyValue : Cint;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_INT, @KeyValue);
    Result := Integer(KeyValue);
end;

function ReadKeyAsLongint(const F : TFitsFile; KeyName : String) : Longint;
var
    KeyValue : Clong;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_LONG, @KeyValue);
    Result := Longint(KeyValue);
end;

function ReadKeyAsCardinal(const F : TFitsFile; KeyName : String) : Cardinal;
var
    KeyValue : Culong;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_ULONG, @KeyValue);
    Result := Cardinal(KeyValue);
end;

function ReadKeyAsInt64(const F : TFitsFile; KeyName : String) : Int64;
var
    KeyValue : Clonglong;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_LONGLONG, @KeyValue);
    Result := Int64(KeyValue);
end;

function ReadKeyAsSingle(const F : TFitsFile; KeyName : String) : Single;
var
    KeyValue : Cfloat;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_FLOAT, @KeyValue);
    Result := Single(KeyValue);
end;

function ReadKeyAsDouble(const F : TFitsFile; KeyName : String) : Double;
var
    KeyValue : Cdouble = 12345.0;

begin
    ReadKeyGeneric(F, KeyName, FITSIO_DATATYPE_DOUBLE, @KeyValue);
    Result := Double(KeyValue);
end;

{------------------------------------------------------------------------------}

type
    TWriteKeyFunction = function(Ptr : Pointer;
                                 Datatype : Cint;
                                 KeyName : PChar;
                                 Value : Pointer;
                                 Comment : PChar;
                                 var Status : Cint) : Cint; cdecl;

function ffpky(Ptr : Pointer;
               Datatype : Cint;
               KeyName : PChar;
               Value : Pointer;
               Comment : PChar;
               var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffuky(Ptr : Pointer;
               Datatype : Cint;
               KeyName : PChar;
               Value : Pointer;
               Comment : PChar;
               var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure WriteKeyGeneric(var F : TFitsFile;
                          KeyName : String;
                          KeyValue : Pointer;
                          KeyType : Cint;
                          Comment : String;
                          WriteKeyFn : TWriteKeyFunction);
var
    KeyNameAsciiz : PChar;
    CommentAsciiz : PChar;
    Status : Cint = 0;

begin
    AllocateAndCopy(KeyNameAsciiz, KeyName);
    AllocateAndCopy(CommentAsciiz, Comment);

    WriteKeyFn(F, KeyType, KeyNameAsciiz,
               KeyValue, CommentAsciiz, Status);

    StrDispose(KeyNameAsciiz);
    StrDispose(CommentAsciiz);

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to write key "%s", %s',
                                   [KeyName, GetFitsioErrorMessage(Status)]);
end;

procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : String;
                   Comment : String);
var
    KeyValueAsciiz : PChar;

begin
    AllocateAndCopy(KeyValueAsciiz, KeyValue);

    try
        WriteKeyGeneric(F, KeyName, KeyValueAsciiz,
                        FITSIO_DATATYPE_STRING, Comment, @ffpky);
    finally
        StrDispose(KeyValueAsciiz);
    end;
end;

procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Boolean;
                   Comment : String);
var
    KeyValueInt : Cint;

begin
    KeyValueInt := Cint(KeyValue);
    WriteKeyGeneric(F, KeyName, @KeyValueInt,
                    FITSIO_DATATYPE_LOGICAL, Comment, @ffpky);
end;

procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Cardinal;
                   Comment : String);
var
    KeyValueUlong : Culong;

begin
    KeyValueUlong := Cint(KeyValue);
    WriteKeyGeneric(F, KeyName, @KeyValueUlong,
                    FITSIO_DATATYPE_ULONG, Comment, @ffpky);
end;

procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Int64;
                   Comment : String);
var
    KeyValueLonglong : Clonglong;

begin
    KeyValueLonglong := Clonglong(KeyValue);
    WriteKeyGeneric(F, KeyName, @KeyValueLonglong,
                    FITSIO_DATATYPE_LONGLONG, Comment, @ffpky);
end;

procedure WriteKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Double;
                   Comment : String);
var
    KeyValueDouble : Cdouble;

begin
    KeyValueDouble := Cdouble(KeyValue);
    WriteKeyGeneric(F, KeyName, @KeyValueDouble,
                    FITSIO_DATATYPE_DOUBLE, Comment, @ffpky);
end;

procedure UpdateKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : String;
                   Comment : String);
var
    KeyValueAsciiz : PChar;

begin
    AllocateAndCopy(KeyValueAsciiz, KeyValue);

    try
        WriteKeyGeneric(F, KeyName, KeyValueAsciiz,
                        FITSIO_DATATYPE_STRING, Comment, @ffuky);
    finally
        StrDispose(KeyValueAsciiz);
    end;
end;

procedure UpdateKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Boolean;
                   Comment : String);
var
    KeyValueInt : Cint;

begin
    KeyValueInt := Cint(KeyValue);
    WriteKeyGeneric(F, KeyName, @KeyValueInt,
                    FITSIO_DATATYPE_LOGICAL, Comment, @ffuky);
end;

procedure UpdateKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Cardinal;
                   Comment : String);
var
    KeyValueUlong : Culong;

begin
    KeyValueUlong := Cint(KeyValue);
    WriteKeyGeneric(F, KeyName, @KeyValueUlong,
                    FITSIO_DATATYPE_ULONG, Comment, @ffuky);
end;

procedure UpdateKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Int64;
                   Comment : String);
var
    KeyValueLonglong : Clonglong;

begin
    KeyValueLonglong := Clonglong(KeyValue);
    WriteKeyGeneric(F, KeyName, @KeyValueLonglong,
                    FITSIO_DATATYPE_LONGLONG, Comment, @ffuky);
end;

procedure UpdateKey(var F : TFitsFile;
                   KeyName : String;
                   KeyValue : Double;
                   Comment : String);
var
    KeyValueDouble : Cdouble;

begin
    KeyValueDouble := Cdouble(KeyValue);
    WriteKeyGeneric(F, KeyName, @KeyValueDouble,
                    FITSIO_DATATYPE_DOUBLE, Comment, @ffuky);
end;

{------------------------------------------------------------------------------}

function ffpcom(Ptr : Pointer;
                Comment : PChar;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffphis(Ptr : Pointer;
                Comment : PChar;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffpdat(Ptr : Pointer; var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure WriteComment(var F : TFitsFile; Comment : String);
var
    CommentAsciiz : PChar;
    Status : Cint = 0;

begin
    AllocateAndCopy(CommentAsciiz, Comment);
    ffpcom(F, CommentAsciiz, Status);
    StrDispose(CommentAsciiz);
    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to write comment "%s", %s',
                                   [Comment, GetFitsioErrorMessage(Status)]);
end;

procedure WriteHistory(var F : TFitsFile; History : String);
var
    HistoryAsciiz : PChar;
    Status : Cint = 0;

begin
    AllocateAndCopy(HistoryAsciiz, History);
    ffphis(F, HistoryAsciiz, Status);
    StrDispose(HistoryAsciiz);
    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to write history "%s", %s',
                                   [History, GetFitsioErrorMessage(Status)]);
end;

procedure WriteDate(var F : TFitsFile);
var
    Status : Cint = 0;

begin
    ffpdat(F, Status);
    if Status <> 0 then
        raise EFitsError.Create('Unable to write the current date, ' +
                                    GetFitsioErrorMessage(Status));
end;

{------------------------------------------------------------------------------}

function ffpunt(Ptr : Pointer;
                KeyName, KeyUnit : PChar;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure WriteKeyUnit(var F : TFitsFile; KeyName : String; KeyUnit : String);
var
    KeyNameAsciiz, KeyUnitAsciiz : PChar;
    Status : Cint = 0;

begin
    AllocateAndCopy(KeyNameAsciiz, KeyName);
    AllocateAndCopy(KeyUnitAsciiz, KeyUnit);

    ffpunt(F, KeyNameAsciiz, KeyUnitAsciiz, Status);

    StrDispose(KeyNameAsciiz);
    StrDispose(KeyUnitAsciiz);

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to write unit "%s" for key "%s", %s',
                                   [KeyUnit, KeyName,
                                    GetFitsioErrorMessage(Status)]);
end;

{------------------------------------------------------------------------------}

function ffcrtb(Ptr : Pointer;
                Tbltype : Cint;
                Naxis2 : Clonglong;
                Tfields : Cint;
                TType : PPChar;
                TForm : PPChar;
                TUnit : PPChar;
                Extname : PChar;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure CreateTable(var F : TFitsFile;
                      TableType : TTableType;
                      NumOfRows : Int64;
                      Columns : Array of TColumn;
                      ExtName : String);

    function CountAndTypeToFormStr(Count : Integer; DataType : TFitsType) : String;
    var
        TypeChar : String;
    begin
        case DataType of
        FitsTypeBit: TypeChar := 'X';
        FitsTypeLogical: TypeChar := 'L';
        FitsTypeString: TypeChar := 'A';
        FitsTypeShort: TypeChar := 'I';
        FitsTypeLong: TypeChar := 'J';
        FitsTypeLonglong: TypeChar := 'K';
        FitsTypeInt: TypeChar := 'I';
        FitsTypeFloat: TypeChar := 'E';
        FitsTypeDouble: TypeChar := 'D';
        FitsTypeComplex: TypeChar := 'C';
        FitsTypeDoubleComplex: TypeChar := 'M';
        FitsTypeSignedByte: TypeChar := 'S';
        FitsTypeUnsignedInt: TypeChar := 'V';
        FitsTypeUnsignedShort: TypeChar := 'U';
        FitsTypeUnsignedLong: TypeChar := 'U'; { Are we sure of this? }
        FitsTypeByte: TypeChar := 'B';
        end;
        Result := Format('%d%s', [Count, TypeChar]);
    end;

var
    TableTypeCode : Cint;
    ExtNameAsciiz : PChar;
    TType : Array of PChar;
    TForm : Array of PChar;
    TUnit : Array of PChar;
    Idx, ColumnIdx : Integer;
    Status : Cint = 0;

begin
    case TableType of
    AsciiTable: TableTypeCode := 1;
    BinaryTable: TableTypeCode := 2;
    end;

    SetLength(TType, Length(Columns));
    SetLength(TForm, Length(Columns));
    SetLength(TUnit, Length(Columns));

    for Idx := 0 to Length(Columns) - 1 do
    begin
        ColumnIdx := Low(Columns) + Idx;
        AllocateAndCopy(TType[Idx], Columns[ColumnIdx].Name);
        AllocateAndCopy(TForm[Idx],
                        CountAndTypeToFormStr(Columns[ColumnIdx].Count,
                                              Columns[ColumnIdx].DataType));
        AllocateAndCopy(TUnit[Idx], Columns[ColumnIdx].UnitStr);
    end;

    AllocateAndCopy(ExtNameAsciiz, ExtName);
    ffcrtb(F, TableTypeCode, Clonglong(NumOfRows), Length(Columns),
           @TType[0], @TForm[0], @TUnit[0], ExtNameAsciiz, Status);
    StrDispose(ExtNameAsciiz);

    for Idx := 0 to Length(Columns) - 1 do
    begin
        StrDispose(TType[Idx]);
        StrDispose(TForm[Idx]);
        StrDispose(TUnit[Idx]);
    end;

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to create table "%s", %s',
                                   [ExtName, GetFitsioErrorMessage(Status)]);
end;

{------------------------------------------------------------------------------}

function ffgnrwll(Ptr : Pointer;
                  out Nrows : Clonglong;
                  var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffgncl(Ptr : Pointer;
                out Ncols : Cint;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffgrsz(Ptr : Pointer;
                out Ncols : Clong;
                var Status : Cint) : Cint; cdecl; external cfitsio_lib;
function ffgtclll(Ptr : Pointer;
                  Colnum : Cint;
                  out Typecode : Cint;
                  out RepeatCount : Int64;
                  out Width : Int64;
                  var Status : Cint) : Cint; cdecl; external cfitsio_lib;

function GetNumberOfRows(const F : TFitsFile) : Int64;
var
    NrowsLonglong : Clonglong;
    Status : Cint = 0;

begin
    ffgnrwll(F, NrowsLonglong, Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));

    Result := Int64(NrowsLonglong);
end;

function GetNumberOfColumns(const F : TFitsFile) : Integer;
var
    NcolsInt : Cint;
    Status : Cint = 0;

begin
    ffgncl(F, NcolsInt, Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));

    Result := Integer(NcolsInt);
end;

function GetRowSize(const F : TFitsFile) : Cardinal;
var
    RowSize : Clong;
    Status : Cint = 0;

begin
    ffgrsz(F, RowSize, Status);
    if Status <> 0 then
        raise EFitsError.Create(GetFitsioErrorMessage(Status));

    Result := Cardinal(RowSize);
end;

procedure GetColumnType(const F : TFitsFile;
                        ColumnNumber : Integer;
                        out FitsType : TFitsType;
                        out RepeatCount : Int64;
                        out Width : Int64);
var
    TypeCode : Cint;
    RepeatCountLonglong : Clonglong;
    WidthLonglong : Clonglong;
    Status : Cint = 0;

begin
    ffgtclll(F, Cint(ColumnNumber), TypeCode,
             RepeatCountLonglong, WidthLonglong, Status);
    FitsType := FitsTypeFromTypeCode(TypeCode);
    RepeatCount := Int64(RepeatCountLongLong);
    Width := Int64(WidthLonglong);
end;

{------------------------------------------------------------------------------}

function ffpcl(Ptr : Pointer;
               Datatype : Cint;
               Colnum : Cint;
               Firstrow : Clonglong;
               Firstelem : Clonglong;
               Nelements : Clonglong;
               Values : Pointer;
               var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure WriteColumnGeneric(var F : TFitsFile;
                             Datatype : Cint;
                             ColumnNumber : Integer;
                             FirstRow : Int64;
                             FirstElement : Int64;
                             Nelements : Int64;
                             Values : Pointer);
var
    Status : Cint = 0;

begin
    ffpcl(F, Datatype, Cint(ColumnNumber),
          Clonglong(FirstRow), Clonglong(FirstElement),
          Clonglong(Nelements), Values, Status);

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to write data in column #%d, %s',
                                   [ColumnNumber,
                                    GetFitsioErrorMessage(Status)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of PChar);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_STRING, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of String);
var
    AsciizArray : Array of PChar;
    Idx : Cardinal;

begin
    SetLength(AsciizArray, Length(Elements));
    for Idx := 0 to Length(AsciizArray) - 1 do
        AllocateAndCopy(AsciizArray[Idx], Elements[Low(Elements) + Idx]);

    WriteColumnGeneric(F, FITSIO_DATATYPE_STRING, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @AsciizArray[0]);

    for Idx := 0 to Length(AsciizArray) - 1 do
        StrDispose(AsciizArray[Idx]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Boolean);
var
    IntArray : Array of Cchar;
    Idx : Cardinal;

begin
    SetLength(IntArray, Length(Elements));
    for Idx := 0 to Length(IntArray) - 1 do
    begin
        if Elements[Low(Elements) + Idx] then
            IntArray[Idx] := 1
        else
            IntArray[Idx] := 0;
    end;

    WriteColumnGeneric(F, FITSIO_DATATYPE_LOGICAL, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @IntArray[0]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Uint8);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_BYTE, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Int8);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_SBYTE, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Uint16);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_USHORT, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Int16);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_SHORT, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Cuint);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_UINT, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Cint);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_INT, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Int64);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_LONGLONG, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Single);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_FLOAT, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

procedure WriteColumn(var F : TFitsFile;
                      ColumnNumber : Integer;
                      FirstRow : Int64;
                      FirstElement : Int64;
                      Elements : Array of Double);
begin
    WriteColumnGeneric(F, FITSIO_DATATYPE_DOUBLE, ColumnNumber,
                       FirstRow, FirstElement, Length(Elements),
                       @Elements[Low(Elements)]);
end;

{------------------------------------------------------------------------------}

function ffgcv(Ptr : Pointer;
               Datatype : Cint;
               Colnum : Cint;
               Firstrow : Clonglong;
               Firstelem : Clonglong;
               Nelements : Clonglong;
               NullValue : Pointer;
               Values : Pointer;
               out AnyNull : Cint;
               var Status : Cint) : Cint; cdecl; external cfitsio_lib;

procedure ReadColumnGeneric(var F : TFitsFile;
                            Datatype : Cint;
                            ColumnNumber : Integer;
                            FirstRow : Int64;
                            FirstElement : Int64;
                            Nelements : Int64;
                            Values : Pointer);
var
    AnyNull : Cint;
    Status : Cint = 0;

begin
    ffgcv(F, Datatype, Cint(ColumnNumber),
          Clonglong(FirstRow), Clonglong(FirstElement),
          Clonglong(Nelements), nil, Values, AnyNull, Status);

    if Status <> 0 then
        raise EFitsError.CreateFmt('Unable to read column #%d, %s',
                                   [ColumnNumber,
                                    GetFitsioErrorMessage(Status)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of String);
var
    AsciizArray : Array of PChar;
    Idx : Cardinal;
    ColType : TFitsType;
    ColRepeat : Int64;
    ColWidth : Int64;

begin
    SetLength(AsciizArray, Length(Elements));
    GetColumnType(F, ColumnNumber, ColType, ColRepeat, ColWidth);
    for Idx := 0 to Length(AsciizArray) - 1 do
        AsciizArray[Idx] := StrAlloc(ColWidth);

    ReadColumnGeneric(F, FITSIO_DATATYPE_STRING, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @AsciizArray[0]);

    for Idx := 0 to Length(AsciizArray) - 1 do
    begin
        Elements[Low(Elements) + Idx] := StrPas(AsciizArray[Idx]);
        StrDispose(AsciizArray[Idx]);
    end;
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Boolean);
var
    IntArray : Array of Cint;
    Idx : Cardinal;

begin
    SetLength(IntArray, Length(Elements));
    for Idx := 0 to Length(IntArray) - 1 do
    begin
        if Elements[Low(Elements) + Idx] then
            IntArray[Idx] := 1
        else
            IntArray[Idx] := 0;
    end;

    ReadColumnGeneric(F, FITSIO_DATATYPE_LOGICAL, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @IntArray[0]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Uint8);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_BYTE, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Int8);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_SHORT, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Uint16);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_USHORT, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Int16);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_SHORT, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Cuint);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_UINT, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Cint);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_INT, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Int64);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_LONGLONG, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Single);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_FLOAT, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

procedure ReadColumn(var F : TFitsFile;
                     ColumnNumber : Integer;
                     FirstRow : Int64;
                     FirstElement : Int64;
                     var Elements : Array of Double);
begin
    ReadColumnGeneric(F, FITSIO_DATATYPE_DOUBLE, ColumnNumber,
                      FirstRow, FirstElement, Length(Elements),
                      @Elements[Low(Elements)]);
end;

end.
