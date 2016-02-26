unit ScanFits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Cfitsio;

type
    TColumn = record
       Index : Integer;
       Name : String;
       Datatype : String;
    end;

    THdu = record
       Index : Integer;
       Name : String;
       HduType : THduType;
       Nside : Integer;
       Columns : Array of TColumn;
    end;

    THduList = record
       FileName : String;
       Hdus : Array of THdu;
    end;

procedure ScanFitsFile(const FileName : String; out HduList : THduList);

implementation

uses Healpix;

procedure ScanFitsFile(const FileName : String; out HduList : THduList);
var
    F : TFitsFile;
    NumOfValidHdus : Integer;
    CurHduType : THduType;
    HduIdx, ColIdx : Integer;

begin
    F := Cfitsio.OpenFile(FileName, ReadOnly);
    try
        HduList.FileName := FileName;
        SetLength(HduList.Hdus, Cfitsio.GetNumberOfHdus(F));
        NumOfValidHdus := 0;

        (* Loop over all the HDUs *)
        for HduIdx := 0 to High(HduList.Hdus) do
        begin
            with HduList.Hdus[NumOfValidHdus] do
            begin
                Index := HduIdx + 1;
                CurHduType := Cfitsio.MoveAbsHdu(F, Index);
                if not (CurHduType in [AsciiTable, BinaryTable]) then
                    continue;

                HduType := CurHduType;

                try
                    Name := Cfitsio.ReadKeyAsString(F, 'EXTNAME');
                except
                    Name := '';
                end;

                try
                    Nside := Cfitsio.ReadKeyAsInteger(F, 'NSIDE');
                except
                    Nside := -1;
                end;

                (* If this isn't a valid NSIDE, it means that this HDU does
                   not contain a Healpix map but something else (e.g., a
                   power spectrum) *)
                if (Nside < 0) or (not Healpix.IsNsideValid(Nside)) then
                    continue;

                SetLength(Columns, Cfitsio.GetNumberOfColumns(F));

                (* Loop over all the columns of the current HDU *)
                for ColIdx := 0 to High(Columns) do
                begin
                    Columns[ColIdx].Index := ColIdx + 1;
                    Columns[ColIdx].Name :=
                        Cfitsio.ReadKeyAsString(F, Format('TTYPE%d', [ColIdx + 1]));
                    Columns[ColIdx].Datatype :=
                        Cfitsio.ReadKeyAsString(F, Format('TFORM%d', [ColIdx + 1]));
                end;

                Inc(NumOfValidHdus);
            end;
        end;

        SetLength(HduList.Hdus, NumOfValidHdus);
    finally
        Cfitsio.CloseFile(F);
    end;
end;

end.

