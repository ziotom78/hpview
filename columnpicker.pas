unit ColumnPicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, ScanFits;

type

  { TColumnPickerDialog }

  TColumnPickerDialog = class(TForm)
      ButtonPanel : TButtonPanel;
      HduLabel : TLabel;
      ColumnLabel : TLabel;
      HduListBox : TListBox;
      ColumnListBox : TListBox;
      HduPanel : TPanel;
      ColumnPanel : TPanel;
      ClientPanel : TPanel;
      Splitter1 : TSplitter;
      Splitter2 : TSplitter;
      procedure HduListBoxClick(Sender : TObject);

  private
      HduList : THduList;

  public
      function Run(const FileName : String;
                    out HduNumber, ColumnNumber : Integer) : Boolean;
  end;

var
  ColumnPickerDialog : TColumnPickerDialog;

implementation

{$R *.lfm}

uses Cfitsio;

procedure FillColumnBox(const Hdu : THdu;
                        var ListBox : TListBox);
var
    ColIdx : Integer;

begin
    ListBox.Items.Clear;
    for ColIdx := Low(Hdu.Columns) to High(Hdu.Columns) do
    begin
        ListBox.Items.Add(Format('Column #%d: "%s"',
            [Hdu.Columns[ColIdx].Index,
             Hdu.Columns[ColIdx].Name]));
    end;
end;

procedure TColumnPickerDialog.HduListBoxClick(Sender : TObject);
var
    ColIdx : Integer;

begin
    if HduListBox.ItemIndex < 0 then
        Exit;

    FillColumnBox(HduList.Hdus[HduListBox.ItemIndex], ColumnListBox);
    ColumnListBox.ItemIndex := 0;
end;

function TColumnPickerDialog.Run(const FileName : String;
                                 out HduNumber, ColumnNumber : Integer) : Boolean;
var
    HduIdx : Integer;

    function HduTypeToStr(const HduType : Cfitsio.THduType) : String;
    begin
        case HduType of
        ImageHdu: Result := '2-D image';
        AsciiTable: Result := 'ASCII table';
        BinaryTable: Result := 'binary table';
        else Result := 'unknown type';
        end;
    end;

begin
    ScanFitsFile(FileName, HduList);

    if Length(HduList.Hdus) = 0 then
    begin
        MessageDlg(Format('No valid Healpix maps found in file "%s".',
                          [FileName]),
                   mtWarning, [mbOk], 0);
        Exit(False);
    end;

    (* If there is just one HDU with one column (and this happens quite often
       with temperature maps produced by simulations), there is no point in
       asking the user what to do: just read that column. *)
    if (Length(HduList.Hdus) = 1) and (Length(HduList.Hdus[0].Columns) = 1) then
    begin
        with HduList.Hdus[0] do
        begin
            HduNumber := Index;
            ColumnNumber := Columns[0].Index;
            Exit(True);
        end;
    end;

    HduListBox.Items.Clear;
    for HduIdx := 0 to High(HduList.Hdus) do
    begin
        HduListBox.Items.Add(Format('"%s" (%s)',
                                    [HduList.Hdus[HduIdx].Name,
                                     HduTypeToStr(HduList.Hdus[HduIdx].HduType)]));
    end;

    HduListBox.ItemIndex := 0;
    FillColumnBox(HduList.Hdus[HduListBox.ItemIndex], ColumnListBox);
    if ColumnListBox.Items.Count > 0 then
        ColumnListBox.ItemIndex := 0;

    (* Run the dialog and collect the results *)
    if (ShowModal = mrOk) and
       (HduListBox.ItemIndex >= 0) and
       (ColumnListBox.ItemIndex >= 0) then
	begin
        HduNumber := HduList.Hdus[HduListBox.ItemIndex].Index;
        ColumnNumber := HduList.Hdus[HduListBox.ItemIndex].Columns[ColumnListBox.ItemIndex].Index;
        Result := True;
    end else
        Result := False;
end;

end.

