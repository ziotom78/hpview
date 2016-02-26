program hpview;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main_form, PaintMap, Palettes, ColumnPicker, ScanFits, FileInfoDlg,
  GnomViewWindow, gnomproj
  { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.ApplicationType := atDesktop;
    Application.Initialize;
    Application.CreateForm(THPViewForm, HPViewForm);
        Application.CreateForm(TColumnPickerDialog, ColumnPickerDialog);
        Application.CreateForm(TFileInformation, FileInformation);
        Application.CreateForm(TGnomonicView, GnomonicView);
    Application.Run;
end.

