{ -*- mode: delphi -*- }

unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ActnList, Menus, ComCtrls, StdCtrls, Healpix, RotMatrix, Palettes, PaintMap,
  ColumnPicker, FileInfoDlg, GnomViewWindow;

type

  { THPViewForm }

  THPViewForm = class(TForm)
      GnomViewMenuItem : TMenuItem;
      WindowMenuItem : TMenuItem;
      ToggleGnomView : TAction;
      InformationMenuItem : TMenuItem;
      ViewInformation : TAction;
      MinUnitLabel : TLabel;
      MaxUnitLabel : TLabel;
      MinValueEdit : TEdit;
      MaxValueEdit : TEdit;
      GalToEqMenuItem : TMenuItem;
      EqToGalMenuItem : TMenuItem;
      ColorBarPaintBox : TPaintBox;
      ColorBarPanel : TPanel;
      MollweidePanel : TPanel;
      MinPanel : TPanel;
      MaxPanel : TPanel;
      RotateEqToGal : TAction;
      RotateGalToEq : TAction;
      MenuItem3 : TMenuItem;
      GrayscaleMenuItem : TMenuItem;
      HealpixMenuItem : TMenuItem;
      PlanckMenuItem : TMenuItem;
      PaletteGrayscale : TAction;
      PalettePlanck : TAction;
      PaletteHealpix : TAction;
      FileSaveAs: TAction;
      MenuItem1: TMenuItem;
      MenuItem2: TMenuItem;
      OpenDialog: TOpenDialog;
      RotateMapMenuItem: TMenuItem;
      ApplyTransformationMenuItem: TMenuItem;
      SaveDialog: TSaveDialog;
      StatisticsMenuItem: TMenuItem;
      MenuItem7: TMenuItem;
      MenuItem8: TMenuItem;
      MainStatusBar: TStatusBar;
      ViewStatistics: TAction;
      ViewApplyTransformation: TAction;
      SetColorRangeMenuItem: TMenuItem;
      ViewMenu: TMenuItem;
      ViewSetColorRange: TAction;
      FileExit: TAction;
      FileOpen: TAction;
      MainActionList: TActionList;
      MainMenu: TMainMenu;
      FIleMenu: TMenuItem;
      OpenMapMenuItem: TMenuItem;
      ExitMenuItem: TMenuItem;
      MollweidePaintBox: TPaintBox;

      procedure ColorBarPaintBoxPaint(Sender : TObject);
      procedure FileExitExecute(Sender: TObject);
      procedure FileOpenExecute(Sender: TObject);
      procedure FileSaveAsExecute(Sender: TObject);
      procedure FormShow(Sender : TObject);
      procedure MaxValueEditEditingDone(Sender : TObject);
      procedure MinValueEditEditingDone(Sender : TObject);
      procedure MollweidePaintBoxMouseMove(Sender : TObject;
          Shift : TShiftState; X, Y : Integer);
      procedure MollweidePaintBoxMouseUp(Sender : TObject;
          Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      procedure MollweidePaintBoxPaint(Sender: TObject);
      procedure PaletteGrayscaleExecute(Sender : TObject);
      procedure PaletteHealpixExecute(Sender : TObject);
      procedure PalettePlanckExecute(Sender : TObject);
      procedure RotateEqToGalExecute(Sender : TObject);
      procedure RotateEqToGalUpdate(Sender : TObject);
      procedure RotateGalToEqExecute(Sender : TObject);
      procedure RotateGalToEqUpdate(Sender : TObject);
      procedure ToggleGnomViewExecute(Sender : TObject);
      procedure ViewInformationExecute(Sender : TObject);

  private
      CurrentFileName : String;
      Map : THealpixMap;
      MapBitmap : TBitmap;
      PixelRange : TValueRange;
      Palette : TPalette;
      DirtyBitmapFlag : Boolean;
      ShowNeverCalled : Boolean;

  public
      constructor Create(AParent : TComponent); override;
      destructor Destroy; override;

      procedure LoadMap(FileName : String);
      procedure UpdateStatus(const Msg : String);
      procedure RepaintMap;
  end;

var
  HPViewForm: THPViewForm;

implementation

{$R *.lfm}

uses Math, Mollweide, cfitsio;

procedure THPViewForm.MollweidePaintBoxPaint(Sender: TObject);
var
  BitmapSize : TPoint;

  function FindBestSizeForBitmap : TPoint;
  begin
    if 2 * MollweidePaintBox.Height > MollweidePaintBox.Width then
    begin
      Result.X := MollweidePaintBox.Width;
      Result.Y := MollweidePaintBox.Width div 2;
    end else begin
      Result.X := MollweidePaintBox.Height * 2;
      Result.Y := MollweidePaintBox.Height;
    end;
  end;

var
   MustRepaintBitmap : Boolean;

begin
  BitmapSize := FindBestSizeForBitmap;
  MustRepaintBitmap := (BitmapSize.X <> MapBitmap.Width) or
                       (BitmapSize.Y <> MapBitmap.Height) or
                       DirtyBitmapFlag;

  if MustRepaintBitmap then
  begin
    MapBitmap.SetSize(BitmapSize.X, BitmapSize.Y);
    PaintMap.PaintMap(Map, PixelRange, Palette, MapBitmap);
    DirtyBitmapFlag := False;
  end;

  with MollweidePaintBox.Canvas do
    Draw((Width - BitmapSize.X) div 2,
         (Height - BitmapSize.Y) div 2,
         MapBitmap);
end;

procedure THPViewForm.PaletteGrayscaleExecute(Sender : TObject);
begin
    CreateGrayscalePalette(Palette);
    RepaintMap;
end;

procedure THPViewForm.PaletteHealpixExecute(Sender : TObject);
begin
    CreateWmapPalette(Palette);
    RepaintMap;
end;

procedure THPViewForm.PalettePlanckExecute(Sender : TObject);
begin
    CreatePlanckPalette(Palette);
    RepaintMap;
end;

procedure THPViewForm.RotateEqToGalExecute(Sender : TObject);
begin
    UpdateStatus('Applying rotation...');
    RotateMap(Map, GalacticToEclipticMatrix);
    Map.CoordSys := csGalactic;
    RepaintMap;
    UpdateStatus('Map rotated from Equatorial to Galactic coordinates');
end;

procedure THPViewForm.RotateEqToGalUpdate(Sender : TObject);
begin
  if Map.CoordSys <> csUnknown then
  begin
      RotateEqToGal.Enabled := Map.CoordSys = csEcliptic;
  end else
      RotateEqToGal.Enabled := True;
end;

procedure THPViewForm.RotateGalToEqExecute(Sender : TObject);
begin
    UpdateStatus('Applying rotation...');
    RotateMap(Map, EclipticToGalacticMatrix);
    Map.CoordSys := csEcliptic;
    RepaintMap;
    UpdateStatus('Map rotated from Galactic to Equatorial coordinates');
end;

procedure THPViewForm.RotateGalToEqUpdate(Sender : TObject);
begin
    if Map.CoordSys <> csUnknown then
    begin
        RotateGalToEq.Enabled := Map.CoordSys = csGalactic;
    end else
        RotateGalToEq.Enabled := True;
end;

procedure THPViewForm.ToggleGnomViewExecute(Sender : TObject);
begin
    GnomonicView.SetDataSources(@Map, @PixelRange, @Palette);
    GnomonicView.Visible := not GnomonicView.Visible;
end;

procedure THPViewForm.ViewInformationExecute(Sender : TObject);
begin
    FileInformation.SetControlValuesFromMap(Map);
    if FileInformation.ShowModal = mrOk then
    begin
        FileInformation.SetMapFromControlValues(Map);
        RepaintMap;
    end;
end;

procedure THPViewForm.FileOpenExecute(Sender: TObject);
begin
     if OpenDialog.Execute then
     begin
        LoadMap(OpenDialog.FileName);

        DirtyBitmapFlag := True;
        RepaintMap;
        GnomonicView.UpdateAll;
     end;
end;

procedure THPViewForm.FileExitExecute(Sender: TObject);
begin
    Close;
end;

procedure THPViewForm.ColorBarPaintBoxPaint(Sender : TObject);
var
    CanvasSize : TPoint;
    X : Integer;

begin
    CanvasSize.X := ColorBarPaintBox.Width;
    CanvasSize.Y := ColorBarPaintBox.Height;

    (* Draw a border around the color bar *)
    ColorBarPaintBox.Canvas.Brush.Color := clBlack;
    ColorBarPaintBox.Canvas.FrameRect(0, 0, CanvasSize.X - 1, CanvasSize.Y - 1);

    (* Draw the gradient *)
    for X := 1 to CanvasSize.X - 2 do
    begin
        ColorBarPaintBox.Canvas.Pen.Color :=
            LevelToColor(Palette, X / CanvasSize.X);
        ColorBarPaintBox.Canvas.Line(X, 1, X, CanvasSize.Y - 2);
    end;
end;

procedure THPViewForm.FileSaveAsExecute(Sender: TObject);
begin
    if SaveDialog.Execute then
    begin
       WriteHealpixMap(SaveDialog.FileName, Map, FitsTypeDouble);
    end;
end;

procedure THPViewForm.FormShow(Sender : TObject);
begin
    (* We cannot run this in OnCreate, as LoadMap might want to
       open the "Column picker" dialog. *)
    if ShowNeverCalled then
    begin
        if ParamCount = 1 then
	        LoadMap(ParamStr(1));

        ShowNeverCalled := False;
    end;
end;

function SetVarToDoubleValue(Text : String; var Value : Double) : Boolean;
var
    Code : Integer;
    Number : Double;

begin
    Val(Text, Number, Code);
    if Code <> 0 then
    begin
        MessageDlg(Format('Invalid floating-point number "%s"', [Text]),
                   mtError, [mbOk], 0);
        Exit(False);
    end else
        Value := Number;

    Result := True;
end;

procedure THPViewForm.MaxValueEditEditingDone(Sender : TObject);
begin
    if MaxValueEdit.Caption <> '' then
    begin
        SetVarToDoubleValue(MaxValueEdit.Caption, PixelRange.MaxValue);
        RepaintMap;
    end;
end;

procedure THPViewForm.MinValueEditEditingDone(Sender : TObject);
begin
    if MinValueEdit.Caption <> '' then
    begin
        SetVarToDoubleValue(MinValueEdit.Caption, PixelRange.MinValue);
        RepaintMap;
    end;
end;

procedure THPViewForm.MollweidePaintBoxMouseMove(Sender : TObject;
    Shift : TShiftState; X, Y : Integer);
var
    UV : TRealPoint;
    Theta, Phi : Real;
    PixIndex : Cardinal;

begin
    Mollweide.XYToUV(Point(X - (MollweidePaintBox.Width - MapBitmap.Width) div 2,
                           Y - (MollweidePaintBox.Height - MapBitmap.Height) div 2),
                     Point(MapBitmap.Width div 2, MapBitmap.Height div 2),
                     UV);
    if Mollweide.IsUVInsideMollweide(UV) then
    begin
        Cursor := crCross;

        Mollweide.ProjectPoint(UV, Theta, Phi);
        PixIndex := Healpix.AnglesToPix(Map, Theta, Phi);
        UpdateStatus(Format('θ = %f°, φ = %f°, pixel value = %.6g %s',
                            [Math.RadToDeg(Theta), Math.RadToDeg(Phi),
                             Map.Pixels[PixIndex], Map.MeasureUnit]));
    end else begin
        Cursor := crDefault;

        UpdateStatus('');
    end;
end;

procedure THPViewForm.MollweidePaintBoxMouseUp(Sender : TObject;
    Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
    UV : TRealPoint;
    Theta, Phi : Real;

begin
    Mollweide.XYToUV(Point(X - (MollweidePaintBox.Width - MapBitmap.Width) div 2,
                           Y - (MollweidePaintBox.Height - MapBitmap.Height) div 2),
                     Point(MapBitmap.Width div 2, MapBitmap.Height div 2),
                     UV);
    if Mollweide.IsUVInsideMollweide(UV) then
    begin
        Mollweide.ProjectPoint(UV, Theta, Phi);
        GnomonicView.Recenter(Theta, Phi);
    end;
end;

constructor THPViewForm.Create(APArent : TComponent);
begin
    inherited Create(AParent);

    InitMap(1, Ring, Map);
    PixelRange.MinValue := 0.0;
    PixelRange.MaxValue := 0.0;

    CreatePlanckPalette(Palette);

    MapBitmap := TBitmap.Create;
    DirtyBitmapFlag := True;
    ShowNeverCalled := True;
end;

destructor THPViewForm.Destroy;
begin
  MapBitmap.Destroy;
  inherited Destroy;
end;

procedure THPViewForm.LoadMap(FileName : String);
var
    HduNumber, ColNumber : Integer;

begin
    if ColumnPickerDialog.Run(FileName, HduNumber, ColNumber) then
    begin
        CurrentFileName := FileName;
        UpdateStatus(Format('Loading map "%s"...', [FileName]));

        Caption := CurrentFileName;
        ReadHealpixMap(CurrentFileName, HduNumber, ColNumber, Map);
        CalculatePixelRange(Map, PixelRange);

        UpdateStatus(Format('Map "%s" loaded', [FileName]));
    end;
end;

procedure THPViewForm.RepaintMap;
begin
    DirtyBitmapFlag := True;

    MinValueEdit.Text := Format('%.4g', [PixelRange.MinValue]);
    MaxValueEdit.Text := Format('%.4g', [PixelRange.MaxValue]);

    MinUnitLabel.Caption := Map.MeasureUnit;
    MaxUnitLabel.Caption := Map.MeasureUnit;

    MollweidePaintBox.Refresh;
    ColorBarPaintBox.Refresh;
    GnomonicView.Update;
end;

procedure THPViewForm.UpdateStatus(const Msg : String);
begin
    MainStatusBar.SimpleText := Msg;
    Application.ProcessMessages;
end;

end.
