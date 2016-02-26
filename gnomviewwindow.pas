unit GnomViewWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Healpix, GnomProj, Palettes;

type

  { TGnomonicView }

  TGnomonicView = class(TForm)
      CenterColatEdit : TEdit;
      CenterLongEdit : TEdit;
      GnomViewPanel : TPanel;
      AngularSizeTrackbar : TTrackBar;
      GnomViewPaintBox : TPaintBox;
      CenterPointPanel : TPanel;
      ColatLabel : TLabel;
      CBMinLabel : TLabel;
      CBMaxLabel : TLabel;
      LongLabel : TLabel;
      CBPaintBox : TPaintBox;
      CBPanel : TPanel;
      StatusBar : TStatusBar;
      procedure AngularSizeTrackbarChange(Sender : TObject);
      procedure CenterColatEditChange(Sender : TObject);
      procedure CenterColatEditEditingDone(Sender : TObject);
      procedure CenterLongEditChange(Sender : TObject);
      procedure CenterLongEditEditingDone(Sender : TObject);
      procedure FormCreate(Sender : TObject);
      procedure GnomViewPaintBoxMouseMove(Sender : TObject;
          Shift : TShiftState; X, Y : Integer);
      procedure GnomViewPaintBoxPaint(Sender : TObject);

  private
      MapPtr : PHealpixMap;
      RangePtr : PValueRange;
      PalettePtr : PPalette;

      Projection : TGnomonicProjection;

    { private declarations }
  public
      procedure SetDataSources(const AMapPtr : PHealpixMap;
                               const ARangePtr : PValueRange;
                               const APalettePtr : PPalette);
      procedure GetCenterCoords(out Theta, Phi : Double);
      procedure Recenter(Theta, Phi : Double);
      procedure UpdateAll;
  end;

var
  GnomonicView : TGnomonicView;

implementation

{$R *.lfm}

uses Math;

procedure TGnomonicView.FormCreate(Sender : TObject);
const
    DefaultRadiusInArcmin = 240;

begin
    MapPtr := nil;
    Projection := GnomonicProjection(0.0, 0.0,
                                     DegToRad(DefaultRadiusInArcmin / 60.0));

    AngularSizeTrackbar.Position := DefaultRadiusInArcmin;
end;

procedure TGnomonicView.AngularSizeTrackbarChange(Sender : TObject);
begin
    Projection.Radius := DegToRad(AngularSizeTrackbar.Position / 60.0);
    UpdateAll;
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

procedure TGnomonicView.CenterColatEditChange(Sender : TObject);
begin
end;

procedure TGnomonicView.CenterColatEditEditingDone(Sender : TObject);
var
    ValueInDeg : Double;
begin
    if CenterColatEdit.Caption <> '' then
    begin
        SetVarToDoubleValue(CenterColatEdit.Caption, ValueInDeg);
        Projection.CenterColat := DegToRad(ValueInDeg);
        UpdateAll;
    end;
end;

procedure TGnomonicView.CenterLongEditChange(Sender : TObject);
begin
end;

procedure TGnomonicView.CenterLongEditEditingDone(Sender : TObject);
var
    ValueInDeg : Double;
begin
    if CenterLongEdit.Caption <> '' then
    begin
        SetVarToDoubleValue(CenterLongEdit.Caption, ValueInDeg);
        Projection.CenterLong := DegToRad(ValueInDeg);
        UpdateAll;
    end;
end;

procedure TGnomonicView.GnomViewPaintBoxMouseMove(Sender : TObject;
    Shift : TShiftState; X, Y : Integer);
var
    Theta, Phi : Double;

begin
    GnomProj.XYToAngles(Projection, X, Y,
        GnomViewPaintBox.Width, GnomViewPaintBox.Height,
        Theta, Phi);
    StatusBar.SimpleText := Format('Theta = %f°, Phi = %f°',
        [RadToDeg(Theta), RadToDeg(Phi)]);
end;

function MapValueToColor(const Palette : TPalette;
                         const Range : TValueRange;
                         Value: Real): TColor; inline;
begin
    if IsNan(Value) or (Value < -1.6e+30) then
        Result := clBackground
    else
        Result := LevelToColor(Palette,
                               (Value - Range.MinValue) /
                               (Range.MaxValue - Range.MinValue));
end;

procedure TGnomonicView.GnomViewPaintBoxPaint(Sender : TObject);
var
    X, Y : Integer;
    Theta, Phi : Double;
    PixIdx : Cardinal;

begin
    if (MapPtr = nil) or
       (RangePtr = nil) or
       (PalettePtr = nil) or
       (RangePtr^.MaxValue <= RangePtr^.MinValue) then
        Exit;

    with GnomViewPaintBox do
    begin
        Canvas.Pen.Color := clBlack;
        Canvas.Rectangle(0, 0, Width - 1, Height - 1);

        for Y := 1 to Height - 2 do
        begin
            for X := 1 to Width - 2 do
            begin
                GnomProj.XYToAngles(Projection, X, Y, Width, Height, Theta, Phi);
                PixIdx := Healpix.AnglesToPix(MapPtr^, Theta, Phi);
                Canvas.Pixels[X,Y] :=
                    MapValueToColor(PalettePtr^, RangePtr^,
                                    MapPtr^.Pixels[PixIdx]);
            end;
        end;
    end;
end;

procedure TGnomonicView.SetDataSources(const AMapPtr : PHealpixMap;
                                       const ARangePtr : PValueRange;
                                       const APalettePtr : PPalette);
begin
    MapPtr := AMapPtr;
    RangePtr := ARangePtr;
    PalettePtr := APalettePtr;

    UpdateAll;
end;

procedure TGnomonicView.GetCenterCoords(out Theta, Phi : Double);
begin
    Theta := Projection.CenterColat;
    Phi := Projection.CenterLong;
end;

procedure TGnomonicView.Recenter(Theta, Phi : Double);
begin
    Projection.CenterColat := Theta;
    Projection.CenterLong := Phi;

    UpdateAll;
end;

procedure TGnomonicView.UpdateAll;
begin
    CenterColatEdit.Caption := Format('%.4g',
        [RadToDeg(Projection.CenterColat)]);
    CenterLongEdit.Caption := Format('%.4g',
        [RadToDeg(Projection.CenterLong)]);

    if RangePtr <> nil then
    begin
        CBMinLabel.Caption := Format('%.4g', [RangePtr^.MinValue]);
        CBMaxLabel.Caption := Format('%.4g', [RangePtr^.MaxValue]);
    end;

    GnomViewPaintBox.Refresh;
    CBPaintBox.Refresh;
end;

end.

