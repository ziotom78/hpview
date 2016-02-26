unit Palettes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
    PValueRange = ^TValueRange;
    TValueRange = record
      MinValue, MaxValue : Double;
    end;

    TPaletteStep = record
      Level : Single; (* Number in the [0, 1] range *)
      Color : TColor;
    end;

    PPalette = ^TPalette;
    TPalette = record
      (* These must be sorted according to their "Level" field,
         and the first and last steps *must* be have their level
         equal to 0.0 and 1.0 *)
      Steps : Array of TPaletteStep;
      UnseenColor : TColor;
    end;

procedure CreateGrayscalePalette(out Palette : TPalette);
procedure CreateWmapPalette(out Palette : TPalette);
procedure CreatePlanckPalette(out Palette : TPalette);

function LevelToColor(const Palette : TPalette; Level : Double) : TColor;

implementation

function RGBLevelsToColor(Red, Green, Blue : Double) : TColor; inline;
begin
    Result := RGBToColor(Round(Red * $FF),
                         Round(Green * $FF),
                         Round(Blue * $FF));
end;

procedure CreateGrayscalePalette(out Palette : TPalette);
begin
    SetLength(Palette.Steps, 2);

    Palette.Steps[0].Level := 0.00;
    Palette.Steps[0].Color := RGBLevelsToColor(0.0, 0.0, 0.0);

    Palette.Steps[1].Level := 1.00;
    Palette.Steps[1].Color := RGBLevelsToColor(1.0, 1.0, 1.0);
end;

procedure CreateWmapPalette(out Palette : TPalette);
begin
    SetLength(Palette.Steps, 6);

    Palette.Steps[0].Level := 0.00;
    Palette.Steps[0].Color := RGBLevelsToColor(0.0, 0.0, 0.5);

    Palette.Steps[1].Level := 0.15;
 	Palette.Steps[1].Color := RGBLevelsToColor(0.0, 0.0, 1.0);

    Palette.Steps[2].Level := 0.40;
    Palette.Steps[2].Color := RGBLevelsToColor(0.0, 1.0, 1.0);

    Palette.Steps[3].Level := 0.70;
    Palette.Steps[3].Color := RGBLevelsToColor(1.0, 1.0, 0.0);

    Palette.Steps[4].Level := 0.90;
    Palette.Steps[4].Color := RGBLevelsToColor(1.0, 0.33, 0.0);

    Palette.Steps[5].Level := 1.00;
    Palette.Steps[5].Color := RGBLevelsToColor(0.5, 0.0, 0.0);
end;

procedure CreatePlanckPalette(out Palette : TPalette);
begin
    SetLength(Palette.Steps, 6);

    Palette.Steps[0].Level := 0.0;
    Palette.Steps[0].Color := RGBLevelsToColor(0.0, 0.0, 1.0);

    Palette.Steps[1].Level := 0.33;
    Palette.Steps[1].Color := RGBLevelsToColor(0.0, 0.87, 1.0);

    Palette.Steps[2].Level := 0.50;
    Palette.Steps[2].Color := RGBLevelsToColor(1.0, 0.93, 0.85);

    Palette.Steps[3].Level := 0.67;
    Palette.Steps[3].Color := RGBLevelsToColor(1.0, 0.7, 0.0);

    Palette.Steps[4].Level := 0.83;
    Palette.Steps[4].Color := RGBLevelsToColor(1.0, 0.3, 0.0);

    Palette.Steps[5].Level := 1.0;
    Palette.Steps[5].Color := RGBLevelsToColor(0.4, 0.0, 0.0);
end;

function LevelToColor(const Palette : TPalette; Level : Double) : TColor;
var
    Idx : Cardinal;
    Level0, Level1 : Double;
    Red0, Green0, Blue0 : Byte;
    Red1, Green1, Blue1 : Byte;
    Weight0, Weight1 : Double;

begin
     if Level <= 0.0 then
        Exit(Palette.Steps[0].Color)
     else if Level >= 1.0 then
        Exit(Palette.Steps[High(Palette.Steps)].Color);

     Idx := 1;
     while Palette.Steps[Idx].Level < Level do
         Inc(Idx);

     Level0 := Palette.Steps[Idx - 1].Level;
     Level1 := Palette.Steps[Idx].Level;

     Weight0 := (Level1 - Level) / (Level1 - Level0);
     Weight1 := (Level - Level0) / (Level1 - Level0);

     RedGreenBlue(Palette.Steps[Idx - 1].Color, Red0, Green0, Blue0);
     RedGreenBlue(Palette.Steps[Idx].Color,     Red1, Green1, Blue1);

     Result :=
         RGBToColor(Byte(Round(Red0 * Weight0 + Red1 * Weight1)),
                    Byte(Round(Green0 * Weight0 + Green1 * Weight1)),
                    Byte(Round(Blue0 * Weight0 + Blue1 * Weight1)));
end;

end.

