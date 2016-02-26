{ -*- mode: delphi -*- }

unit PaintMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Healpix, Palettes;

procedure CalculatePixelRange(const Map : THealpixMap;
                              out ValueRange : TValueRange);
procedure PaintMap(const Map: THealpixMap;
                   const Range : TValueRange;
                   const Palette : TPalette;
                   var Bitmap: TBitmap);

implementation

uses Math, Mollweide;

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

procedure PaintMap(const Map: THealpixMap;
                   const Range : TValueRange;
                   const Palette : TPalette;
                   var Bitmap: TBitmap);
var
    X, Y: longword;
    CurColor : TColor;
    UV: TRealPoint;
    BitmapSize : TPoint;
    Center: TPoint;
    Theta, Phi: real;
    PixelIdx: Int64;

begin
    Bitmap.GetSize(BitmapSize.X, BitmapSize.Y);
    Center.X := BitmapSize.X div 2;
    Center.Y := BitmapSize.Y div 2;

    Bitmap.Canvas.Brush.Color := clBackground;
    Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);

    Bitmap.Canvas.Brush.Color := Palette.UnseenColor;
    Bitmap.Canvas.Pen.Color := Palette.UnseenColor;
    Bitmap.Canvas.Ellipse(0, 0, BitmapSize.X - 1, BitmapSize.Y - 1);

    if Range.MinValue < Range.MaxValue then
    begin
        for Y := 0 to BitmapSize.Y - 1 do
        begin
            for X := 0 to BitmapSize.X - 1 do
            begin
                XYToUV(Point(X, Y), Center, UV);
                if IsUVInsideMollweide(UV) then
                begin
                    { We're inside the ellipse: find the point}
                    ProjectPoint(UV, Theta, Phi);
                    PixelIdx := AnglesToPix(Map, Theta, Phi);
                    CurColor :=
                        MapValueToColor(Palette, Range, Map.Pixels[PixelIdx]);

                    Bitmap.Canvas.Pixels[X, Y] := CurColor;
                end;
            end;
        end;
    end;

    Bitmap.Canvas.Pen.Color := clBlack;
    Bitmap.Canvas.Arc(0, 0, BitmapSize.X - 1, BitmapSize.Y - 1, 0, 5760);
end;

procedure CalculatePixelRange(const Map : THealpixMap;
                              out ValueRange : TValueRange);
var
    Idx : Cardinal;

begin
    with ValueRange do
    begin
        MinValue := Map.Pixels[0];
        MaxValue := Map.Pixels[0];
        for Idx := 1 to High(Map.Pixels) do
        begin
            if Map.Pixels[Idx] < -1.6e+30 then
                continue;

            if Map.Pixels[Idx] > MaxValue then
               MaxValue := Map.Pixels[Idx]
            else if Map.Pixels[Idx] < MinValue then
               MinValue := Map.Pixels[Idx];
        end;
    end;
end;

end.

