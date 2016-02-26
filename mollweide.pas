{ -*- mode: delphi -*- }

unit Mollweide;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    TRealPoint = record
        X, Y: Real;
    end;

procedure ProjectPoint(const UV: TRealPoint;
                       out Theta, Phi: real); inline;

{ Return a floating-point pair in the interval [-1,1] x [-0.5,0.5] }
procedure XYToUV(const Point, Center : TPoint; out UV : TRealPoint); inline;

function IsUVInsideMollweide(const UV: TRealPoint): boolean; inline;

implementation

uses Math;

procedure XYToUV(const Point, Center : TPoint; out UV : TRealPoint); inline;
begin
    UV.X := 2.0 * (Point.X - Center.X) / Center.X;
    UV.Y := (Center.Y - Point.Y) / Center.Y;
end;

procedure ProjectPoint(const UV: TRealPoint;
                       out Theta, Phi: real); inline;
const
    M_PI = 3.14159265358979323848;
    M_PI_2 = 1.57079632679489661924;

var
    asinV: real;
    cosAsinV: real;
begin
    asinV := ArcSin(UV.Y);
    cosAsinV := Cos(asinV);

    Theta := M_PI_2 - ArcSin(2.0 / M_PI * (asinV + UV.Y * cosAsinV));
    Phi := -M_PI_2 * UV.X / Max(cosAsinV, 1.0e-6);
end;

function xyToUV(const xyPoint, CanvasSize: TPoint): TRealPoint;
var
    Center: TRealPoint;

begin
    Center.X := CanvasSize.X / 2.0;
    Center.Y := CanvasSize.Y / 2.0;

    Result.X := 2.0 * (xyPoint.X - Center.X) / Center.X;
    Result.Y := (Center.Y - xyPoint.y) / Center.X;
end;

function IsUVInsideMollweide(const UV: TRealPoint): boolean; inline;
begin
    Result := UV.X * UV.X / 4 + UV.Y * UV.Y < 1.0;
end;

end.
