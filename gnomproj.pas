(* Gnomonic projection functions
   Author: Maurizio Tomasi
   First version: September 25, 2014

   This unit implements a number of mathematical functions for projecting
   a sphere on a plane using the Gnomonic projection.

   For details about this projection, see the MathWorld site:

       http://mathworld.wolfram.com/GnomonicProjection.html

   The only difference with the equations in the MathWorld page is that here
   we use the colatitude θ = π/2 - λ instead of the latitude λ. *)

unit GnomProj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    TGnomonicProjection = record
        CenterColat, CenterLong : Double;
        Radius : Double; (* (0, π] *)
    end;

function GnomonicProjection(CenterColat, CenterLong : Double;
                            Radius : Double) : TGnomonicProjection;

(* x = 0..Width-1, y = 0..Height-1 *)
procedure XYToAngles(const GnomProj : TGnomonicProjection;
                     x, y : Integer;
                     Width, Height : Integer;
                     out Theta, Phi : Double);

implementation

uses Math;

function GnomonicProjection(CenterColat, CenterLong : Double;
                            Radius : Double) : TGnomonicProjection;
begin
    Result.CenterColat := CenterColat;
    Result.CenterLong := CenterLong;
    Result.Radius := Radius;
end;

procedure XYToAngles(const GnomProj : TGnomonicProjection;
                     x, y : Integer;
                     Width, Height : Integer;
                     out Theta, Phi : Double);
const
    Epsilon = 1.0e-12;

var
    RectSide : Integer;
    SphereRadius : Double;
    TrueRadius : Double;
    Rho, c, SinC, CosC : Double;
    ScaledX, ScaledY : Double;

begin
    TrueRadius := Abs(GnomProj.Radius);
    if TrueRadius > Pi / 4 then
        TrueRadius := Pi / 4;

    RectSide := Max(Width, Height);
    ScaledY := (Width div 2) - x;
    ScaledX := (Height div 2) - y;

    Rho := Sqrt(ScaledX * ScaledX + ScaledY * ScaledY);
    if Rho < Epsilon then
    begin
        Theta := GnomProj.CenterColat;
        Phi := GnomProj.CenterLong;
        Exit;
    end;

    (* SphereRadius is the radius of the sky sphere. Since RectSide is in
       pixels, then the sphere radius is in pixel too. It is chosen so that
       a bitmap whose side is RectSide encompasses an angle TrueRadius. *)
    SphereRadius := RectSide / Tan(TrueRadius);
    c := ArcTan(Rho / SphereRadius);
    SinC := Sin(C);
    CosC := Cos(C);

    if CosC > 0.0 then
    begin
        Theta := GnomProj.CenterColat -
            ArcTan2(ScaledX * SinC,
                    Rho * Cos(GnomProj.CenterLong) * CosC -
                    ScaledY * Sin(GnomProj.CenterLong) * SinC);
        Phi := ArcSin(CosC * Sin(GnomProj.CenterLong) +
                      ScaledY * SinC * Cos(GnomProj.CenterLong) / Rho);
    end else begin
        Theta := Math.NaN;
        Phi := Math.NaN;
    end;
end;

end.

