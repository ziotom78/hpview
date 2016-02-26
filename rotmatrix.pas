{ -*- mode: delphi -*- }

unit rotmatrix;

interface

{$mode objfpc}{$h+}

type
    TRotationMatrix = Array[1..3, 1..3] of Double;
    
    TDirection = record
        Theta, Phi : Double;
    end;
    
    TVector = record
        x, y, z : Double;
    end;
    
function Vector(const x, y, z : Double) : TVector;
function VectorDot(const A, B : TVector) : Double;
function VectorLength(const V : TVector) : Double;

function ScaleVector(const V : TVector; Factor : Double) : TVector;
function AddVectors(const A, B : TVector) : TVector;
function SubVectors(const A, B : TVector) : TVector;
    
function RotateVersor(Theta, Phi, Psi : Double;
                      const Versor : TVector) : TVector;
procedure RotationAroundAxis(const Axis : TVector;
                             Angle : Double;
                             var Matrix : TRotationMatrix);
function Rotate(const Mat : TRotationMatrix;
                const x, y, z : Double) : TVector;
function Rotate(const Mat : TRotationMatrix;
                const V : TVector) : TVector;

const
    (* This is the result of the following NumPy code:
    
       healpy.rotator.get_coordconv_matrix(('E', 'G'))[0]
    *)
    EclipticToGalacticMatrix : TRotationMatrix =
        ((-0.054882486, -0.993821033, -0.096476249),
         ( 0.494116468, -0.110993846,  0.862281440),
         (-0.867661702, -0.000346354,  0.497154957));
    
    (* This is the inverse of EclipticToGalacticMatrix *)
    GalacticToEclipticMatrix : TRotationMatrix =
        ((-5.48824861e-02,  4.94116468e-01, -8.67661702e-01),
         (-9.93821034e-01, -1.10993846e-01, -3.46354343e-04),
         (-9.64762486e-02,  8.62281440e-01,  4.97154957e-01));
    
implementation

{------------------------------------------------------------------------------}

function Vector(const x, y, z : Double) : TVector;
begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
end;

{------------------------------------------------------------------------------}

function VectorDot(const A, B : TVector) : Double;
begin
    Result := A.x * B.x + A.y * B.y + A.z * B.z;
end;

{------------------------------------------------------------------------------}

function VectorLength(const V : TVector) : Double;
begin
    Result := Sqrt(VectorDot(V, V));
end;

{------------------------------------------------------------------------------}

function ScaleVector(const V : TVector; Factor : Double) : TVector;
begin
    with Result do
    begin
        x := V.x * Factor;
        y := V.y * Factor;
        z := V.z * Factor;
    end;
end;

{------------------------------------------------------------------------------}

function AddVectors(const A, B : TVector) : TVector;
begin
    with Result do
    begin
        x := A.x + B.x;
        y := A.y + B.y;
        z := A.z + B.z;
    end;
end;

{------------------------------------------------------------------------------}

function SubVectors(const A, B : TVector) : TVector;
begin
    with Result do
    begin
        x := A.x - B.x;
        y := A.y - B.y;
        z := A.z - B.z;
    end;
end;

{------------------------------------------------------------------------------}

function RotateVersor(Theta, Phi, Psi : Double;
                      const Versor : TVector) : TVector;
var
    SinTheta, CosTheta : Double;
    SinPhi, CosPhi : Double;
    SinPsi, CosPsi : Double;
    Vec1x, Vec1y, Vec1z : Double;
    Vec2x, Vec2y, Vec2z : Double;
    
begin
    SinTheta := Sin(Theta); CosTheta := Cos(Theta);
    SinPhi   := Sin(Phi);   CosPhi   := Cos(Phi);
    SinPsi   := Sin(Psi);   CosPsi   := Cos(Psi);

    Vec1x :=  CosPhi * Versor.x + SinPhi * Versor.y;
    Vec1y := -SinPhi * Versor.x + CosPhi * Versor.y;
    Vec1z := Versor.z;

    Vec2x := CosTheta * Vec1x - SinTheta * Vec1z;
    Vec2y := Vec1y;
    Vec2z := SinTheta * Vec1x + CosTheta * Vec1z;

    with Result do
    begin
        x := CosPsi * Vec2x + SinPsi * Vec2y;
        y := -SinPsi * Vec2x + CosPsi * Vec2y;
        z := Vec2z;
    end;
end;

{------------------------------------------------------------------------------}

procedure RotationAroundAxis(const Axis : TVector;
                             Angle : Double;
                             var Matrix : TRotationMatrix);
var
    SinAngle, CosAngle : Double;
    t1, t2 : Double;
    
begin
    SinAngle := Sin(Angle);
    CosAngle := Cos(Angle);
    
    Matrix[1, 1] := Axis.x * Axis.x * (1 - CosAngle) + CosAngle;
    Matrix[2, 2] := Axis.y * Axis.y * (1 - CosAngle) + CosAngle;
    Matrix[3, 3] := Axis.z * Axis.z * (1 - CosAngle) + CosAngle;
    
    t1 := Axis.x * Axis.y * (1 - CosAngle);
    t2 := Axis.z * SinAngle;
    Matrix[2, 1] := t1 + t2;
    Matrix[1, 2] := t1 - t2;
    
    t1 := Axis.x * Axis.z * (1 - CosAngle);
    t2 := Axis.y * SinAngle;
    Matrix[3, 1] := t1 - t2;
    Matrix[1, 3] := t1 + t2;
    
    t1 := Axis.y * Axis.z * (1 - CosAngle);
    t2 := Axis.x * SinAngle;
    Matrix[2, 3] := t1 - t2;
    Matrix[3, 2] := t1 + t2;
end;

{------------------------------------------------------------------------------}

function Rotate(const Mat : TRotationMatrix;
          const x, y, z : Double) : TVector;
begin
    Result.x := x * Mat[1,1] + y * Mat[1,2] + z * Mat[1,3];
    Result.y := x * Mat[2,1] + y * Mat[2,2] + z * Mat[2,3];
    Result.z := x * Mat[3,1] + y * Mat[3,2] + z * Mat[3,3];
end;

{------------------------------------------------------------------------------}

function Rotate(const Mat : TRotationMatrix;
                const V : TVector) : TVector;
begin
    Result := Rotate(Mat, V.x, V.y, V.z);
end;

end.
