unit fastbitmap;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type
    PFastBitmapPixel = ^TFastBitmapPixel;
    TFastBitmapPixel = integer;

    TFastBitmap = class
    private
        FPixelsData: PFastBitmapPixel;
        FSize: TPoint;
        procedure SetSize(const AValue: TPoint);

    public
        constructor Create;
        destructor Destroy; override;
        property Size: TPoint read FSize write SetSize;
        function GetPixelAddress(X, Y: integer): PFastBitmapPixel; inline;
        function GetPixelSize: integer; inline;
    end;

implementation

procedure TFastBitmap.SetSize(const AValue: TPoint);
begin
    if (FSize.X = AValue.X) and (FSize.Y = AValue.Y) then
        Exit;

    if FPixelsData = nil then
        FreeMem(FPixelsData, FSize.X * FSize.Y * Sizeof(TFastBitmapPixel));

    FSize := AValue;
    GetMem(FPixelsData, FSize.X * FSize.Y * SizeOf(TFastBitmapPixel));
end;

constructor TFastBitmap.Create;
begin
    FSize := Point(0, 0);
    FPixelsData := nil;
end;

destructor TFastBitmap.Destroy;
begin
    if FPixelsData <> nil then
        FreeMem(FPixelsData, FSize.X * FSize.Y * SizeOf(TFastBitmapPixel));

    inherited Destroy;
end;

function TFastBitmap.GetPixelAddress(X, Y: integer): PFastBitmapPixel;
begin
    Result := PFastBitmapPixel(FPixelsData) + Y * FSize.X + X;
end;

function TFastBitmap.GetPixelSize: integer;
begin
    Result := SizeOf(TFastBitmapPixel);
end;

end.
