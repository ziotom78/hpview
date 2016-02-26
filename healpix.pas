{ -*- mode: delphi -*- }

unit Healpix;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, RotMatrix, Cfitsio;

const
    Unseen = -1.6375e+30;

type
    THealpixError = class(Exception)
    end;

    THealpixOrdering = (Ring, Nested);

    THealpixCoordSys = (csUnknown, csGalactic, csEcliptic);

    PHealpixResolution = ^THealpixResolution;
    THealpixResolution = record
        Nside : Cardinal;
        NsideTimesTwo : Cardinal;
        NsideTimesFour : Cardinal;
        Npix : Cardinal;
        Order : Longint;
        PixelsPerFace : Cardinal;
        Ncap : Cardinal;
        Fact1, Fact2 : Real;
    end;

    PHealpixMap = ^THealpixMap;
    THealpixMap = record
        Pixels : array of Double;
        Resolution : THealpixResolution;
        Ordering : THealpixOrdering;
        CoordSys : THealpixCoordSys;
        Description : String; (* This goes in the TTFORM field of the FITS file *)
        MeasureUnit : String; (* This goes in the TUNIT field of the FITS file *)
    end;

function IsNpixValid(Npix : Cardinal) : Boolean;
function IsNsideValid(Nside : Cardinal) : Boolean;

function NsideToNpix(Nside : Cardinal) : Cardinal;
function NpixToNside(Npix : Cardinal) : Cardinal;

function GetResolution(Nside : Cardinal) : THealpixResolution;
procedure InitMap(Nside : Cardinal;
                  Ordering : THealpixOrdering;
                  out Map : THealpixMap);

{----------------------------------------}

procedure AnglesToVector(Theta, Phi : Double;
                         out x, y, z : Double);

procedure VectorToAngles(x, y, z : Double;
                         out Theta, Phi : Double);

{----------------------------------------}

function AnglesToNestPix(const Resol : THealpixResolution;
                         Theta, Phi : Double) : Cardinal;

function AnglesToRingPix(const Resol : THealpixResolution;
                         Theta, Phi : Double) : Cardinal;

function AnglesToPix(const Map : THealpixMap;
                     Theta, Phi : Double) : Cardinal;

{----------------------------------------}

function VectorToNestPix(const Resol : THealpixResolution;
                         x, y, z : Double) : Cardinal;

function VectorToRingPix(const Resol : THealpixResolution;
                         x, y, z : Double) : Cardinal;

function VectorToPix(const Map : THealpixMap;
                     x, y, z : Double) : Cardinal;

{----------------------------------------}

procedure RingPixToAngles(const Resol : THealpixResolution;
                          Pixel : Cardinal;
                          out Theta, Phi : Double);

procedure NestPixToAngles(const Resol : THealpixResolution;
                          Pixel : Cardinal;
                          out Theta, Phi : Double);

procedure PixToAngles(const Map : THealpixMap;
                      Pixel : Cardinal;
                      out Theta, Phi : Double);

{----------------------------------------}

procedure RingPixToVector(const Resol : THealpixResolution;
                          Pixel : Cardinal;
                          out x, y, z : Double);

procedure NestPixToVector(const Resol : THealpixResolution;
                          Pixel : Cardinal;
                          out x, y, z : Double);

procedure PixToVector(const Map : THealpixMap;
                      Pixel : Cardinal;
                      out x, y, z : Double);

{----------------------------------------}

procedure RotateMap(const InputMap : THealpixMap;
                    const InverseMatrix : TRotationMatrix;
                    out OutputMap : THealpixMap);

(* This version of RotateMap overwrites the pixels in "Map" *)
procedure RotateMap(var Map : THealpixMap;
                    const InverseMatrix : TRotationMatrix);

{----------------------------------------}

procedure ReadHealpixMap(var F : TFitsFile;
                         ColumnNumber : Integer;
                         out Map : THealpixMap);

{ Just write the standard Healpix keywords (e.g., NSIDE, COORDSYS...)
  in an already existing table, but do not create columns nor write
  into them. This is useful if you want to write multiple columns in
  the same table. }
procedure WriteHealpixKeywords(var F : TFitsFile; const Map : THealpixMap);

procedure WriteHealpixMap(var F : TFitsFile;
                          const Map : THealpixMap;
                          FileDataType : TFitsType);

procedure ReadHealpixMap(const FileName : String;
                         HduNumber, ColumnNumber : Integer;
                         out Map : THealpixMap);

procedure ReadHealpixMap(const FileName : String;
                         ColumnNumber : Integer;
                         out Map : THealpixMap);

{ This calls the WriteHealpixMap procedure above, but first it creates
a file with just one HDU and one column in it. }
procedure WriteHealpixMap(const FileName : String;
                          const Map : THealpixMap;
                          FileDataType : TFitsType);

implementation

uses Math;

const

    NSIDE_MAX = 8192;

{------------------------------------------------------------------------------}

    XToPix: array[0..127] of Longint =
      ( 0,     1,     4,     5,    16,    17,    20,    21,    64,    65,
       68,    69,    80,    81,    84,    85,   256,   257,   260,   261,
      272,   273,   276,   277,   320,   321,   324,   325,   336,   337,
      340,   341,  1024,  1025,  1028,  1029,  1040,  1041,  1044,  1045,
     1088,  1089,  1092,  1093,  1104,  1105,  1108,  1109,  1280,  1281,
     1284,  1285,  1296,  1297,  1300,  1301,  1344,  1345,  1348,  1349,
     1360,  1361,  1364,  1365,  4096,  4097,  4100,  4101,  4112,  4113,
     4116,  4117,  4160,  4161,  4164,  4165,  4176,  4177,  4180,  4181,
     4352,  4353,  4356,  4357,  4368,  4369,  4372,  4373,  4416,  4417,
     4420,  4421,  4432,  4433,  4436,  4437,  5120,  5121,  5124,  5125,
     5136,  5137,  5140,  5141,  5184,  5185,  5188,  5189,  5200,  5201,
     5204,  5205,  5376,  5377,  5380,  5381,  5392,  5393,  5396,  5397,
     5440,  5441,  5444,  5445,  5456,  5457,  5460,  5461 );

{------------------------------------------------------------------------------}

    YToPix: array[0..127] of Longint =
      ( 0,     2,     8,    10,    32,    34,    40,    42,   128,   130,
      136,   138,   160,   162,   168,   170,   512,   514,   520,   522,
      544,   546,   552,   554,   640,   642,   648,   650,   672,   674,
      680,   682,  2048,  2050,  2056,  2058,  2080,  2082,  2088,  2090,
     2176,  2178,  2184,  2186,  2208,  2210,  2216,  2218,  2560,  2562,
     2568,  2570,  2592,  2594,  2600,  2602,  2688,  2690,  2696,  2698,
     2720,  2722,  2728,  2730,  8192,  8194,  8200,  8202,  8224,  8226,
     8232,  8234,  8320,  8322,  8328,  8330,  8352,  8354,  8360,  8362,
     8704,  8706,  8712,  8714,  8736,  8738,  8744,  8746,  8832,  8834,
     8840,  8842,  8864,  8866,  8872,  8874, 10240, 10242, 10248, 10250,
    10272, 10274, 10280, 10282, 10368, 10370, 10376, 10378, 10400, 10402,
    10408, 10410, 10752, 10754, 10760, 10762, 10784, 10786, 10792, 10794,
    10880, 10882, 10888, 10890, 10912, 10914, 10920, 10922 );

    PixToX: array[0..1023] of Integer =
	(  0,    1,    0,    1,    2,    3,    2,    3,    0,    1,
	   0,    1,    2,    3,    2,    3,    4,    5,    4,    5,
	   6,    7,    6,    7,    4,    5,    4,    5,    6,    7,
	   6,    7,    0,    1,    0,    1,    2,    3,    2,    3,
	   0,    1,    0,    1,    2,    3,    2,    3,    4,    5,
	   4,    5,    6,    7,    6,    7,    4,    5,    4,    5,
	   6,    7,    6,    7,    8,    9,    8,    9,   10,   11,
	  10,   11,    8,    9,    8,    9,   10,   11,   10,   11,
	  12,   13,   12,   13,   14,   15,   14,   15,   12,   13,
	  12,   13,   14,   15,   14,   15,    8,    9,    8,    9,
	  10,   11,   10,   11,    8,    9,    8,    9,   10,   11,
	  10,   11,   12,   13,   12,   13,   14,   15,   14,   15,
	  12,   13,   12,   13,   14,   15,   14,   15,    0,    1,
	   0,    1,    2,    3,    2,    3,    0,    1,    0,    1,
	   2,    3,    2,    3,    4,    5,    4,    5,    6,    7,
	   6,    7,    4,    5,    4,    5,    6,    7,    6,    7,
	   0,    1,    0,    1,    2,    3,    2,    3,    0,    1,
	   0,    1,    2,    3,    2,    3,    4,    5,    4,    5,
	   6,    7,    6,    7,    4,    5,    4,    5,    6,    7,
	   6,    7,    8,    9,    8,    9,   10,   11,   10,   11,
	   8,    9,    8,    9,   10,   11,   10,   11,   12,   13,
	  12,   13,   14,   15,   14,   15,   12,   13,   12,   13,
	  14,   15,   14,   15,    8,    9,    8,    9,   10,   11,
	  10,   11,    8,    9,    8,    9,   10,   11,   10,   11,
	  12,   13,   12,   13,   14,   15,   14,   15,   12,   13,
	  12,   13,   14,   15,   14,   15,   16,   17,   16,   17,
	  18,   19,   18,   19,   16,   17,   16,   17,   18,   19,
	  18,   19,   20,   21,   20,   21,   22,   23,   22,   23,
	  20,   21,   20,   21,   22,   23,   22,   23,   16,   17,
	  16,   17,   18,   19,   18,   19,   16,   17,   16,   17,
	  18,   19,   18,   19,   20,   21,   20,   21,   22,   23,
	  22,   23,   20,   21,   20,   21,   22,   23,   22,   23,
	  24,   25,   24,   25,   26,   27,   26,   27,   24,   25,
	  24,   25,   26,   27,   26,   27,   28,   29,   28,   29,
	  30,   31,   30,   31,   28,   29,   28,   29,   30,   31,
	  30,   31,   24,   25,   24,   25,   26,   27,   26,   27,
	  24,   25,   24,   25,   26,   27,   26,   27,   28,   29,
	  28,   29,   30,   31,   30,   31,   28,   29,   28,   29,
	  30,   31,   30,   31,   16,   17,   16,   17,   18,   19,
	  18,   19,   16,   17,   16,   17,   18,   19,   18,   19,
	  20,   21,   20,   21,   22,   23,   22,   23,   20,   21,
	  20,   21,   22,   23,   22,   23,   16,   17,   16,   17,
	  18,   19,   18,   19,   16,   17,   16,   17,   18,   19,
	  18,   19,   20,   21,   20,   21,   22,   23,   22,   23,
	  20,   21,   20,   21,   22,   23,   22,   23,   24,   25,
	  24,   25,   26,   27,   26,   27,   24,   25,   24,   25,
	  26,   27,   26,   27,   28,   29,   28,   29,   30,   31,
	  30,   31,   28,   29,   28,   29,   30,   31,   30,   31,
	  24,   25,   24,   25,   26,   27,   26,   27,   24,   25,
	  24,   25,   26,   27,   26,   27,   28,   29,   28,   29,
	  30,   31,   30,   31,   28,   29,   28,   29,   30,   31,
	  30,   31,    0,    1,    0,    1,    2,    3,    2,    3,
	   0,    1,    0,    1,    2,    3,    2,    3,    4,    5,
	   4,    5,    6,    7,    6,    7,    4,    5,    4,    5,
	   6,    7,    6,    7,    0,    1,    0,    1,    2,    3,
	   2,    3,    0,    1,    0,    1,    2,    3,    2,    3,
	   4,    5,    4,    5,    6,    7,    6,    7,    4,    5,
	   4,    5,    6,    7,    6,    7,    8,    9,    8,    9,
	  10,   11,   10,   11,    8,    9,    8,    9,   10,   11,
	  10,   11,   12,   13,   12,   13,   14,   15,   14,   15,
	  12,   13,   12,   13,   14,   15,   14,   15,    8,    9,
	   8,    9,   10,   11,   10,   11,    8,    9,    8,    9,
	  10,   11,   10,   11,   12,   13,   12,   13,   14,   15,
	  14,   15,   12,   13,   12,   13,   14,   15,   14,   15,
	   0,    1,    0,    1,    2,    3,    2,    3,    0,    1,
	   0,    1,    2,    3,    2,    3,    4,    5,    4,    5,
	   6,    7,    6,    7,    4,    5,    4,    5,    6,    7,
	   6,    7,    0,    1,    0,    1,    2,    3,    2,    3,
	   0,    1,    0,    1,    2,    3,    2,    3,    4,    5,
	   4,    5,    6,    7,    6,    7,    4,    5,    4,    5,
	   6,    7,    6,    7,    8,    9,    8,    9,   10,   11,
	  10,   11,    8,    9,    8,    9,   10,   11,   10,   11,
	  12,   13,   12,   13,   14,   15,   14,   15,   12,   13,
	  12,   13,   14,   15,   14,   15,    8,    9,    8,    9,
	  10,   11,   10,   11,    8,    9,    8,    9,   10,   11,
	  10,   11,   12,   13,   12,   13,   14,   15,   14,   15,
	  12,   13,   12,   13,   14,   15,   14,   15,   16,   17,
	  16,   17,   18,   19,   18,   19,   16,   17,   16,   17,
	  18,   19,   18,   19,   20,   21,   20,   21,   22,   23,
	  22,   23,   20,   21,   20,   21,   22,   23,   22,   23,
	  16,   17,   16,   17,   18,   19,   18,   19,   16,   17,
	  16,   17,   18,   19,   18,   19,   20,   21,   20,   21,
	  22,   23,   22,   23,   20,   21,   20,   21,   22,   23,
	  22,   23,   24,   25,   24,   25,   26,   27,   26,   27,
	  24,   25,   24,   25,   26,   27,   26,   27,   28,   29,
	  28,   29,   30,   31,   30,   31,   28,   29,   28,   29,
	  30,   31,   30,   31,   24,   25,   24,   25,   26,   27,
	  26,   27,   24,   25,   24,   25,   26,   27,   26,   27,
	  28,   29,   28,   29,   30,   31,   30,   31,   28,   29,
	  28,   29,   30,   31,   30,   31,   16,   17,   16,   17,
	  18,   19,   18,   19,   16,   17,   16,   17,   18,   19,
	  18,   19,   20,   21,   20,   21,   22,   23,   22,   23,
	  20,   21,   20,   21,   22,   23,   22,   23,   16,   17,
	  16,   17,   18,   19,   18,   19,   16,   17,   16,   17,
	  18,   19,   18,   19,   20,   21,   20,   21,   22,   23,
	  22,   23,   20,   21,   20,   21,   22,   23,   22,   23,
	  24,   25,   24,   25,   26,   27,   26,   27,   24,   25,
	  24,   25,   26,   27,   26,   27,   28,   29,   28,   29,
	  30,   31,   30,   31,   28,   29,   28,   29,   30,   31,
	  30,   31,   24,   25,   24,   25,   26,   27,   26,   27,
	  24,   25,   24,   25,   26,   27,   26,   27,   28,   29,
	  28,   29,   30,   31,   30,   31,   28,   29,   28,   29,
	  30,   31,   30,   31 );

    PixToY : Array[0..1023] of Integer =
	(  0,    0,    1,    1,    0,    0,    1,    1,    2,    2,
	   3,    3,    2,    2,    3,    3,    0,    0,    1,    1,
	   0,    0,    1,    1,    2,    2,    3,    3,    2,    2,
	   3,    3,    4,    4,    5,    5,    4,    4,    5,    5,
	   6,    6,    7,    7,    6,    6,    7,    7,    4,    4,
	   5,    5,    4,    4,    5,    5,    6,    6,    7,    7,
	   6,    6,    7,    7,    0,    0,    1,    1,    0,    0,
	   1,    1,    2,    2,    3,    3,    2,    2,    3,    3,
	   0,    0,    1,    1,    0,    0,    1,    1,    2,    2,
	   3,    3,    2,    2,    3,    3,    4,    4,    5,    5,
	   4,    4,    5,    5,    6,    6,    7,    7,    6,    6,
	   7,    7,    4,    4,    5,    5,    4,    4,    5,    5,
	   6,    6,    7,    7,    6,    6,    7,    7,    8,    8,
	   9,    9,    8,    8,    9,    9,   10,   10,   11,   11,
	  10,   10,   11,   11,    8,    8,    9,    9,    8,    8,
	   9,    9,   10,   10,   11,   11,   10,   10,   11,   11,
	  12,   12,   13,   13,   12,   12,   13,   13,   14,   14,
	  15,   15,   14,   14,   15,   15,   12,   12,   13,   13,
	  12,   12,   13,   13,   14,   14,   15,   15,   14,   14,
	  15,   15,    8,    8,    9,    9,    8,    8,    9,    9,
	  10,   10,   11,   11,   10,   10,   11,   11,    8,    8,
	   9,    9,    8,    8,    9,    9,   10,   10,   11,   11,
	  10,   10,   11,   11,   12,   12,   13,   13,   12,   12,
	  13,   13,   14,   14,   15,   15,   14,   14,   15,   15,
	  12,   12,   13,   13,   12,   12,   13,   13,   14,   14,
	  15,   15,   14,   14,   15,   15,    0,    0,    1,    1,
	   0,    0,    1,    1,    2,    2,    3,    3,    2,    2,
	   3,    3,    0,    0,    1,    1,    0,    0,    1,    1,
	   2,    2,    3,    3,    2,    2,    3,    3,    4,    4,
	   5,    5,    4,    4,    5,    5,    6,    6,    7,    7,
	   6,    6,    7,    7,    4,    4,    5,    5,    4,    4,
	   5,    5,    6,    6,    7,    7,    6,    6,    7,    7,
	   0,    0,    1,    1,    0,    0,    1,    1,    2,    2,
	   3,    3,    2,    2,    3,    3,    0,    0,    1,    1,
	   0,    0,    1,    1,    2,    2,    3,    3,    2,    2,
	   3,    3,    4,    4,    5,    5,    4,    4,    5,    5,
	   6,    6,    7,    7,    6,    6,    7,    7,    4,    4,
	   5,    5,    4,    4,    5,    5,    6,    6,    7,    7,
	   6,    6,    7,    7,    8,    8,    9,    9,    8,    8,
	   9,    9,   10,   10,   11,   11,   10,   10,   11,   11,
	   8,    8,    9,    9,    8,    8,    9,    9,   10,   10,
	  11,   11,   10,   10,   11,   11,   12,   12,   13,   13,
	  12,   12,   13,   13,   14,   14,   15,   15,   14,   14,
	  15,   15,   12,   12,   13,   13,   12,   12,   13,   13,
	  14,   14,   15,   15,   14,   14,   15,   15,    8,    8,
	   9,    9,    8,    8,    9,    9,   10,   10,   11,   11,
	  10,   10,   11,   11,    8,    8,    9,    9,    8,    8,
	   9,    9,   10,   10,   11,   11,   10,   10,   11,   11,
	  12,   12,   13,   13,   12,   12,   13,   13,   14,   14,
	  15,   15,   14,   14,   15,   15,   12,   12,   13,   13,
	  12,   12,   13,   13,   14,   14,   15,   15,   14,   14,
	  15,   15,   16,   16,   17,   17,   16,   16,   17,   17,
	  18,   18,   19,   19,   18,   18,   19,   19,   16,   16,
	  17,   17,   16,   16,   17,   17,   18,   18,   19,   19,
	  18,   18,   19,   19,   20,   20,   21,   21,   20,   20,
	  21,   21,   22,   22,   23,   23,   22,   22,   23,   23,
	  20,   20,   21,   21,   20,   20,   21,   21,   22,   22,
	  23,   23,   22,   22,   23,   23,   16,   16,   17,   17,
	  16,   16,   17,   17,   18,   18,   19,   19,   18,   18,
	  19,   19,   16,   16,   17,   17,   16,   16,   17,   17,
	  18,   18,   19,   19,   18,   18,   19,   19,   20,   20,
	  21,   21,   20,   20,   21,   21,   22,   22,   23,   23,
	  22,   22,   23,   23,   20,   20,   21,   21,   20,   20,
	  21,   21,   22,   22,   23,   23,   22,   22,   23,   23,
	  24,   24,   25,   25,   24,   24,   25,   25,   26,   26,
	  27,   27,   26,   26,   27,   27,   24,   24,   25,   25,
	  24,   24,   25,   25,   26,   26,   27,   27,   26,   26,
	  27,   27,   28,   28,   29,   29,   28,   28,   29,   29,
	  30,   30,   31,   31,   30,   30,   31,   31,   28,   28,
	  29,   29,   28,   28,   29,   29,   30,   30,   31,   31,
	  30,   30,   31,   31,   24,   24,   25,   25,   24,   24,
	  25,   25,   26,   26,   27,   27,   26,   26,   27,   27,
	  24,   24,   25,   25,   24,   24,   25,   25,   26,   26,
	  27,   27,   26,   26,   27,   27,   28,   28,   29,   29,
	  28,   28,   29,   29,   30,   30,   31,   31,   30,   30,
	  31,   31,   28,   28,   29,   29,   28,   28,   29,   29,
	  30,   30,   31,   31,   30,   30,   31,   31,   16,   16,
	  17,   17,   16,   16,   17,   17,   18,   18,   19,   19,
	  18,   18,   19,   19,   16,   16,   17,   17,   16,   16,
	  17,   17,   18,   18,   19,   19,   18,   18,   19,   19,
	  20,   20,   21,   21,   20,   20,   21,   21,   22,   22,
	  23,   23,   22,   22,   23,   23,   20,   20,   21,   21,
	  20,   20,   21,   21,   22,   22,   23,   23,   22,   22,
	  23,   23,   16,   16,   17,   17,   16,   16,   17,   17,
	  18,   18,   19,   19,   18,   18,   19,   19,   16,   16,
	  17,   17,   16,   16,   17,   17,   18,   18,   19,   19,
	  18,   18,   19,   19,   20,   20,   21,   21,   20,   20,
	  21,   21,   22,   22,   23,   23,   22,   22,   23,   23,
	  20,   20,   21,   21,   20,   20,   21,   21,   22,   22,
	  23,   23,   22,   22,   23,   23,   24,   24,   25,   25,
	  24,   24,   25,   25,   26,   26,   27,   27,   26,   26,
	  27,   27,   24,   24,   25,   25,   24,   24,   25,   25,
	  26,   26,   27,   27,   26,   26,   27,   27,   28,   28,
	  29,   29,   28,   28,   29,   29,   30,   30,   31,   31,
	  30,   30,   31,   31,   28,   28,   29,   29,   28,   28,
	  29,   29,   30,   30,   31,   31,   30,   30,   31,   31,
	  24,   24,   25,   25,   24,   24,   25,   25,   26,   26,
	  27,   27,   26,   26,   27,   27,   24,   24,   25,   25,
	  24,   24,   25,   25,   26,   26,   27,   27,   26,   26,
	  27,   27,   28,   28,   29,   29,   28,   28,   29,   29,
	  30,   30,   31,   31,   30,   30,   31,   31,   28,   28,
	  29,   29,   28,   28,   29,   29,   30,   30,   31,   31,
	  30,   30,   31,   31 );

function ILog2(x : Longint) : Longint;
var
    ShiftedX : Longint;
begin
    ShiftedX := x;
    Result := 0;

    while ShiftedX > $0000FFFF do
    begin
	Inc(Result, 16);
	ShiftedX := ShiftedX shr 16;
    end;

    if ShiftedX > $000000FF then
    begin
	Result := Result or 8;
	ShiftedX := ShiftedX shr 8;
    end;

    if ShiftedX > $0000000F then
    begin
	Result := Result or 4;
	ShiftedX := ShiftedX shr 4;
    end;

    if ShiftedX > $00000003 then
    begin
	Result := Result or 2;
	ShiftedX := ShiftedX shr 2;
    end;

    if ShiftedX > $00000001 then
	Result := Result or 1;
end;

{------------------------------------------------------------------------------}

function IsNpixValid(Npix : Cardinal) : Boolean;
var
    Nside : Cardinal;
begin
    if Npix mod 12 <> 0 then
    begin
       Result := False;
    end else begin
        Nside := Round(Sqrt(Npix div 12));
        Result := Nside * Nside * 12 = Npix;
    end;
end;

{------------------------------------------------------------------------------}

function IsNsideValid(Nside : Cardinal) : Boolean;
begin
    Result := (Nside and (Nside - 1)) = 0;
end;

{------------------------------------------------------------------------------}

function NsideToNpix(Nside : Cardinal) : Cardinal;
begin
    if not IsNsideValid(Nside) then
    begin
        raise THealpixError(Format('Invalid value for NSIDE: %d', [Nside]));
    end;

    NsideToNpix := 12 * Nside * Nside;
end;

{------------------------------------------------------------------------------}

function NpixToNside(Npix : Cardinal) : Cardinal;
begin
    if not IsNpixValid(Npix) then
    begin
        raise THealpixError(Format('Invalid number of pixels: %d', [Npix]));
    end;

    Result := Round(Sqrt(Npix div 12));
end;

{------------------------------------------------------------------------------}

function GetResolution(Nside : Cardinal) : THealpixResolution;
begin
    Result.Nside := Nside;
    with Result do
    begin
        NsideTimesTwo := 2 * Nside;
        NsideTimesFour := 4 * Nside;
        Npix := NsideToNpix(Nside);

        if IsNsideValid(Nside) then
           Order := Ilog2(Nside)
        else
            Order := -1;

        PixelsPerFace := Nside * Nside;
        Ncap := 2 * (PixelsPerFace - Nside);
        Fact2 := 4.0 / Npix;
        Fact1 := 2 * Nside * Fact2;
    end;
end;

{------------------------------------------------------------------------------}

procedure InitMap(Nside : Cardinal;
                  Ordering : THealpixOrdering;
                  out Map : THealpixMap);
begin
    Map.Resolution := GetResolution(Nside);
    Map.Ordering := Ordering;
    Map.Description := '64-bit float';
    Map.MeasureUnit := '';
    SetLength(Map.Pixels, Map.Resolution.Npix);
    FillByte(Map.Pixels[0], SizeOf(Map.Pixels[0]) * Map.Resolution.Npix, 0);
end;

{------------------------------------------------------------------------------}

function NormalizeAngle(x : Double) : Double;
begin
    Result := x;
    while Result >= 2 * PI do
        Result := Result - 2 * PI;

    while Result < 0.0 do
        Result := Result + 2 * PI;
end;

{------------------------------------------------------------------------------}

procedure CalcNestPosForEquator(Z, ScaledPhi : Double;
                                out ix, iy, FaceNum : Integer);
var
    jp, jm : Integer;
    idfp, idfm : Integer;

begin
    jp := Floor(NSIDE_MAX * (0.5 + ScaledPhi - z * 0.75));
    jm := Floor(NSIDE_MAX * (0.5 + ScaledPhi + z * 0.75));

    idfp := jp div NSIDE_MAX; // in {0,4}
    idfm := jm div NSIDE_MAX;

    if idfp = idfm then
        FaceNum := (idfp mod 4) + 4
    else if idfp < idfm then
        FaceNum := (idfp mod 4)
    else
        FaceNum := (idfm mod 4) + 8;

    ix := jm mod NSIDE_MAX;
    iy := NSIDE_MAX - (jp mod NSIDE_MAX) - 1;
end;

{------------------------------------------------------------------------------}

procedure CalcNestPosForPole(Z, ZAbs, ScaledPhi : Double;
                             out ix, iy, FaceNum : Integer);
var
    ntt : Integer;
    tp, tmp : Double;
    jp, jm : Integer;

begin
    ntt := Floor(ScaledPhi);
    if ntt >= 4 then
        ntt := 3;

    tp := ScaledPhi - ntt;
    tmp := sqrt (3. * (1. - ZAbs)); // in ]0,1]

    jp := floor(NSIDE_MAX * tp * tmp);
    jm := floor(NSIDE_MAX * (1. - tp) * tmp);

    { Clip jp and jm }

    if jp > NSIDE_MAX - 1 then
        jp := NSIDE_MAX - 1;
    if jm > NSIDE_MAX - 1 then
        jm := NSIDE_MAX - 1;

    if z >= 0 then
    begin
	FaceNum := ntt; // in {0,3}
	ix := NSIDE_MAX - jm - 1;
	iy := NSIDE_MAX - jp - 1;
    end else
    begin
	FaceNum := ntt + 8; // in {8,11}
	ix := jp;
	iy := jm;
    end
end;

{------------------------------------------------------------------------------}

procedure AnglesToVector(Theta, Phi : Double;
                         out x, y, z : Double);
var
    SinTheta : Double;

begin
    SinTheta := Sin(Theta);
    x := SinTheta * Cos(Phi);
    y := SinTheta * Sin(Phi);
    z := Cos(Theta);
end;

{------------------------------------------------------------------------------}

procedure VectorToAngles(x, y, z : Double;
                         out Theta, Phi : Double);
var
    VectorLength : Double;

begin
    VectorLength := Sqrt(Sqr(x) + Sqr(y) + Sqr(z));
    Theta := ArcCos(z / VectorLength);
    Phi := ArcTan2(y, x);
    Phi := NormalizeAngle(Phi);
end;

{------------------------------------------------------------------------------}

function AnglesToNestPix(const Resol : THealpixResolution;
                         Theta, Phi : Double) : Cardinal;
var
    ix, iy, FaceNum : Integer;
    z, ZAbs, ScaledPhi : Double;
    ipf : Longint;
    ix_hi, ix_low : Integer;
    iy_hi, iy_low : Integer;

begin
    z := Cos(Theta);
    ZAbs := Abs(z);
    ScaledPhi := NormalizeAngle(phi) / (0.5 * PI); // in [0,4[

    if ZAbs <= 2./3. then
        CalcNestPosForEquator(z, ScaledPhi, ix, iy, FaceNum)
    else
        CalcNestPosForPole(z, ZAbs, ScaledPhi, ix, iy, FaceNum);

    {$warn 5057 off}
    DivMod(ix, 128, ix_hi, ix_low);
    DivMod(iy, 128, iy_hi, iy_low);
    {$warn 5057 on}

    ipf := (XToPix[ix_hi]  + YToPix[iy_hi]) * (128 * 128)
	+ (XToPix[ix_low] + YToPix[iy_low]);
    ipf := Floor(ipf / ((NSIDE_MAX / Resol.Nside) ** 2));

    Result := ipf + FaceNum * (Resol.Nside ** 2);
end;

{------------------------------------------------------------------------------}

function AnglesToRingPix(const Resol : THealpixResolution;
                         Theta, Phi : Double) : Cardinal;
var
    jp, jm, ipix1 : Integer;
    ir, ip : Integer;
    z, ZAbs : Double;
    tt : Double;

    procedure EquatorPixel;
    var
        KShift : Integer;
    begin
        jp := Floor(Resol.Nside * (0.5 + tt - z * 0.75));
		jm := Floor(Resol.Nside * (0.5 + tt + z * 0.75));

        ir := Resol.Nside + 1 + jp - jm;
        KShift := 0;
        if ir mod 2 = 0 then
            KShift := 1;

        ip := Floor((jp + jm - Resol.Nside + KShift + 1) / 2) + 1;
        if ip > Resol.NsideTimesFour then
            Dec(ip, Resol.NsideTimesFour);

        ipix1 := Resol.Ncap + Resol.NsideTimesFour * (ir - 1) + ip;
    end;

    procedure PolePixel;
    var
        tp, tmp : Double;

    begin
        tp := tt - floor(tt);
        tmp := Sqrt(3. * (1. - ZAbs));

        jp := Floor(Resol.Nside * tp * tmp );
        jm := Floor(Resol.Nside * (1. - tp) * tmp);

        ir := jp + jm + 1;
        ip := Floor(tt * ir) + 1;
        if ip > 4 * ir then
            Dec(ip, 4 * ir);

        ipix1 := 2 * ir * (ir - 1) + ip;
        if z <= 0. then
            ipix1 := Resol.Npix - 2 * ir * (ir + 1) + ip;
    end;

begin
    z := cos(theta);
    ZAbs := Abs(z);
    phi := NormalizeAngle(phi);
    tt := phi / (0.5 * PI);

    if ZAbs <= 2./3. then
        EquatorPixel
    else
        PolePixel;

    Result := ipix1 - 1;
end;

{------------------------------------------------------------------------------}

function AnglesToPix(const Map : THealpixMap;
                     Theta, Phi : Double) : Cardinal;
begin
    case Map.Ordering of
    Ring: Result := AnglesToRingPix(Map.Resolution, Theta, Phi);
    Nested: Result := AnglesToNestPix(Map.Resolution, Theta, Phi);
    end;
end;

{------------------------------------------------------------------------------}

function VectorToNestPix(const Resol : THealpixResolution;
                         x, y, z : Double) : Cardinal;
var
    Theta, Phi : Double;

begin
    VectorToAngles(x, y, z, Theta, Phi);
    Result := AnglesToNestPix(Resol, Theta, Phi);
end;

{------------------------------------------------------------------------------}

function VectorToRingPix(const Resol : THealpixResolution;
                         x, y, z : Double) : Cardinal;
var
    Theta, Phi : Double;

begin
    VectorToAngles(x, y, z, Theta, Phi);
    Result := AnglesToRingPix(Resol, Theta, Phi);
end;

{------------------------------------------------------------------------------}

function VectorToPix(const Map : THealpixMap;
                     x, y, z : Double) : Cardinal;
var
    Theta, Phi : Double;

begin
    VectorToAngles(x, y, z, Theta, Phi);
    Result := AnglesToPix(Map, Theta, Phi);
end;

{------------------------------------------------------------------------------}

procedure RingPixToAngles(const Resol : THealpixResolution;
                          Pixel : Cardinal;
                          out Theta, Phi : Double);
var
    IRing, IPhi, Ip, Ipix1 : Integer;
    Fact1, Fact2, Fodd, Hip, FiHip : Double;

begin
    Ipix1 := Pixel + 1; // in {1, npix}
    Fact1 := 1.5 * Resol.Nside;
    Fact2 := 3.0 * Resol.PixelsPerFace;

    if Ipix1 <= Resol.Ncap then
    begin
        //! North Polar cap -------------
	Hip   := Ipix1 / 2.;
	FiHip := Floor(Hip);
	IRing := Floor(Sqrt(Hip - Sqrt(FiHip))) + 1; // counted from North pole
	IPhi  := Ipix1 - 2 * IRing * (IRing - 1);

	Theta := ArcCos(1. - IRing*IRing / fact2);
	Phi   := (1.*IPhi - 0.5) * Pi / (2. * IRing);
    end else if Ipix1 <= Resol.NsideTimesTwo * (5 * Resol.Nside + 1) then
    begin // Equatorial region ------
	Ip := Ipix1 - Resol.Ncap - 1;
	IRing := Floor(ip / Resol.NsideTimesFour) + Resol.Nside; // counted from North pole
	IPhi := (ip mod Resol.NsideTimesFour) + 1;

        // 1 if IRing + Resol.Nside is odd, 1/2 otherwise
	Fodd  := 0.5 * (1 + ((IRing + Resol.Nside) mod 2));
	Theta := ArcCos( (Resol.NsideTimesTwo - IRing) / fact1 );
	Phi := (1.*IPhi - fodd) * Pi /(2. * Resol.Nside);
    end else begin //! South Polar cap -----------------------------------
	ip := Resol.Npix - Ipix1 + 1;
	hip := ip / 2.;
	fihip := Floor(hip);
	IRing := Floor(Sqrt(Hip - Sqrt(FiHip))) + 1;//     ! counted from South pole
	IPhi := (4 * IRing + 1 - (Ip - 2 * IRing * (IRing - 1)));

	Theta := ArcCos(IRing * IRing / fact2 - 1.0);
	Phi := (1. * IPhi - 0.5) * Pi / (2. * IRing);
    end;
end;

{------------------------------------------------------------------------------}

procedure NestPixToAngles(const Resol : THealpixResolution;
                          Pixel : Cardinal;
                          out Theta, Phi : Double);
const
    jrll : Array[0..11] of Integer = ( 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4 );
    jpll : Array[0..11] of Integer = ( 1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7 );

var
    ix, iy, jrt, jr, nr, jpt, jp, kshift : Integer;
    z, PiOverTwo, fn, fact1, fact2 : Double;
    FaceNum, ipf, ip_low, ip_trunc, ip_med, ip_hi : Integer;

begin
    PiOverTwo := 0.5 * Pi;
    fn := 1. * Resol.Nside;
    fact1 := 1./(3. * fn * fn);
    fact2 := 2./(3. * fn);

    FaceNum := pixel div Resol.PixelsPerFace; // face number in {0,11}
    ipf := pixel mod Resol.PixelsPerFace; // pixel number in the face {0,Resol.PixelsPerFace-1}

    ip_low := ipf mod 1024; // content of the last 10 bits
    ip_trunc := ipf div 1024; // truncation of the last 10 bits
    ip_med := ip_trunc mod 1024; // content of the next 10 bits
    ip_hi  := ip_trunc div 1024; // content of the high weight 10 bits

    ix := 1024 * PixToX[ip_hi] + 32 * PixToX[ip_med] + PixToX[ip_low];
    iy := 1024 * PixToY[ip_hi] + 32 * PixToY[ip_med] + PixToY[ip_low];

    // Transforms this in (horizontal, vertical) coordinates
    jrt := ix + iy; // 'vertical' in {0,2*(Resol.Nside-1)}
    jpt := ix - iy; // 'horizontal' in {-Resol.Nside+1,Resol.Nside-1}

    jr :=  jrll[FaceNum] * Resol.Nside - jrt - 1;
    nr := Resol.Nside; // Equatorial region (the most frequent)
    z  := (2 * Resol.Nside - jr) * fact2;
    kshift := (jr - Resol.Nside) mod 2;
    if jr < Resol.Nside then
    begin
	nr := jr;
	z := 1. - nr * nr * fact1;
	kshift := 0;
    end else begin
	if(jr > 3 * Resol.Nside) then
	begin
	    nr := Resol.NsideTimesFour - jr;
	    z := - 1. + nr * nr * fact1;
	    kshift := 0;
	end;
    end;
    Theta := ArcCos(z);

    jp := (jpll[FaceNum] * nr + jpt + 1 + kshift) div 2;
    if jp > Resol.NsideTimesFour then
        jp := jp - Resol.NsideTimesFour
    else if jp < 1 then
        jp := jp + Resol.NsideTimesFour;

    Phi := (jp - (kshift + 1) * 0.5) * (PiOverTwo / nr);
end;

{------------------------------------------------------------------------------}

procedure PixToAngles(const Map : THealpixMap;
                      Pixel : Cardinal;
                      out Theta, Phi : Double);
begin
    case Map.Ordering of
    Ring: RingPixToAngles(Map.Resolution, Pixel, Theta, Phi);
    Nested: NestPixToAngles(Map.Resolution, Pixel, Theta, Phi);
    end;
end;

{------------------------------------------------------------------------------}

procedure RingPixToVector(const Resol : THealpixResolution;
                          Pixel : Cardinal;
                          out x, y, z : Double);
var
    Theta, Phi : Double;

begin
    RingPixToAngles(Resol, Pixel, Theta, Phi);
    AnglesToVector(Theta, Phi, x, y, z);
end;

procedure NestPixToVector(const Resol : THealpixResolution;
                          Pixel : Cardinal;
                          out x, y, z : Double);
var
    Theta, Phi : Double;

begin
    NestPixToAngles(Resol, Pixel, Theta, Phi);
    AnglesToVector(Theta, Phi, x, y, z);
end;

procedure PixToVector(const Map : THealpixMap;
                      Pixel : Cardinal;
                      out x, y, z : Double);
begin
    case Map.Ordering of
    Ring: RingPixToVector(Map.Resolution, Pixel, x, y, z);
    Nested: NestPixToVector(Map.Resolution, Pixel, x, y, z);
    end;
end;

{------------------------------------------------------------------------------}

{ Note that you must specify the *inverse* matrix required to perform
  the rotation. This is because the pixels in the output map are
  constructed by turning back to the input map coordinate system (in
  order to avoid holes in the output map).

  The callback function is called at the beginning and at the end of the
  computation, plus a fixed number of times during the computation itself. }

procedure RotateMap(const InputMap : THealpixMap;
                    const InverseMatrix : TRotationMatrix;
                    out OutputMap : THealpixMap);
var
    InputIdx, OutputIdx : Cardinal;
    OriginalVect, RotatedVect : TVector;

begin
    InitMap(InputMap.Resolution.Nside,
            InputMap.Ordering,
            OutputMap);

    for OutputIdx := Low(OutputMap.Pixels) to High(OutputMap.Pixels) do
    begin
        PixToVector(OutputMap, OutputIdx,
                    RotatedVect.x, RotatedVect.y, RotatedVect.z);
        OriginalVect := Rotate(InverseMatrix, RotatedVect);
        InputIdx := VectorToPix(InputMap,
                                 OriginalVect.x, OriginalVect.y, OriginalVect.z);
        OutputMap.Pixels[OutputIdx] := InputMap.Pixels[InputIdx];
    end;
end;

{------------------------------------------------------------------------------}

procedure RotateMap(var Map : THealpixMap;
                    const InverseMatrix : TRotationMatrix);
var
    TempMap : THealpixMap;

begin
    RotateMap(Map, InverseMatrix, TempMap);
    Move(TempMap.Pixels[0], Map.Pixels[0],
         SizeOf(Map.Pixels[0]) * Length(Map.Pixels));
end;

{------------------------------------------------------------------------------}

procedure ReadHealpixMap(var F : TFitsFile;
                         ColumnNumber : Integer;
                         out Map : THealpixMap);
var
    Nside : Integer;
    OrderingStr : String;
    CoordSysStr : String;

begin
    Nside := Cfitsio.ReadKeyAsInteger(F, 'NSIDE');
    if not IsNsideValid(Nside) then
        raise THealpixError.CreateFmt('Invalid NSIDE value for map %s: %d',
                                      [Cfitsio.GetFileName(F), Nside]);
    Map.Resolution := GetResolution(Nside);

    OrderingStr := Cfitsio.ReadKeyAsString(F, 'ORDERING');
    Map.Description := Cfitsio.ReadKeyAsString(F, Format('TTYPE%d', [ColumnNumber]));
    try
        Map.MeasureUnit :=
            Cfitsio.ReadKeyAsString(F, Format('TUNIT%d', [ColumnNumber]));
    except
        Map.MeasureUnit := '';
    end;

    try
        CoordSysStr := Cfitsio.ReadKeyAsString(F, 'COORDSYS');
        case UpperCase(LeftStr(CoordSysStr, 3)) of
        'GAL': Map.CoordSys := csGalactic;
        'ECL': Map.CoordSys := csEcliptic;
        else Map.CoordSys := csUnknown;
        end;
    except
        Map.CoordSys := csUnknown;
    end;

    case UpperCase(OrderingStr) of
    'RING': Map.Ordering := Ring;
    'NEST', 'NESTED': Map.ordering := Nested;
    else THealpixError.CreateFmt('Invalid ordering for map %s: "%s"',
                                 [Cfitsio.GetFileName(F), OrderingStr]);
    end;

    SetLength(Map.Pixels, Map.Resolution.Npix);
    Cfitsio.ReadColumn(F, ColumnNumber, 1, 1, Map.Pixels);
end;

{------------------------------------------------------------------------------}

procedure WriteHealpixKeywords(var F : TFitsFile; const Map : THealpixMap);
var
    OrderingStr : String;
    CoordSysStr : String;
begin
    case Map.Ordering of
    Ring: OrderingStr := 'RING';
    Nested: OrderingStr := 'NESTED';
    end;

    Cfitsio.WriteKey(F, 'PIXTYPE', 'HEALPIX',
                     'HEALPIX pixelisation');
    Cfitsio.WriteKey(F, 'NSIDE', Map.Resolution.Nside,
                    'Resolution parameter of HEALPIX');
    Cfitsio.WriteKey(F, 'FIRSTPIX', 0, 'First pixel # (0 based)');
    Cfitsio.WriteKey(F, 'LASTPIX', High(Map.Pixels), 'Last pixel # (0 based)');
    Cfitsio.WriteKey(F, 'ORDERING', OrderingStr,
                    'Pixel ordering scheme, either RING or NESTED');

    case Map.CoordSys of
    csUnknown: CoordSysStr := '';
    csEcliptic: CoordSysStr := 'Ecliptic';
    csGalactic: CoordSysStr := 'Galactic';
    end;
    if CoordSysStr <> '' then
        Cfitsio.WriteKey(F, 'COORDSYS', CoordSysStr, 'Coordinate system');

    Cfitsio.WriteKey(F, 'INDXSCHM', 'IMPLICIT',
                    'Indexing: IMPLICIT or EXPLICIT');
end;

{------------------------------------------------------------------------------}

procedure WriteHealpixMap(var F : TFitsFile;
                          const Map : THealpixMap;
                          FileDataType : TFitsType);
var
    TableColumns : Array[1..1] of Cfitsio.TColumn;

begin
    with TableColumns[1] do
    begin
        Name := Map.Description;
        Count := 1;
        DataType := FileDataType;
        UnitStr := Map.MeasureUnit;
    end;

    Cfitsio.CreateTable(F, BinaryTable, 0, TableColumns, 'xtension');

    WriteHealpixKeywords(F, Map);
    Cfitsio.WriteColumn(F, 1, 1, 1, Map.Pixels);
end;

{------------------------------------------------------------------------------}

procedure ReadHealpixMap(const FileName : String;
                         HduNumber, ColumnNumber : Integer;
                         out Map : THealpixMap);
var
    F : TFitsFile;
begin
    F := Cfitsio.OpenFile(FileName, ReadOnly);
    try
        Cfitsio.MoveAbsHdu(F, HduNumber);
        ReadHealpixMap(F, ColumnNumber, Map);
    finally
        Cfitsio.CloseFile(F);
    end;
end;

{------------------------------------------------------------------------------}

procedure ReadHealpixMap(const FileName : String;
                         ColumnNumber : Integer;
                         out Map : THealpixMap);
var
    F : TFitsFile;
begin
    F := Cfitsio.OpenTable(FileName, ReadOnly);
    try
        ReadHealpixMap(F, ColumnNumber, Map);
    finally
        Cfitsio.CloseFile(F);
    end;
end;

{------------------------------------------------------------------------------}

procedure WriteHealpixMap(const FileName : String;
                          const Map : THealpixMap;
                          FileDataType : TFitsType);
var
    F : TFitsFile;

begin
    F := Cfitsio.CreateFile(FileName, Overwrite);
    try
        WriteHealpixMap(F, Map, FileDataType);
    finally
        Cfitsio.CloseFile(F);
    end;
end;

end.
