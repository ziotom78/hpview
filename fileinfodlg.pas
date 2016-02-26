unit fileinfodlg;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    ButtonPanel, Healpix;

type

    { TFileInformation }

    TFileInformation = class(TForm)
        ButtonPanel : TButtonPanel;
        CoordinateSystemGroup : TRadioGroup;
        PixelOrderingGroup : TRadioGroup;
        procedure FormCreate(Sender : TObject);
    private
        { private declarations }
    public
        procedure SetControlValuesFromMap(const Map : THealpixMap);
        procedure SetMapFromControlValues(var Map : THealpixMap);
    end;

var
    FileInformation : TFileInformation;

implementation

{$R *.lfm}

{ TFileInformation }

procedure TFileInformation.FormCreate(Sender : TObject);
begin
    CoordinateSystemGroup.ItemIndex := 2;
    PixelOrderingGroup.ItemIndex := 0;
end;

procedure TFileInformation.SetControlValuesFromMap(const Map : THealpixMap);
begin
    case Map.CoordSys of
    csGalactic: CoordinateSystemGroup.ItemIndex := 0;
    csEcliptic: CoordinateSystemGroup.ItemIndex := 1;
    else CoordinateSystemGroup.ItemIndex := 2;
    end;

    case Map.Ordering of
    Ring: PixelOrderingGroup.ItemIndex := 0;
    Nested: PixelOrderingGroup.ItemIndex := 1;
    end;
end;

procedure TFileInformation.SetMapFromControlValues(var Map : THealpixMap);
begin
    case CoordinateSystemGroup.ItemIndex of
    0: Map.CoordSys := csGalactic;
    1: Map.CoordSys := csEcliptic;
    else Map.CoordSys := csUnknown;
    end;

    case PixelOrderingGroup.ItemIndex of
    0: Map.Ordering := Ring;
    1: Map.Ordering := Nested;
    end;
end;

end.

