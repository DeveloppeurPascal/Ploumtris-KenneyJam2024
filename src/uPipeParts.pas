unit uPipeParts;

interface

uses
  System.Classes,
  FMX.Objects,
  PuzzleAssets2;

type
  TPipePart = class(TRectangle)
  private
    FGrilleX: integer;
    FGrilleY: integer;
    FSVGIndex: tsvgsvgindex;
    FVy: single;
    procedure SetHasWater(const Value: boolean);
    procedure SetGrilleX(const Value: integer);
    procedure SetGrilleY(const Value: integer);
    procedure SetSVGIndex(const Value: tsvgsvgindex);
    function GetHasDownConnection: boolean;
    function GetHasLeftConnection: boolean;
    function GetHasRightConnection: boolean;
    function GetHasUpConnection: boolean;
    function GetScreenX: single;
    function GetScreenY: single;
    procedure SetScreenX(const Value: single);
    procedure SetScreenY(const Value: single);
    function GetHasWater: boolean;
    procedure SetVy(const Value: single);
  protected
    procedure RefreshBackgroundImage;
    procedure DoResized; override;
  public
    property SVGIndex: tsvgsvgindex read FSVGIndex write SetSVGIndex;
    property HasLeftConnection: boolean read GetHasLeftConnection;
    property HasRightConnection: boolean read GetHasRightConnection;
    property HasUpConnection: boolean read GetHasUpConnection;
    property HasDownConnection: boolean read GetHasDownConnection;
    property HasWater: boolean read GetHasWater write SetHasWater;
    property GrilleX: integer read FGrilleX write SetGrilleX;
    property GrilleY: integer read FGrilleY write SetGrilleY;
    property ScreenX: single read GetScreenX write SetScreenX;
    property ScreenY: single read GetScreenY write SetScreenY;
    property Vy: single read FVy write SetVy;
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
  end;

implementation

uses
  FMX.Graphics,
  System.SysUtils,
  uConsts,
  uSVGToImages;

{ TPipePart }

procedure TPipePart.AfterConstruction;
begin
  inherited;
  width := CPipeSize;
  height := CPipeSize;
  stroke.Kind := tbrushkind.none;
  fill.Kind := tbrushkind.Bitmap;
  fill.Bitmap.WrapMode := twrapmode.TileStretch;
  RefreshBackgroundImage;
end;

constructor TPipePart.Create(AOwner: TComponent);
begin
  inherited;
  FGrilleX := -1;
  FGrilleY := -1;
  FSVGIndex := tsvgsvgindex.PipeHdbg;
  FVy := 0;
end;

procedure TPipePart.DoResized;
begin
  inherited;
  RefreshBackgroundImage;
end;

function TPipePart.GetHasDownConnection: boolean;
begin
  result := FSVGIndex in [tsvgsvgindex.EauDb, tsvgsvgindex.EauGb,
    tsvgsvgindex.EauGdb, tsvgsvgindex.EauHb, tsvgsvgindex.EauHdb,
    tsvgsvgindex.EauHdbg, tsvgsvgindex.EauHgb, tsvgsvgindex.pipeDb,
    tsvgsvgindex.pipeGb, tsvgsvgindex.pipeGdb, tsvgsvgindex.pipeHb,
    tsvgsvgindex.pipeHdb, tsvgsvgindex.PipeHdbg, tsvgsvgindex.pipeHgb];
end;

function TPipePart.GetHasLeftConnection: boolean;
begin
  result := FSVGIndex in [tsvgsvgindex.EauGb, tsvgsvgindex.EauGd,
    tsvgsvgindex.EauGdb, tsvgsvgindex.EauGdh, tsvgsvgindex.EauHdbg,
    tsvgsvgindex.EauHg, tsvgsvgindex.EauHgb, tsvgsvgindex.pipeGb,
    tsvgsvgindex.pipeGd, tsvgsvgindex.pipeGdb, tsvgsvgindex.pipehGd,
    tsvgsvgindex.PipeHdbg, tsvgsvgindex.pipeHg, tsvgsvgindex.pipeHgb];
end;

function TPipePart.GetHasRightConnection: boolean;
begin
  result := FSVGIndex in [tsvgsvgindex.EauDb, tsvgsvgindex.EauGd,
    tsvgsvgindex.EauGdb, tsvgsvgindex.EauGdh, tsvgsvgindex.EauHdbg,
    tsvgsvgindex.EauHd, tsvgsvgindex.EauHdb, tsvgsvgindex.pipeDb,
    tsvgsvgindex.pipeGd, tsvgsvgindex.pipeGdb, tsvgsvgindex.pipehGd,
    tsvgsvgindex.PipeHdbg, tsvgsvgindex.pipeHd, tsvgsvgindex.pipeHdb];
end;

function TPipePart.GetHasUpConnection: boolean;
begin
  result := FSVGIndex in [tsvgsvgindex.EauHd, tsvgsvgindex.EauHg,
    tsvgsvgindex.EauGdh, tsvgsvgindex.EauHb, tsvgsvgindex.EauHdb,
    tsvgsvgindex.EauHdbg, tsvgsvgindex.EauHgb, tsvgsvgindex.pipeHd,
    tsvgsvgindex.pipeHg, tsvgsvgindex.pipehGd, tsvgsvgindex.pipeHb,
    tsvgsvgindex.pipeHdb, tsvgsvgindex.PipeHdbg, tsvgsvgindex.pipeHgb];
end;

function TPipePart.GetHasWater: boolean;
begin
  if SVGIndex in [tsvgsvgindex.EauDb .. tsvgsvgindex.EauHgb] then
    result := true
  else if SVGIndex in [tsvgsvgindex.pipeDb .. tsvgsvgindex.pipehGd] then
    result := false
  else
    raise exception.Create('Unknow water status for ' + ord(SVGIndex).tostring);
end;

function TPipePart.GetScreenX: single;
begin
  result := position.x;
end;

function TPipePart.GetScreenY: single;
begin
  result := position.y;
end;

procedure TPipePart.RefreshBackgroundImage;
var
  bmp: TBitmap;
begin
  bmp := getBitmapFromSVG(FSVGIndex, width, height,
    fill.Bitmap.Bitmap.BitmapScale, 0);
  try
    fill.Bitmap.Bitmap.assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TPipePart.SetGrilleX(const Value: integer);
begin
  FGrilleX := Value;
end;

procedure TPipePart.SetGrilleY(const Value: integer);
begin
  FGrilleY := Value;
end;

procedure TPipePart.SetHasWater(const Value: boolean);
begin
  if Value then
    case FSVGIndex of
      tsvgsvgindex.pipeDb:
        SVGIndex := tsvgsvgindex.EauDb;
      tsvgsvgindex.pipeGb:
        SVGIndex := tsvgsvgindex.EauGb;
      tsvgsvgindex.pipeGd:
        SVGIndex := tsvgsvgindex.EauGd;
      tsvgsvgindex.pipeGdb:
        SVGIndex := tsvgsvgindex.EauGdb;
      tsvgsvgindex.pipeHb:
        SVGIndex := tsvgsvgindex.EauHb;
      tsvgsvgindex.pipeHd:
        SVGIndex := tsvgsvgindex.EauHd;
      tsvgsvgindex.pipeHdb:
        SVGIndex := tsvgsvgindex.EauHdb;
      tsvgsvgindex.PipeHdbg:
        SVGIndex := tsvgsvgindex.EauHdbg;
      tsvgsvgindex.pipeHg:
        SVGIndex := tsvgsvgindex.EauHg;
      tsvgsvgindex.pipeHgb:
        SVGIndex := tsvgsvgindex.EauHgb;
      tsvgsvgindex.pipehGd:
        SVGIndex := tsvgsvgindex.EauGdh;
    end
  else
    case FSVGIndex of
      tsvgsvgindex.EauDb:
        SVGIndex := tsvgsvgindex.pipeDb;
      tsvgsvgindex.EauGb:
        SVGIndex := tsvgsvgindex.pipeGb;
      tsvgsvgindex.EauGd:
        SVGIndex := tsvgsvgindex.pipeGd;
      tsvgsvgindex.EauGdb:
        SVGIndex := tsvgsvgindex.pipeGdb;
      tsvgsvgindex.EauGdh:
        SVGIndex := tsvgsvgindex.pipehGd;
      tsvgsvgindex.EauHb:
        SVGIndex := tsvgsvgindex.pipeHb;
      tsvgsvgindex.EauHd:
        SVGIndex := tsvgsvgindex.pipeHd;
      tsvgsvgindex.EauHdb:
        SVGIndex := tsvgsvgindex.pipeHdb;
      tsvgsvgindex.EauHdbg:
        SVGIndex := tsvgsvgindex.PipeHdbg;
      tsvgsvgindex.EauHg:
        SVGIndex := tsvgsvgindex.pipeHg;
      tsvgsvgindex.EauHgb:
        SVGIndex := tsvgsvgindex.pipeHgb;
    end;
end;

procedure TPipePart.SetScreenX(const Value: single);
begin
  position.x := Value;
end;

procedure TPipePart.SetScreenY(const Value: single);
begin
  position.y := Value;
end;

procedure TPipePart.SetSVGIndex(const Value: tsvgsvgindex);
begin
  if Value in [tsvgsvgindex.EauDb .. tsvgsvgindex.pipehGd] then
  begin
    FSVGIndex := Value;
    RefreshBackgroundImage;
  end
  else
    raise exception.Create('Pipe ' + ord(Value).tostring + ' not allowed !');
end;

procedure TPipePart.SetVy(const Value: single);
begin
  FVy := Value;
end;

end.
