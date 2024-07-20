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
    function ParentWidth: single;
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
    procedure GoToLeft;
    procedure GoToRight;
    procedure StopFalling;
    procedure RotateRight;
  end;

implementation

uses
  FMX.Graphics,
  FMX.Controls,
  System.SysUtils,
  uConsts,
  uSVGToImages,
  uCurrentGame;

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
  FGrilleX := 0;
  FGrilleY := 0;
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

procedure TPipePart.GoToLeft;
begin
  if (GrilleX > 0) and not assigned(CurrentGame.GetGrid(GrilleX - 1, GrilleY))
  then
    CurrentGame.CurPipe.ScreenX := CurrentGame.CurPipe.ScreenX -
      CurrentGame.CurPipe.width;
end;

procedure TPipePart.GoToRight;
begin
  if (GrilleX + 1 < CNbCol) and not assigned(CurrentGame.GetGrid(GrilleX + 1,
    GrilleY)) then
    CurrentGame.CurPipe.ScreenX := CurrentGame.CurPipe.ScreenX +
      CurrentGame.CurPipe.width;
end;

function TPipePart.ParentWidth: single;
begin
  if parent is tcontrol then
    result := (parent as tcontrol).width
  else
    result := 0;
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

procedure TPipePart.RotateRight;
begin
  case FSVGIndex of
    tsvgsvgindex.pipeDb:
      SVGIndex := tsvgsvgindex.pipeGb;
    tsvgsvgindex.pipeGb:
      SVGIndex := tsvgsvgindex.pipeHd;
    tsvgsvgindex.pipeGd:
      SVGIndex := tsvgsvgindex.pipeHb;
    tsvgsvgindex.pipeGdb:
      SVGIndex := tsvgsvgindex.pipeHgb;
    tsvgsvgindex.pipeHb:
      SVGIndex := tsvgsvgindex.pipeGd;
    tsvgsvgindex.pipeHd:
      SVGIndex := tsvgsvgindex.pipeDb;
    tsvgsvgindex.pipeHdb:
      SVGIndex := tsvgsvgindex.pipeGdb;
    tsvgsvgindex.PipeHdbg:
      SVGIndex := tsvgsvgindex.PipeHdbg;
    tsvgsvgindex.pipeHg:
      SVGIndex := tsvgsvgindex.pipeHd;
    tsvgsvgindex.pipeHgb:
      SVGIndex := tsvgsvgindex.pipehGd;
    tsvgsvgindex.pipehGd:
      SVGIndex := tsvgsvgindex.pipeHdb;
    tsvgsvgindex.EauDb:
      SVGIndex := tsvgsvgindex.EauGb;
    tsvgsvgindex.EauGb:
      SVGIndex := tsvgsvgindex.EauHd;
    tsvgsvgindex.EauGd:
      SVGIndex := tsvgsvgindex.EauHb;
    tsvgsvgindex.EauGdb:
      SVGIndex := tsvgsvgindex.EauHgb;
    tsvgsvgindex.EauGdh:
      SVGIndex := tsvgsvgindex.EauGdh;
    tsvgsvgindex.EauHb:
      SVGIndex := tsvgsvgindex.EauGd;
    tsvgsvgindex.EauHd:
      SVGIndex := tsvgsvgindex.EauDb;
    tsvgsvgindex.EauHdb:
      SVGIndex := tsvgsvgindex.EauGdb;
    tsvgsvgindex.EauHdbg:
      SVGIndex := tsvgsvgindex.EauHdbg;
    tsvgsvgindex.EauHg:
      SVGIndex := tsvgsvgindex.EauHd;
    tsvgsvgindex.EauHgb:
      SVGIndex := tsvgsvgindex.EauGdh;
  end;
end;

procedure TPipePart.SetGrilleX(const Value: integer);
begin
  if (FGrilleX <> Value) then
  begin
    CurrentGame.SetGrid(FGrilleX, FGrilleY, nil);
    FGrilleX := Value;
    CurrentGame.SetGrid(FGrilleX, FGrilleY, self);
  end;
end;

procedure TPipePart.SetGrilleY(const Value: integer);
begin
  if (FGrilleY <> Value) then
  begin
    CurrentGame.SetGrid(FGrilleX, FGrilleY, nil);
    FGrilleY := Value;
    CurrentGame.SetGrid(FGrilleX, FGrilleY, self);
  end;
end;

procedure TPipePart.SetHasWater(const Value: boolean);
var
  recursif: boolean;
begin
  recursif := (not HasWater) and Value;

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

  if recursif then
  begin // passage de "pipe" à "eau", on répercute sur les voisins
    if HasLeftConnection and (GrilleX > 0) and
      assigned(CurrentGame.GetGrid(GrilleX - 1, GrilleY)) and
      CurrentGame.GetGrid(GrilleX - 1, GrilleY).HasRightConnection then
      CurrentGame.GetGrid(GrilleX - 1, GrilleY).HasWater := true;

    if HasRightConnection and (GrilleX < CNbCol - 1) and
      assigned(CurrentGame.GetGrid(GrilleX + 1, GrilleY)) and
      CurrentGame.GetGrid(GrilleX + 1, GrilleY).HasLeftConnection then
      CurrentGame.GetGrid(GrilleX + 1, GrilleY).HasWater := true;

    if HasUpConnection and (GrilleY > 0) and
      assigned(CurrentGame.GetGrid(GrilleX, GrilleY - 1)) and
      CurrentGame.GetGrid(GrilleX, GrilleY - 1).HasDownConnection then
      CurrentGame.GetGrid(GrilleX, GrilleY - 1).HasWater := true;

    if HasDownConnection and (GrilleY < CNbrow - 1) and
      assigned(CurrentGame.GetGrid(GrilleX, GrilleY + 1)) and
      CurrentGame.GetGrid(GrilleX, GrilleY + 1).HasUpConnection then
      CurrentGame.GetGrid(GrilleX, GrilleY + 1).HasWater := true;
  end;
end;

procedure TPipePart.SetScreenX(const Value: single);
begin
  position.x := Value;
  GrilleX := trunc(position.x / width);
end;

procedure TPipePart.SetScreenY(const Value: single);
begin
  position.y := Value;
  GrilleY := trunc(position.y / width);
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

procedure TPipePart.StopFalling;
begin
  Vy := 0;
  ScreenY := GrilleY * height;

  if HasLeftConnection and not HasWater then
  begin
    if (GrilleX = 0) then
      HasWater := true
    else if assigned(CurrentGame.GetGrid(GrilleX - 1, GrilleY)) and
      CurrentGame.GetGrid(GrilleX - 1, GrilleY).HasRightConnection then
      HasWater := CurrentGame.GetGrid(GrilleX - 1, GrilleY).HasWater;
  end;

  if HasRightConnection and not HasWater then
  begin
    if (GrilleX = CNbCol - 1) then
      HasWater := true
    else if assigned(CurrentGame.GetGrid(GrilleX + 1, GrilleY)) and
      CurrentGame.GetGrid(GrilleX + 1, GrilleY).HasLeftConnection then
      HasWater := CurrentGame.GetGrid(GrilleX + 1, GrilleY).HasWater;
  end;

  if HasUpConnection and not HasWater then
  begin
    if (GrilleY > 0) and assigned(CurrentGame.GetGrid(GrilleX, GrilleY - 1)) and
      CurrentGame.GetGrid(GrilleX, GrilleY - 1).HasDownConnection then
      HasWater := CurrentGame.GetGrid(GrilleX, GrilleY - 1).HasWater;
  end;

  if HasDownConnection and not HasWater then
  begin
    if (GrilleY < CNbrow - 1) and
      assigned(CurrentGame.GetGrid(GrilleX, GrilleY + 1)) and
      CurrentGame.GetGrid(GrilleX, GrilleY + 1).HasUpConnection then
      HasWater := CurrentGame.GetGrid(GrilleX, GrilleY + 1).HasWater;
  end;
end;

end.
