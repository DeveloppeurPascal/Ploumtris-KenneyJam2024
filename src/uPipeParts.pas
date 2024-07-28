unit uPipeParts;

interface

uses
  System.Generics.Collections,
  System.Classes,
  FMX.Objects,
  USVGPuzzleAssets2;

type
  TPipePart = class(TRectangle)
  private
    FGrilleX: integer;
    FGrilleY: integer;
    FSVGIndex: TSVGPuzzleAssets2Index;
    FVy: single;
    procedure SetHasWater(const Value: boolean);
    procedure SetGrilleX(const Value: integer);
    procedure SetGrilleY(const Value: integer);
    procedure SetSVGIndex(const Value: TSVGPuzzleAssets2Index);
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
    FChecked: boolean;
    procedure RefreshBackgroundImage;
    procedure DoResized; override;
  public
    property SVGIndex: TSVGPuzzleAssets2Index read FSVGIndex write SetSVGIndex;
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
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure GoToLeft;
    procedure GoToRight;
    procedure StopFalling;
    procedure RotateRight;
    /// <summary>
    /// chech the left/right connection and eliminate the connected pipe's parts
    /// </summary>
    procedure TestConnections(var ConnectedLeft, ConnectedRight: boolean;
      const First: boolean = false);
    /// <summary>
    /// remove all connected pipes and calculate the points to add to the score
    /// </summary>
    procedure RemoveConnectedPipes(var NbPipe: int64;
      const First: boolean = false);
  end;

  TPipePartsList = tlist<TPipePart>;

implementation

uses
  FMX.Graphics,
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
  FSVGIndex := TSVGPuzzleAssets2Index.PipeHdbg;
  FVy := 0;
  FChecked := false;
  HitTest := false;
end;

destructor TPipePart.Destroy;
begin
  CurrentGame.SetGrid(FGrilleX, FGrilleY, nil);
  CurrentGame.WaterPipes.Remove(self);
  inherited;
end;

procedure TPipePart.DoResized;
begin
  inherited;
  RefreshBackgroundImage;
end;

function TPipePart.GetHasDownConnection: boolean;
begin
  result := FSVGIndex in [TSVGPuzzleAssets2Index.EauDb,
    TSVGPuzzleAssets2Index.EauGb, TSVGPuzzleAssets2Index.EauGdb,
    TSVGPuzzleAssets2Index.EauHb, TSVGPuzzleAssets2Index.EauHdb,
    TSVGPuzzleAssets2Index.EauHdbg, TSVGPuzzleAssets2Index.EauHgb,
    TSVGPuzzleAssets2Index.pipeDb, TSVGPuzzleAssets2Index.pipeGb,
    TSVGPuzzleAssets2Index.pipeGdb, TSVGPuzzleAssets2Index.pipeHb,
    TSVGPuzzleAssets2Index.pipeHdb, TSVGPuzzleAssets2Index.PipeHdbg,
    TSVGPuzzleAssets2Index.pipeHgb];
end;

function TPipePart.GetHasLeftConnection: boolean;
begin
  result := FSVGIndex in [TSVGPuzzleAssets2Index.EauGb,
    TSVGPuzzleAssets2Index.EauGd, TSVGPuzzleAssets2Index.EauGdb,
    TSVGPuzzleAssets2Index.EauGdh, TSVGPuzzleAssets2Index.EauHdbg,
    TSVGPuzzleAssets2Index.EauHg, TSVGPuzzleAssets2Index.EauHgb,
    TSVGPuzzleAssets2Index.pipeGb, TSVGPuzzleAssets2Index.pipeGd,
    TSVGPuzzleAssets2Index.pipeGdb, TSVGPuzzleAssets2Index.pipehGd,
    TSVGPuzzleAssets2Index.PipeHdbg, TSVGPuzzleAssets2Index.pipeHg,
    TSVGPuzzleAssets2Index.pipeHgb];
end;

function TPipePart.GetHasRightConnection: boolean;
begin
  result := FSVGIndex in [TSVGPuzzleAssets2Index.EauDb,
    TSVGPuzzleAssets2Index.EauGd, TSVGPuzzleAssets2Index.EauGdb,
    TSVGPuzzleAssets2Index.EauGdh, TSVGPuzzleAssets2Index.EauHdbg,
    TSVGPuzzleAssets2Index.EauHd, TSVGPuzzleAssets2Index.EauHdb,
    TSVGPuzzleAssets2Index.pipeDb, TSVGPuzzleAssets2Index.pipeGd,
    TSVGPuzzleAssets2Index.pipeGdb, TSVGPuzzleAssets2Index.pipehGd,
    TSVGPuzzleAssets2Index.PipeHdbg, TSVGPuzzleAssets2Index.pipeHd,
    TSVGPuzzleAssets2Index.pipeHdb];
end;

function TPipePart.GetHasUpConnection: boolean;
begin
  result := FSVGIndex in [TSVGPuzzleAssets2Index.EauHd,
    TSVGPuzzleAssets2Index.EauHg, TSVGPuzzleAssets2Index.EauGdh,
    TSVGPuzzleAssets2Index.EauHb, TSVGPuzzleAssets2Index.EauHdb,
    TSVGPuzzleAssets2Index.EauHdbg, TSVGPuzzleAssets2Index.EauHgb,
    TSVGPuzzleAssets2Index.pipeHd, TSVGPuzzleAssets2Index.pipeHg,
    TSVGPuzzleAssets2Index.pipehGd, TSVGPuzzleAssets2Index.pipeHb,
    TSVGPuzzleAssets2Index.pipeHdb, TSVGPuzzleAssets2Index.PipeHdbg,
    TSVGPuzzleAssets2Index.pipeHgb];
end;

function TPipePart.GetHasWater: boolean;
begin
  if SVGIndex in [TSVGPuzzleAssets2Index.EauDb .. TSVGPuzzleAssets2Index.EauHgb]
  then
    result := true
  else if SVGIndex in [TSVGPuzzleAssets2Index.pipeDb .. TSVGPuzzleAssets2Index.
    pipehGd] then
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
    ScreenX := ScreenX - width;
end;

procedure TPipePart.GoToRight;
begin
  if (GrilleX + 1 < CNbCol) and not assigned(CurrentGame.GetGrid(GrilleX + 1,
    GrilleY)) then
    ScreenX := ScreenX + width;
end;

procedure TPipePart.RefreshBackgroundImage;
begin
  fill.Bitmap.Bitmap.assign(getBitmapFromSVG(FSVGIndex, width, height,
    fill.Bitmap.Bitmap.BitmapScale));
end;

procedure TPipePart.RemoveConnectedPipes(var NbPipe: int64;
  const First: boolean);
var
  Pipe: TPipePart;
begin
  if First then
    for Pipe in CurrentGame.WaterPipes do
      Pipe.FChecked := false;

  if FChecked then
    exit;

  FChecked := true;
  NbPipe := NbPipe + 1;
  CurrentGame.Score := CurrentGame.Score + NbPipe;
  // TODO : à changer selon le niveau de jeu (si on en gère un un jour)

  if HasLeftConnection then
  begin
    if (GrilleX > 0) and assigned(CurrentGame.GetGrid(GrilleX - 1, GrilleY)) and
      CurrentGame.GetGrid(GrilleX - 1, GrilleY).HasRightConnection then
      CurrentGame.GetGrid(GrilleX - 1, GrilleY).RemoveConnectedPipes(NbPipe);
  end;

  if HasRightConnection then
  begin
    if (GrilleX < CNbCol - 1) and
      assigned(CurrentGame.GetGrid(GrilleX + 1, GrilleY)) and
      CurrentGame.GetGrid(GrilleX + 1, GrilleY).HasLeftConnection then
      CurrentGame.GetGrid(GrilleX + 1, GrilleY).RemoveConnectedPipes(NbPipe);
  end;

  if HasUpConnection then
  begin
    if (GrilleY > 0) and assigned(CurrentGame.GetGrid(GrilleX, GrilleY - 1)) and
      CurrentGame.GetGrid(GrilleX, GrilleY - 1).HasDownConnection then
      CurrentGame.GetGrid(GrilleX, GrilleY - 1).RemoveConnectedPipes(NbPipe);
  end;

  if HasDownConnection then
  begin
    if (GrilleY < CNbrow - 1) and
      assigned(CurrentGame.GetGrid(GrilleX, GrilleY + 1)) and
      CurrentGame.GetGrid(GrilleX, GrilleY + 1).HasUpConnection then
      CurrentGame.GetGrid(GrilleX, GrilleY + 1).RemoveConnectedPipes(NbPipe);
  end;

  tthread.forcequeue(nil,
    procedure
    begin
      self.Free;
    end);
end;

procedure TPipePart.RotateRight;
begin
  case FSVGIndex of
    TSVGPuzzleAssets2Index.pipeDb:
      SVGIndex := TSVGPuzzleAssets2Index.pipeGb;
    TSVGPuzzleAssets2Index.pipeGb:
      SVGIndex := TSVGPuzzleAssets2Index.pipeHg;
    TSVGPuzzleAssets2Index.pipeGd:
      SVGIndex := TSVGPuzzleAssets2Index.pipeHb;
    TSVGPuzzleAssets2Index.pipeGdb:
      SVGIndex := TSVGPuzzleAssets2Index.pipeHgb;
    TSVGPuzzleAssets2Index.pipeHb:
      SVGIndex := TSVGPuzzleAssets2Index.pipeGd;
    TSVGPuzzleAssets2Index.pipeHd:
      SVGIndex := TSVGPuzzleAssets2Index.pipeDb;
    TSVGPuzzleAssets2Index.pipeHdb:
      SVGIndex := TSVGPuzzleAssets2Index.pipeGdb;
    TSVGPuzzleAssets2Index.PipeHdbg:
      SVGIndex := TSVGPuzzleAssets2Index.PipeHdbg;
    TSVGPuzzleAssets2Index.pipeHg:
      SVGIndex := TSVGPuzzleAssets2Index.pipeHd;
    TSVGPuzzleAssets2Index.pipeHgb:
      SVGIndex := TSVGPuzzleAssets2Index.pipehGd;
    TSVGPuzzleAssets2Index.pipehGd:
      SVGIndex := TSVGPuzzleAssets2Index.pipeHdb;
    TSVGPuzzleAssets2Index.EauDb:
      SVGIndex := TSVGPuzzleAssets2Index.EauGb;
    TSVGPuzzleAssets2Index.EauGb:
      SVGIndex := TSVGPuzzleAssets2Index.EauHg;
    TSVGPuzzleAssets2Index.EauGd:
      SVGIndex := TSVGPuzzleAssets2Index.EauHb;
    TSVGPuzzleAssets2Index.EauGdb:
      SVGIndex := TSVGPuzzleAssets2Index.EauHgb;
    TSVGPuzzleAssets2Index.EauGdh:
      SVGIndex := TSVGPuzzleAssets2Index.EauGdh;
    TSVGPuzzleAssets2Index.EauHb:
      SVGIndex := TSVGPuzzleAssets2Index.EauGd;
    TSVGPuzzleAssets2Index.EauHd:
      SVGIndex := TSVGPuzzleAssets2Index.EauDb;
    TSVGPuzzleAssets2Index.EauHdb:
      SVGIndex := TSVGPuzzleAssets2Index.EauGdb;
    TSVGPuzzleAssets2Index.EauHdbg:
      SVGIndex := TSVGPuzzleAssets2Index.EauHdbg;
    TSVGPuzzleAssets2Index.EauHg:
      SVGIndex := TSVGPuzzleAssets2Index.EauHd;
    TSVGPuzzleAssets2Index.EauHgb:
      SVGIndex := TSVGPuzzleAssets2Index.EauGdh;
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
  BecameWater: boolean;
begin
  BecameWater := (not HasWater) and Value;

  if Value then
    case FSVGIndex of
      TSVGPuzzleAssets2Index.pipeDb:
        SVGIndex := TSVGPuzzleAssets2Index.EauDb;
      TSVGPuzzleAssets2Index.pipeGb:
        SVGIndex := TSVGPuzzleAssets2Index.EauGb;
      TSVGPuzzleAssets2Index.pipeGd:
        SVGIndex := TSVGPuzzleAssets2Index.EauGd;
      TSVGPuzzleAssets2Index.pipeGdb:
        SVGIndex := TSVGPuzzleAssets2Index.EauGdb;
      TSVGPuzzleAssets2Index.pipeHb:
        SVGIndex := TSVGPuzzleAssets2Index.EauHb;
      TSVGPuzzleAssets2Index.pipeHd:
        SVGIndex := TSVGPuzzleAssets2Index.EauHd;
      TSVGPuzzleAssets2Index.pipeHdb:
        SVGIndex := TSVGPuzzleAssets2Index.EauHdb;
      TSVGPuzzleAssets2Index.PipeHdbg:
        SVGIndex := TSVGPuzzleAssets2Index.EauHdbg;
      TSVGPuzzleAssets2Index.pipeHg:
        SVGIndex := TSVGPuzzleAssets2Index.EauHg;
      TSVGPuzzleAssets2Index.pipeHgb:
        SVGIndex := TSVGPuzzleAssets2Index.EauHgb;
      TSVGPuzzleAssets2Index.pipehGd:
        SVGIndex := TSVGPuzzleAssets2Index.EauGdh;
    end
  else
    case FSVGIndex of
      TSVGPuzzleAssets2Index.EauDb:
        SVGIndex := TSVGPuzzleAssets2Index.pipeDb;
      TSVGPuzzleAssets2Index.EauGb:
        SVGIndex := TSVGPuzzleAssets2Index.pipeGb;
      TSVGPuzzleAssets2Index.EauGd:
        SVGIndex := TSVGPuzzleAssets2Index.pipeGd;
      TSVGPuzzleAssets2Index.EauGdb:
        SVGIndex := TSVGPuzzleAssets2Index.pipeGdb;
      TSVGPuzzleAssets2Index.EauGdh:
        SVGIndex := TSVGPuzzleAssets2Index.pipehGd;
      TSVGPuzzleAssets2Index.EauHb:
        SVGIndex := TSVGPuzzleAssets2Index.pipeHb;
      TSVGPuzzleAssets2Index.EauHd:
        SVGIndex := TSVGPuzzleAssets2Index.pipeHd;
      TSVGPuzzleAssets2Index.EauHdb:
        SVGIndex := TSVGPuzzleAssets2Index.pipeHdb;
      TSVGPuzzleAssets2Index.EauHdbg:
        SVGIndex := TSVGPuzzleAssets2Index.PipeHdbg;
      TSVGPuzzleAssets2Index.EauHg:
        SVGIndex := TSVGPuzzleAssets2Index.pipeHg;
      TSVGPuzzleAssets2Index.EauHgb:
        SVGIndex := TSVGPuzzleAssets2Index.pipeHgb;
    end;

  if BecameWater then
  begin
    if not CurrentGame.WaterPipes.contains(self) then
      CurrentGame.WaterPipes.Add(self);

    // passage de "pipe" à "eau", on répercute sur les voisins
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
  end
  else if not Value then
    CurrentGame.WaterPipes.Remove(self);
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

procedure TPipePart.SetSVGIndex(const Value: TSVGPuzzleAssets2Index);
begin
  if Value in [TSVGPuzzleAssets2Index.EauDb .. TSVGPuzzleAssets2Index.pipehGd]
  then
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
  if FVy > height then
    FVy := height - 1;
end;

procedure TPipePart.StopFalling;
var
  ConnectedLeft, ConnectedRight: boolean;
  NbPipe: int64;
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

  if HasWater then
  begin
    ConnectedLeft := false;
    ConnectedRight := false;
    TestConnections(ConnectedLeft, ConnectedRight, true);
    if ConnectedLeft and ConnectedRight then
    begin
      NbPipe := 0;
      RemoveConnectedPipes(NbPipe, true);
      tthread.forcequeue(nil,
        procedure
        begin
          CurrentGame.AfterRemovingConnectedPieces;
        end);
    end;
  end;
end;

procedure TPipePart.TestConnections(var ConnectedLeft, ConnectedRight: boolean;
const First: boolean);
var
  Pipe: TPipePart;
begin
  if First then
    for Pipe in CurrentGame.WaterPipes do
      Pipe.FChecked := false;

  if FChecked or (ConnectedLeft and ConnectedRight) then
    exit;

  FChecked := true;

  if HasLeftConnection then
  begin
    if (GrilleX = 0) then
      ConnectedLeft := true
    else if assigned(CurrentGame.GetGrid(GrilleX - 1, GrilleY)) and
      CurrentGame.GetGrid(GrilleX - 1, GrilleY).HasRightConnection then
      CurrentGame.GetGrid(GrilleX - 1, GrilleY).TestConnections(ConnectedLeft,
        ConnectedRight);
  end;

  if HasRightConnection then
  begin
    if (GrilleX = CNbCol - 1) then
      ConnectedRight := true
    else if assigned(CurrentGame.GetGrid(GrilleX + 1, GrilleY)) and
      CurrentGame.GetGrid(GrilleX + 1, GrilleY).HasLeftConnection then
      CurrentGame.GetGrid(GrilleX + 1, GrilleY).TestConnections(ConnectedLeft,
        ConnectedRight);
  end;

  if HasUpConnection then
  begin
    if (GrilleY > 0) and assigned(CurrentGame.GetGrid(GrilleX, GrilleY - 1)) and
      CurrentGame.GetGrid(GrilleX, GrilleY - 1).HasDownConnection then
      CurrentGame.GetGrid(GrilleX, GrilleY - 1).TestConnections(ConnectedLeft,
        ConnectedRight);
  end;

  if HasDownConnection then
  begin
    if (GrilleY < CNbrow - 1) and
      assigned(CurrentGame.GetGrid(GrilleX, GrilleY + 1)) and
      CurrentGame.GetGrid(GrilleX, GrilleY + 1).HasUpConnection then
      CurrentGame.GetGrid(GrilleX, GrilleY + 1).TestConnections(ConnectedLeft,
        ConnectedRight);
  end;
end;

end.
