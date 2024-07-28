unit uCurrentGame;

interface

uses
  uPipeParts,
  uConsts;

type
  TGameGrid = array [0 .. CNbCol - 1, 0 .. CNbRow - 1] of tpipepart;

  TGame = class
  private
    FGrid: TGameGrid;
    FWaterPipes: TPipePartsList;
    FIsRunning: boolean;
    FCurPipe: tpipepart;
    FScore: int64;
    procedure SetIsRunning(const Value: boolean);
    procedure SetCurPipe(const Value: tpipepart);
    procedure SetScore(const Value: int64);
    procedure SetWaterPipes(const Value: TPipePartsList);
  protected
  public
    property CurPipe: tpipepart read FCurPipe write SetCurPipe;
    property IsRunning: boolean read FIsRunning write SetIsRunning;
    property Score: int64 read FScore write SetScore;
    property WaterPipes: TPipePartsList read FWaterPipes write SetWaterPipes;
    constructor Create;
    procedure Init;
    procedure Save;
    procedure Load;
    function GetGrid(const x, y: integer): tpipepart;
    procedure SetGrid(const x, y: integer; const PipePart: tpipepart);
    destructor Destroy; override;
    procedure AfterRemovingConnectedPieces;
  end;

function CurrentGame: TGame;

implementation

uses
  System.SysUtils,
  System.Classes;

var
  GCurGame: TGame;

function CurrentGame: TGame;
begin
  if not assigned(GCurGame) then
    GCurGame := TGame.Create;
  result := GCurGame;
end;

{ TGame }

procedure TGame.AfterRemovingConnectedPieces;
var
  x, y: integer;
  moved: boolean;
begin
  moved := false;

  for x := 0 to CNbCol - 1 do
    for y := 0 to (CNbRow - 1) - 1 do
      if assigned(FGrid[x, y]) and (FGrid[x, y] <> CurPipe) and
        (not assigned(FGrid[x, y + 1])) then
      begin
        FGrid[x, y].ScreenY := FGrid[x, y].ScreenY + FGrid[x, y].Height;
        moved := true;
      end;

  if moved then
    tthread.forcequeue(nil,
      procedure
      begin
        AfterRemovingConnectedPieces;
      end);
end;

constructor TGame.Create;
begin
  inherited;
  FWaterPipes := TPipePartsList.Create;
  Init;
end;

destructor TGame.Destroy;
begin
  FWaterPipes.free;
  inherited;
end;

function TGame.GetGrid(const x, y: integer): tpipepart;
begin
  if (x < 0) or (y < 0) or (x >= CNbCol) or (y >= CNbRow) then
    raise Exception.Create('Hors zone !');

  result := FGrid[x, y];
end;

procedure TGame.Init;
var
  x, y: integer;
begin
  FIsRunning := false;
  FCurPipe := nil;
  FScore := 0;
  for x := 0 to CNbCol - 1 do
    for y := 0 to CNbRow - 1 do
      FGrid[x, y] := nil;
  FWaterPipes.Clear;
end;

procedure TGame.Load;
begin
  // TODO : à compléter
end;

procedure TGame.Save;
begin
  // TODO : à compléter
end;

procedure TGame.SetCurPipe(const Value: tpipepart);
begin
  FCurPipe := Value;
end;

procedure TGame.SetGrid(const x, y: integer; const PipePart: tpipepart);
begin
  if (x < 0) or (y < 0) or (x >= CNbCol) or (y >= CNbRow) then
    raise Exception.Create('Hors zone !');

  FGrid[x, y] := PipePart;
end;

procedure TGame.SetIsRunning(const Value: boolean);
begin
  FIsRunning := Value;
end;

procedure TGame.SetScore(const Value: int64);
begin
  FScore := Value;
end;

procedure TGame.SetWaterPipes(const Value: TPipePartsList);
begin
  FWaterPipes := Value;
end;

initialization

GCurGame := nil;

finalization

GCurGame.free;

end.
