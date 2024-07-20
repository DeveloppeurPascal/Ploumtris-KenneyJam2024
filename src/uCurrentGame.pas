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
    FIsRunning: boolean;
    FCurPipe: tpipepart;
    procedure SetIsRunning(const Value: boolean);
    procedure SetCurPipe(const Value: tpipepart);
  protected
  public
    property CurPipe: tpipepart read FCurPipe write SetCurPipe;
    property IsRunning: boolean read FIsRunning write SetIsRunning;
    constructor Create;
    procedure Init;
    procedure Save;
    procedure Load;
    function GetGrid(const x, y: integer): tpipepart;
    procedure SetGrid(const x, y: integer; const PipePart: tpipepart);
  end;

function CurrentGame: TGame;

implementation

var
  GCurGame: TGame;

function CurrentGame: TGame;
begin
  if not assigned(GCurGame) then
    GCurGame := TGame.Create;
  result := GCurGame;
end;

{ TGame }

constructor TGame.Create;
begin
  inherited;
  Init;
end;

function TGame.GetGrid(const x, y: integer): tpipepart;
begin
  // TODO : contrôler les valeurs de x et y
  result := FGrid[x, y];
end;

procedure TGame.Init;
var
  x, y: integer;
begin
  FIsRunning := false;
  FCurPipe := nil;
  for x := 0 to CNbCol - 1 do
    for y := 0 to CNbRow - 1 do
      FGrid[x, y] := nil;
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
  // TODO : contrôler les valeurs de x et y
  FGrid[x, y] := PipePart;
end;

procedure TGame.SetIsRunning(const Value: boolean);
begin
  FIsRunning := Value;
end;

initialization

GCurGame := nil;

finalization

GCurGame.free;

end.
