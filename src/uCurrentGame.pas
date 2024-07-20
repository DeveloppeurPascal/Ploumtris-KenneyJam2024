unit uCurrentGame;

interface

uses uPipeParts;

type
  TGame = class
  private
    FIsRunning: boolean;
    FCurPipe: TPipePart;
    procedure SetIsRunning(const Value: boolean);
    procedure SetCurPipe(const Value: TPipePart);
  protected
  public
    property CurPipe: TPipePart read FCurPipe write SetCurPipe;
    property IsRunning: boolean read FIsRunning write SetIsRunning;
    constructor Create;
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
  FIsRunning := false;
  FCurPipe := nil;
end;

procedure TGame.SetCurPipe(const Value: TPipePart);
begin
  FCurPipe := Value;
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
