unit uScores;

interface

uses
  Gamolf.RTL.Scores;

function GetScoresList: TScoreList;

implementation

var
  Scores: TScoreList;

function GetScoresList: TScoreList;
begin
  if not assigned(Scores) then
  begin
    Scores := TScoreList.Create('Gamolf', 'Ploumtris');
    Scores.Load;
  end;
  // TODO : ajouter les informations de chiffrement pour les scores en RELEASE
  result := Scores;
end;

initialization

Scores := nil;

finalization

Scores.free;

end.
