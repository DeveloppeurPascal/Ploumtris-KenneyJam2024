/// <summary>
/// ***************************************************************************
///
/// Ploumtris
///
/// Copyright 2024-2025 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://ploumtris.gamolf.fr/
///
/// Project site :
/// https://github.com/DeveloppeurPascal/Ploumtris-KenneyJam2024
///
/// ***************************************************************************
/// File last update : 2024-07-28T19:17:30.000+02:00
/// Signature : f482296e877abf02237b459b9c5d2b4429554a33
/// ***************************************************************************
/// </summary>

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
