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
/// File last update : 2024-07-28T17:55:16.000+02:00
/// Signature : d5871d37e0861af51135e022f1f9d89a7b2f1151
/// ***************************************************************************
/// </summary>

program ShowSVG;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {Form1},
  Olf.Skia.SVGToBitmap in '..\lib-externes\librairies\src\Olf.Skia.SVGToBitmap.pas',
  uSVGToImages in '..\src\uSVGToImages.pas',
  USVGInputPrompts in '..\assets\kenney.nl\InputPrompts\USVGInputPrompts.pas',
  USVGPuzzleAssets2 in '..\assets\kenney.nl\PuzzleAssets2\USVGPuzzleAssets2.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
