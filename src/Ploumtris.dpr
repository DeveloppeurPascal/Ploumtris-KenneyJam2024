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
/// File last update : 2025-01-19T16:02:40.000+01:00
/// Signature : cb7d40a4811d6f3e7803b34d5c3a84c60c583814
/// ***************************************************************************
/// </summary>

program Ploumtris;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {frmMain},
  Gamolf.FMX.Joystick in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick.Mac in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Joystick.Mac.pas',
  Gamolf.RTL.Joystick in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Joystick.pas',
  iOSapi.GameController in '..\lib-externes\Delphi-Game-Engine\src\iOSapi.GameController.pas',
  Macapi.GameController in '..\lib-externes\Delphi-Game-Engine\src\Macapi.GameController.pas',
  Gamolf.FMX.MusicLoop in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.FMX.MusicLoop.pas',
  Olf.RTL.Params in '..\lib-externes\librairies\src\Olf.RTL.Params.pas',
  Gamolf.RTL.Scores in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Scores.pas',
  Olf.FMX.AboutDialog in '..\lib-externes\AboutDialog-Delphi-Component\src\Olf.FMX.AboutDialog.pas',
  Olf.FMX.AboutDialogForm in '..\lib-externes\AboutDialog-Delphi-Component\src\Olf.FMX.AboutDialogForm.pas' {OlfAboutDialogForm},
  uDMLogo in 'uDMLogo.pas' {dmLogo: TDataModule},
  u_urlOpen in '..\lib-externes\librairies\src\u_urlOpen.pas',
  uConfig in 'uConfig.pas',
  uUIItemsList in 'uUIItemsList.pas',
  uSVGToImages in 'uSVGToImages.pas',
  Olf.Skia.SVGToBitmap in '..\lib-externes\librairies\src\Olf.Skia.SVGToBitmap.pas',
  cTextButton in 'cTextButton.pas' {cadTextButton: TFrame},
  uConsts in 'uConsts.pas',
  uPipeParts in 'uPipeParts.pas',
  uCurrentGame in 'uCurrentGame.pas',
  uBackgroundMusic in 'uBackgroundMusic.pas',
  cDialogBox in 'cDialogBox.pas' {cadDialogBox: TFrame},
  cPloumtrisTitle in 'cPloumtrisTitle.pas' {cadPloumtrisTitle: TFrame},
  Olf.RTL.CryptDecrypt in '..\lib-externes\librairies\src\Olf.RTL.CryptDecrypt.pas',
  Gamolf.RTL.UIElements in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.UIElements.pas',
  USVGInputPrompts in '..\assets\kenney.nl\InputPrompts\USVGInputPrompts.pas',
  USVGPuzzleAssets2 in '..\assets\kenney.nl\PuzzleAssets2\USVGPuzzleAssets2.pas',
  uScores in 'uScores.pas',
  Gamolf.RTL.GamepadDetected in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.GamepadDetected.pas',
  Gamolf.FMX.HelpBar in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.FMX.HelpBar.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TdmLogo, dmLogo);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
