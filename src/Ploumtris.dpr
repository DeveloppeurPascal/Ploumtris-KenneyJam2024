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
  cGameControllerStatus in 'cGameControllerStatus.pas' {cadGameControllerStatus: TFrame},
  uScores in 'uScores.pas',
  cHelpBar in 'cHelpBar.pas' {cadHelpBar: TFrame},
  Gamolf.RTL.Joystick.Deprecated in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Joystick.Deprecated.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TdmLogo, dmLogo);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
