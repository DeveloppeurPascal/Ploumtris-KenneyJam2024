program FrameTests;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {frmMain},
  fTextButtons in 'fTextButtons.pas' {frmTextButtons},
  uUIItemsList in '..\src\uUIItemsList.pas',
  Gamolf.FMX.Joystick in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.FMX.Joystick.pas',
  Gamolf.RTL.Joystick.DirectInput.Win in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Joystick.DirectInput.Win.pas',
  Gamolf.RTL.Joystick.Mac in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Joystick.Mac.pas',
  Gamolf.RTL.Joystick in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Joystick.pas',
  iOSapi.GameController in '..\lib-externes\Delphi-Game-Engine\src\iOSapi.GameController.pas',
  Macapi.GameController in '..\lib-externes\Delphi-Game-Engine\src\Macapi.GameController.pas',
  dmGamepad in 'dmGamepad.pas' {DataModule1: TDataModule},
  fTrackBar in 'fTrackBar.pas' {frmTrackBar},
  fCheckBox in 'fCheckBox.pas' {frmCheckBox},
  fDialogBox in 'fDialogBox.pas' {frmDialogBox},
  fImageButtons in 'fImageButtons.pas' {frmImageButtons},
  uSVGToImages in '..\src\uSVGToImages.pas',
  cTextButton in '..\src\cTextButton.pas' {cadTextButton: TFrame},
  Olf.Skia.SVGToBitmap in '..\lib-externes\librairies\src\Olf.Skia.SVGToBitmap.pas',
  cDialogBox in '..\src\cDialogBox.pas' {cadDialogBox: TFrame},
  fPloumtrisTitle in 'fPloumtrisTitle.pas' {frmPloumtrisTitle},
  uConsts in '..\src\uConsts.pas',
  cPloumtrisTitle in '..\src\cPloumtrisTitle.pas' {cadPloumtrisTitle: TFrame},
  Gamolf.RTL.UIElements in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.UIElements.pas',
  USVGInputPrompts in '..\assets\kenney.nl\InputPrompts\USVGInputPrompts.pas',
  USVGPuzzleAssets2 in '..\assets\kenney.nl\PuzzleAssets2\USVGPuzzleAssets2.pas',
  Gamolf.RTL.Joystick.Deprecated in '..\lib-externes\Delphi-Game-Engine\src\Gamolf.RTL.Joystick.Deprecated.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;

end.
