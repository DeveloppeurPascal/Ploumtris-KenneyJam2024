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
