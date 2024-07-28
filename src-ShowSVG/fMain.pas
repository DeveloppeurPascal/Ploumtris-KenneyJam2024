unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm1 = class(TForm)
    GridLayout1: TGridLayout;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ShowAll;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  uSVGToImages,
  USVGInputPrompts,
  USVGPuzzleAssets2;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowAll;
end;

procedure TForm1.ShowAll;
var
  img: timage;
  SVGPuzzle: TSVGPuzzleAssets2Index;
  SVGInputPrompts: TSVGInputPromptsIndex;
  lbl: TLabel;
begin
  for SVGPuzzle := low(TSVGPuzzleAssets2Index)
    to high(TSVGPuzzleAssets2Index) do
  begin
    img := timage.Create(self);
    img.Parent := GridLayout1;
    img.WrapMode := TImageWrapMode.Original;

    img.bitmap.Assign(getBitmapFromSVG(SVGPuzzle, img.width, img.height,
      img.bitmap.BitmapScale));

    lbl := TLabel.Create(self);
    lbl.Parent := img;
    lbl.Align := talignlayout.client;
    lbl.Text := GetEnumName(TypeInfo(TSVGPuzzleAssets2Index), ord(SVGPuzzle));
    lbl.TextSettings.HorzAlign := TTextAlign.Center;
  end;

  for SVGInputPrompts := low(TSVGInputPromptsIndex)
    to high(TSVGInputPromptsIndex) do
  begin
    img := timage.Create(self);
    img.Parent := GridLayout1;
    img.WrapMode := TImageWrapMode.Original;

    img.bitmap.Assign(getBitmapFromSVG(SVGInputPrompts, img.width, img.height,
      img.bitmap.BitmapScale));

    lbl := TLabel.Create(self);
    lbl.Parent := img;
    lbl.Align := talignlayout.client;
    lbl.Text := GetEnumName(TypeInfo(TSVGInputPromptsIndex),
      ord(SVGInputPrompts));
    lbl.TextSettings.HorzAlign := TTextAlign.Center;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
