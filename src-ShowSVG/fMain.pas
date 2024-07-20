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
    procedure InitSVGToBitmap;
    procedure ShowAll;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  PuzzleAssets2,
  Olf.Skia.SVGToBitmap,
  uSVGToImages;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitSVGToBitmap;
  ShowAll;
end;

procedure TForm1.InitSVGToBitmap;
var
  i: integer;
begin
  for i := 0 to length(SVGSVG) - 1 do
    TOlfSVGBitmapList.AddItemAt(i, SVGSVG[i]);
end;

procedure TForm1.ShowAll;
var
  bmp: tbitmap;
  img: timage;
  SVGIndex: TSVGSVGIndex;
  lbl: TLabel;
begin
  for SVGIndex := low(TSVGSVGIndex) to high(TSVGSVGIndex) do
  begin
    img := timage.Create(self);
    img.Parent := GridLayout1;
    img.WrapMode := TImageWrapMode.Original;

    bmp := getBitmapFromSVG(SVGIndex, img.width, img.height,
      img.bitmap.BitmapScale, talphacolors.Yellow);
    try
      img.bitmap.Assign(bmp);
    finally
      bmp.free;
    end;

    lbl := TLabel.Create(self);
    lbl.Parent := img;
    lbl.Align := talignlayout.client;
    lbl.Text := GetEnumName(TypeInfo(TSVGSVGIndex), ord(SVGIndex));
    lbl.TextSettings.HorzAlign := TTextAlign.Center;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
