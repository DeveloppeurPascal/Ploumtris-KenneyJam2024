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
  Olf.Skia.SVGToBitmap;

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
  i: integer;
  lbl: TLabel;
  MargeHaut, MargeBas, MargeGauche, MargeDroite: single;
begin
  for i := 0 to length(SVGSVG) - 1 do
  begin
    // bmp := TOlfSVGBitmapList.Bitmap(i, round(GridLayout1.ItemWidth),
    // round(GridLayout1.Itemheight));
    // img := timage.Create(self);
    // img.Bitmap.Assign(bmp);
    // img.Parent := GridLayout1;
    MargeHaut := 0;
    MargeDroite := 0;
    MargeBas := 0;
    MargeGauche := 0;
    case TSVGSVGIndex(i) of
      TSVGSVGIndex.EauDb:
        begin
          MargeHaut := 100 * ((117.55 - 87.9) / 117.55);
          MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
        end;
      TSVGSVGIndex.EauGb:
        begin
          MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
          MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
        end;
      TSVGSVGIndex.EauGd:
        begin
          MargeHaut := 100 * ((117.55 - 58.6) / 117.55) / 2;
          MargeDroite := 0;
          MargeBas := 100 * ((117.55 - 58.6) / 117.55) / 2;
          MargeGauche := 0;
        end;
      TSVGSVGIndex.EauGdb:
        MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
      TSVGSVGIndex.EauGdh:
        MargeBas := 100 * ((117.55 - 87.9) / 117.55);
      TSVGSVGIndex.EauHb:
        begin
          MargeDroite := 100 * ((117.55 - 58.6) / 117.55) / 2;
          MargeGauche := 100 * ((117.55 - 58.6) / 117.55) / 2;
        end;
      TSVGSVGIndex.EauHd:
        begin
          MargeBas := 100 * ((117.55 - 87.9) / 117.55);
          MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
        end;
      TSVGSVGIndex.EauHdb:
        MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
      TSVGSVGIndex.EauHg:
        begin
          MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
          MargeBas := 100 * ((117.55 - 88.05) / 117.55);
        end;
      TSVGSVGIndex.EauHgb:
        MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
      TSVGSVGIndex.PipeDb:
        begin
          MargeHaut := 100 * ((117.55 - 87.9) / 117.55);
          MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
        end;
      TSVGSVGIndex.PipeGb:
        begin
          MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
          MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
        end;
      TSVGSVGIndex.PipeGd:
        begin
          MargeHaut := 100 * ((117.55 - 58.6) / 117.55) / 2;
          MargeBas := 100 * ((117.55 - 58.6) / 117.55) / 2;
        end;
      TSVGSVGIndex.PipeGdb:
        MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
      TSVGSVGIndex.PipeHb:
        begin
          MargeDroite := 100 * ((117.55 - 58.6) / 117.55) / 2;
          MargeGauche := 100 * ((117.55 - 58.6) / 117.55) / 2;
        end;
      TSVGSVGIndex.PipeHd:
        begin
          MargeBas := 100 * ((117.55 - 87.9) / 117.55);
          MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
        end;
      TSVGSVGIndex.PipeHdb:
        MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
      TSVGSVGIndex.PipeHg:
        begin
          MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
          MargeBas := 100 * ((117.55 - 88.05) / 117.55);
        end;
      TSVGSVGIndex.PipeHgb:
        MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
      TSVGSVGIndex.PipeHgd:
        MargeBas := 100 * ((117.55 - 87.9) / 117.55);
    end;
    bmp := TOlfSVGBitmapList.Bitmap(i,
      round(GridLayout1.ItemWidth * (100 - MargeGauche - MargeDroite) / 100),
      round(GridLayout1.Itemheight * (100 - MargeHaut - MargeBas) / 100));
    img := timage.Create(self);
    img.Parent := GridLayout1;
    img.Bitmap.SetSize(round(img.Width), round(img.Height));
    img.Bitmap.Clear(TAlphaColors.Snow);
    img.Bitmap.Canvas.BeginScene;
    try
      img.Bitmap.Canvas.DrawBitmap(bmp, bmp.BoundsF,
        trectf.Create(img.Bitmap.Width * MargeGauche / 100,
        img.Bitmap.Height * MargeHaut / 100, bmp.Width + img.Bitmap.Width *
        MargeGauche / 100, bmp.Height + img.Bitmap.Height * MargeHaut /
        100), 1);
    finally
      img.Bitmap.Canvas.EndScene;
    end;
    lbl := TLabel.Create(self);
    lbl.Parent := img;
    lbl.Align := talignlayout.client;
    lbl.Text := GetEnumName(TypeInfo(TSVGSVGIndex), i);
    lbl.TextSettings.HorzAlign := TTextAlign.Center;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
