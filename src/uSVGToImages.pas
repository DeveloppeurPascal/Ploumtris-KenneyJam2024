unit uSVGToImages;

interface

uses
  System.Types,
  System.UITypes,
  FMX.Graphics,
  PuzzleAssets2;

function getBitmapFromSVG(const Index: TSVGSVGIndex;
  const width, height: single; const BitmapScale: single;
  const BackgroundColor: TAlphaColor): tbitmap;

implementation

uses
  Olf.Skia.SVGToBitmap;

function getBitmapFromSVG(const Index: TSVGSVGIndex;
  const width, height: single; const BitmapScale: single;
  const BackgroundColor: TAlphaColor): tbitmap;
var
  MargeHaut, MargeBas, MargeGauche, MargeDroite: single;
  bmp: tbitmap;
begin
  MargeHaut := 0;
  MargeDroite := 0;
  MargeBas := 0;
  MargeGauche := 0;
  case Index of
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
    TSVGSVGIndex.BtnOn:
      begin
        MargeHaut := 100 * ((54 - 44) / 54) / 2;
        MargeBas := 100 * ((54 - 44) / 54) / 2;
        MargeGauche := 100 * ((104 - 94) / 104) / 2;
        MargeDroite := 100 * ((104 - 94) / 104) / 2;
      end;
  end;
  bmp := TOlfSVGBitmapList.Bitmap(ord(Index),
    round(width * (100 - MargeGauche - MargeDroite) / 100),
    round(height * (100 - MargeHaut - MargeBas) / 100), BitmapScale);

  result := tbitmap.Create(round(width * BitmapScale),
    round(height * BitmapScale));
  result.BitmapScale := BitmapScale;
  result.Clear(BackgroundColor);
  result.Canvas.BeginScene;
  try
    result.Canvas.DrawBitmap(bmp, bmp.BoundsF,
      trectf.Create((result.width * MargeGauche / 100) / BitmapScale,
      (result.height * MargeHaut / 100) / BitmapScale,
      (bmp.width + result.width * MargeGauche / 100) / BitmapScale,
      (bmp.height + result.height * MargeHaut / 100) / BitmapScale), 1);
  finally
    result.Canvas.EndScene;
  end;
end;

procedure RegisterSVGImages;
var
  i: Integer;
begin
  for i := 0 to length(SVGSVG) - 1 do
    TOlfSVGBitmapList.AddItemAt(i, SVGSVG[i]);
end;

initialization

RegisterSVGImages;

end.
