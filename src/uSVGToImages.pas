unit uSVGToImages;

interface

uses
  System.Types,
  System.UITypes,
  FMX.Graphics,
  PuzzleAssets2;

function getBitmapFromSVG(const Index: TSVGSVGIndex;
  const width, height: single; const BitmapScale: single): tbitmap;

implementation

uses
  Olf.Skia.SVGToBitmap;

function getBitmapFromSVG(const Index: TSVGSVGIndex;
  const width, height: single; const BitmapScale: single): tbitmap;
var
  MargeHaut, MargeBas, MargeGauche, MargeDroite: single;
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
  result := TOlfSVGBitmapList.Bitmap(ord(Index) + tsvgsvg.Tag, round(width),
    round(height), MargeHaut, MargeDroite, MargeBas, MargeGauche, BitmapScale);
end;

procedure RegisterSVGImages;
begin
  tsvgsvg.Tag := TOlfSVGBitmapList.AddItem(SVGSVG);
end;

initialization

RegisterSVGImages;

end.
