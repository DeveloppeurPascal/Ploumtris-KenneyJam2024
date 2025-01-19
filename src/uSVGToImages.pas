unit uSVGToImages;

interface

uses
  System.Types,
  System.UITypes,
  FMX.Graphics,
  USVGInputPrompts,
  USVGPuzzleAssets2;

function getBitmapFromSVG(const Index: TSVGPuzzleAssets2Index;
  const width, height: single; const BitmapScale: single): tbitmap; overload;

function getBitmapFromSVG(const Index: TSVGInputPromptsIndex;
  const width, height: single; const BitmapScale: single): tbitmap; overload;

implementation

uses
  Olf.Skia.SVGToBitmap;

function getBitmapFromSVG(const Index: TSVGInputPromptsIndex;
  const width, height: single; const BitmapScale: single): tbitmap;
begin
  result := TOlfSVGBitmapList.Bitmap(ord(Index) + TSVGInputPrompts.Tag,
    round(width), round(height), BitmapScale);
end;

function getBitmapFromSVG(const Index: TSVGPuzzleAssets2Index;
  const width, height: single; const BitmapScale: single): tbitmap;
var
  MargeHaut, MargeBas, MargeGauche, MargeDroite: single;
begin
  MargeHaut := 0;
  MargeDroite := 0;
  MargeBas := 0;
  MargeGauche := 0;
  case Index of
    TSVGPuzzleAssets2Index.EauDb:
      begin
        MargeHaut := 100 * ((117.55 - 87.9) / 117.55);
        MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
      end;
    TSVGPuzzleAssets2Index.EauGb:
      begin
        MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
        MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
      end;
    TSVGPuzzleAssets2Index.EauGd:
      begin
        MargeHaut := 100 * ((117.55 - 58.6) / 117.55) / 2;
        MargeDroite := 0;
        MargeBas := 100 * ((117.55 - 58.6) / 117.55) / 2;
        MargeGauche := 0;
      end;
    TSVGPuzzleAssets2Index.EauGdb:
      MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
    TSVGPuzzleAssets2Index.EauGdh:
      MargeBas := 100 * ((117.55 - 87.9) / 117.55);
    TSVGPuzzleAssets2Index.EauHb:
      begin
        MargeDroite := 100 * ((117.55 - 58.6) / 117.55) / 2;
        MargeGauche := 100 * ((117.55 - 58.6) / 117.55) / 2;
      end;
    TSVGPuzzleAssets2Index.EauHd:
      begin
        MargeBas := 100 * ((117.55 - 87.9) / 117.55);
        MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
      end;
    TSVGPuzzleAssets2Index.EauHdb:
      MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
    TSVGPuzzleAssets2Index.EauHg:
      begin
        MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
        MargeBas := 100 * ((117.55 - 88.05) / 117.55);
      end;
    TSVGPuzzleAssets2Index.EauHgb:
      MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
    TSVGPuzzleAssets2Index.PipeDb:
      begin
        MargeHaut := 100 * ((117.55 - 87.9) / 117.55);
        MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
      end;
    TSVGPuzzleAssets2Index.PipeGb:
      begin
        MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
        MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
      end;
    TSVGPuzzleAssets2Index.PipeGd:
      begin
        MargeHaut := 100 * ((117.55 - 58.6) / 117.55) / 2;
        MargeBas := 100 * ((117.55 - 58.6) / 117.55) / 2;
      end;
    TSVGPuzzleAssets2Index.PipeGdb:
      MargeHaut := 100 * ((117.55 - 88.05) / 117.55);
    TSVGPuzzleAssets2Index.PipeHb:
      begin
        MargeDroite := 100 * ((117.55 - 58.6) / 117.55) / 2;
        MargeGauche := 100 * ((117.55 - 58.6) / 117.55) / 2;
      end;
    TSVGPuzzleAssets2Index.PipeHd:
      begin
        MargeBas := 100 * ((117.55 - 87.9) / 117.55);
        MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
      end;
    TSVGPuzzleAssets2Index.PipeHdb:
      MargeGauche := 100 * ((117.55 - 88.05) / 117.55);
    TSVGPuzzleAssets2Index.PipeHg:
      begin
        MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
        MargeBas := 100 * ((117.55 - 88.05) / 117.55);
      end;
    TSVGPuzzleAssets2Index.PipeHgb:
      MargeDroite := 100 * ((117.55 - 87.9) / 117.55);
    TSVGPuzzleAssets2Index.PipeHgd:
      MargeBas := 100 * ((117.55 - 87.9) / 117.55);
    TSVGPuzzleAssets2Index.BtnOn:
      begin
        MargeHaut := 100 * ((54 - 44) / 54) / 2;
        MargeBas := 100 * ((54 - 44) / 54) / 2;
        MargeGauche := 100 * ((104 - 94) / 104) / 2;
        MargeDroite := 100 * ((104 - 94) / 104) / 2;
      end;
  end;
  result := TOlfSVGBitmapList.Bitmap(TSVGPuzzleAssets2.Tag, ord(Index),
    round(width), round(height), MargeHaut, MargeDroite, MargeBas, MargeGauche,
    BitmapScale);
end;

procedure RegisterSVGImages;
begin
  TSVGPuzzleAssets2.Tag := TOlfSVGBitmapList.AddAList;
  TOlfSVGBitmapList.AddItem(TSVGPuzzleAssets2.Tag, SVGPuzzleAssets2);
  TSVGInputPrompts.Tag := TOlfSVGBitmapList.AddAList;
  TOlfSVGBitmapList.AddItem(TSVGInputPrompts.Tag, SVGInputPrompts);
end;

initialization

RegisterSVGImages;

end.
