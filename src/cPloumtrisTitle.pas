unit cPloumtrisTitle;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  USVGPuzzleAssets2;

type
  TcadPloumtrisTitle = class(TFrame)
    GridLayout1: TGridLayout;
    Image1: TImage;
    procedure FrameResized(Sender: TObject);
  private
    procedure AddBitmap(const ID: TSVGPuzzleAssets2Index);
    procedure AddEmptyCell;
    procedure RefreshImages;
  protected
  public

  end;

implementation

{$R *.fmx}

uses
  uSVGToImages;

procedure TcadPloumtrisTitle.AddBitmap(const ID: TSVGPuzzleAssets2Index);
var
  img: TImage;
begin
  img := TImage.create(self);
  img.parent := GridLayout1;
  img.HitTest := false;
  img.Bitmap.assign(getBitmapFromSVG(ID, GridLayout1.ItemWidth,
    GridLayout1.ItemHeight, Image1.Bitmap.BitmapScale));
end;

procedure TcadPloumtrisTitle.AddEmptyCell;
var
  l: tlayout;
begin
  l := tlayout.create(self);
  l.parent := GridLayout1;
  l.HitTest := false;
end;

procedure TcadPloumtrisTitle.FrameResized(Sender: TObject);
begin
  RefreshImages;
end;

procedure TcadPloumtrisTitle.RefreshImages;
begin
  GridLayout1.Align := TAlignLayout.center;

  while (GridLayout1.ChildrenCount > 0) do
    GridLayout1.Children[0].free;

  GridLayout1.ItemWidth := trunc(width / 16);
  GridLayout1.ItemHeight := trunc(height / 3);
  if GridLayout1.ItemWidth > GridLayout1.ItemHeight then
    GridLayout1.ItemWidth := GridLayout1.ItemHeight
  else
    GridLayout1.ItemHeight := GridLayout1.ItemWidth;

  GridLayout1.width := GridLayout1.ItemWidth * 16;
  GridLayout1.height := GridLayout1.ItemHeight * 3;

  // P
  AddBitmap(TSVGPuzzleAssets2Index.eauDb);
  AddBitmap(TSVGPuzzleAssets2Index.eaugb);
  // l
  AddBitmap(TSVGPuzzleAssets2Index.Pipehb);
  // o
  AddEmptyCell;
  AddEmptyCell;
  // u
  AddEmptyCell;
  AddEmptyCell;
  // m
  AddEmptyCell;
  AddEmptyCell;
  // AddEmptyCell;
  // t
  AddBitmap(TSVGPuzzleAssets2Index.Pipegd);
  AddBitmap(TSVGPuzzleAssets2Index.eaugdb);
  AddBitmap(TSVGPuzzleAssets2Index.Pipegd);
  // r
  // AddEmptyCell;
  AddEmptyCell;
  // i
  AddEmptyCell;
  // s
  AddBitmap(TSVGPuzzleAssets2Index.PipeDb);
  AddBitmap(TSVGPuzzleAssets2Index.eaugb);
  // **********
  // P
  AddBitmap(TSVGPuzzleAssets2Index.eauhdb);
  AddBitmap(TSVGPuzzleAssets2Index.Pipehg);
  // l
  AddBitmap(TSVGPuzzleAssets2Index.Pipehb);
  // o
  AddBitmap(TSVGPuzzleAssets2Index.eauDb);
  AddBitmap(TSVGPuzzleAssets2Index.Pipegb);
  // u
  AddBitmap(TSVGPuzzleAssets2Index.Pipehb);
  AddBitmap(TSVGPuzzleAssets2Index.eauhb);
  // m
  AddBitmap(TSVGPuzzleAssets2Index.PipeDb);
  AddBitmap(TSVGPuzzleAssets2Index.eaugdb);
  AddBitmap(TSVGPuzzleAssets2Index.eaugb);
  // t
  AddBitmap(TSVGPuzzleAssets2Index.eauhb);
  // r
  AddBitmap(TSVGPuzzleAssets2Index.eauhdb);
  AddBitmap(TSVGPuzzleAssets2Index.Pipegb);
  // i
  AddBitmap(TSVGPuzzleAssets2Index.Pipehb);
  // s
  AddBitmap(TSVGPuzzleAssets2Index.eauhd);
  AddBitmap(TSVGPuzzleAssets2Index.eaugb);
  // **********
  // P
  AddBitmap(TSVGPuzzleAssets2Index.Pipehb);
  AddEmptyCell;
  // l
  AddBitmap(TSVGPuzzleAssets2Index.eauhb);
  // o
  AddBitmap(TSVGPuzzleAssets2Index.Pipehd);
  AddBitmap(TSVGPuzzleAssets2Index.eauhg);
  // u
  AddBitmap(TSVGPuzzleAssets2Index.Pipehd);
  AddBitmap(TSVGPuzzleAssets2Index.Pipehg);
  // m
  AddBitmap(TSVGPuzzleAssets2Index.eauhb);
  AddBitmap(TSVGPuzzleAssets2Index.Pipehb);
  AddBitmap(TSVGPuzzleAssets2Index.eauhb);
  // t
  AddBitmap(TSVGPuzzleAssets2Index.eauhb);
  // r
  AddBitmap(TSVGPuzzleAssets2Index.eauhb);
  AddEmptyCell;
  // i
  AddBitmap(TSVGPuzzleAssets2Index.Pipehb);
  // s
  AddBitmap(TSVGPuzzleAssets2Index.eauhd);
  AddBitmap(TSVGPuzzleAssets2Index.Pipehg);
end;

end.
