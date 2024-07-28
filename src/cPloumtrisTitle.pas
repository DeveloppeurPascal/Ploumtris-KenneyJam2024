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
  PuzzleAssets2,
  FMX.Objects;

type
  TcadPloumtrisTitle = class(TFrame)
    GridLayout1: TGridLayout;
    Image1: TImage;
    procedure FrameResized(Sender: TObject);
  private
    procedure AddBitmap(const ID: tsvgsvgindex);
    procedure AddEmptyCell;
    procedure RefreshImages;
  protected
  public

  end;

implementation

{$R *.fmx}

uses
  uSVGToImages;

procedure TcadPloumtrisTitle.AddBitmap(const ID: tsvgsvgindex);
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
  AddBitmap(tsvgsvgindex.eauDb);
  AddBitmap(tsvgsvgindex.eaugb);
  // l
  AddBitmap(tsvgsvgindex.Pipehb);
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
  AddBitmap(tsvgsvgindex.Pipegd);
  AddBitmap(tsvgsvgindex.eaugdb);
  AddBitmap(tsvgsvgindex.Pipegd);
  // r
  // AddEmptyCell;
  AddEmptyCell;
  // i
  AddEmptyCell;
  // s
  AddBitmap(tsvgsvgindex.PipeDb);
  AddBitmap(tsvgsvgindex.eaugb);
  // **********
  // P
  AddBitmap(tsvgsvgindex.eauhdb);
  AddBitmap(tsvgsvgindex.Pipehg);
  // l
  AddBitmap(tsvgsvgindex.Pipehb);
  // o
  AddBitmap(tsvgsvgindex.eauDb);
  AddBitmap(tsvgsvgindex.Pipegb);
  // u
  AddBitmap(tsvgsvgindex.Pipehb);
  AddBitmap(tsvgsvgindex.eauhb);
  // m
  AddBitmap(tsvgsvgindex.PipeDb);
  AddBitmap(tsvgsvgindex.eaugdb);
  AddBitmap(tsvgsvgindex.eaugb);
  // t
  AddBitmap(tsvgsvgindex.eauhb);
  // r
  AddBitmap(tsvgsvgindex.eauhdb);
  AddBitmap(tsvgsvgindex.Pipegb);
  // i
  AddBitmap(tsvgsvgindex.Pipehb);
  // s
  AddBitmap(tsvgsvgindex.eauhd);
  AddBitmap(tsvgsvgindex.eaugb);
  // **********
  // P
  AddBitmap(tsvgsvgindex.Pipehb);
  AddEmptyCell;
  // l
  AddBitmap(tsvgsvgindex.eauhb);
  // o
  AddBitmap(tsvgsvgindex.Pipehd);
  AddBitmap(tsvgsvgindex.eauhg);
  // u
  AddBitmap(tsvgsvgindex.Pipehd);
  AddBitmap(tsvgsvgindex.Pipehg);
  // m
  AddBitmap(tsvgsvgindex.eauhb);
  AddBitmap(tsvgsvgindex.Pipehb);
  AddBitmap(tsvgsvgindex.eauhb);
  // t
  AddBitmap(tsvgsvgindex.eauhb);
  // r
  AddBitmap(tsvgsvgindex.eauhb);
  AddEmptyCell;
  // i
  AddBitmap(tsvgsvgindex.Pipehb);
  // s
  AddBitmap(tsvgsvgindex.eauhd);
  AddBitmap(tsvgsvgindex.Pipehg);
end;

end.
