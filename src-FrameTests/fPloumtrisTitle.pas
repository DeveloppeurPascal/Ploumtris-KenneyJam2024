unit fPloumtrisTitle;

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
  FMX.Layouts,
  FMX.Objects,
  PuzzleAssets2,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  cPloumtrisTitle;

type
  TfrmPloumtrisTitle = class(TForm)
    GridLayout1: TGridLayout;
    HorzScrollBox1: THorzScrollBox;
    Image1: TImage;
    Button1: TButton;
    cadPloumtrisTitle1: TcadPloumtrisTitle;
    cadPloumtrisTitle2: TcadPloumtrisTitle;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure AddBitmap(const ID: tsvgsvgindex);
    procedure AddEmptyCell;
  end;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  uConsts,
  uSVGToImages;

procedure TfrmPloumtrisTitle.AddBitmap(const ID: tsvgsvgindex);
var
  bmp: tbitmap;
  img: TImage;
begin
  bmp := getBitmapFromSVG(ID, GridLayout1.ItemWidth, GridLayout1.ItemHeight,
    Image1.Bitmap.BitmapScale, 0);
  try
    img := TImage.create(self);
    img.parent := GridLayout1;
    img.Bitmap.assign(bmp);
  finally
    bmp.free;
  end;
end;

procedure TfrmPloumtrisTitle.AddEmptyCell;
var
  l: tlayout;
begin
  l := tlayout.create(self);
  l.parent := GridLayout1;
end;

procedure TfrmPloumtrisTitle.Button1Click(Sender: TObject);
var
  bmp: tbitmap;
begin
  bmp := GridLayout1.MakeScreenshot;
  try
    bmp.SaveToFile(tpath.combine(tpath.GetDownloadsPath,
      'Ploumtris-Title-' + bmp.width.tostring + 'x' + bmp.height.tostring
      + '.png'));
  finally
    bmp.free;
  end;
end;

procedure TfrmPloumtrisTitle.FormCreate(Sender: TObject);
begin
  GridLayout1.Position.x := 0;
  GridLayout1.Position.y := 0;

  GridLayout1.ItemWidth := CPipeSize;
  GridLayout1.ItemHeight := GridLayout1.ItemWidth;

  GridLayout1.width := GridLayout1.ItemWidth * 16;
  GridLayout1.height := GridLayout1.ItemHeight * 3;

  width := round(GridLayout1.width + 50);

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
