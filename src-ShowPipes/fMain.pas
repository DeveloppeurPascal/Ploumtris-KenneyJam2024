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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  PuzzleAssets2,
  FMX.Objects;

const
  CTileSize = 128;

type
  TForm2 = class(TForm)
    FlowLayout1: TFlowLayout;
    GridLayout1: TGridLayout;
    GridLayout2: TGridLayout;
    Switch1: TSwitch;
    GridLayout3: TGridLayout;
    GridLayout4: TGridLayout;
    procedure Switch1Switch(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure InitSVGToBitmap;
    procedure ShowGridFromSwitch;
    procedure AddImage(const Id: TSVGSVGIndex; const GL: TGridLayout);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  Olf.Skia.SVGToBitmap,
  uSVGToImages;

{ TForm2 }

procedure TForm2.AddImage(const Id: TSVGSVGIndex; const GL: TGridLayout);
var
  bmp: tbitmap;
  img: timage;
  lbl: TLabel;
begin
  img := timage.Create(self);
  img.Parent := gl;
  img.WrapMode := TImageWrapMode.Original;

  bmp := getBitmapFromSVG(Id, img.width, img.height, img.bitmap.BitmapScale,
    talphacolors.Yellow);
  try
    img.bitmap.Assign(bmp);
  finally
    bmp.free;
  end;

  lbl := TLabel.Create(self);
  lbl.Parent := img;
  lbl.Align := talignlayout.client;
  lbl.Text := GetEnumName(TypeInfo(TSVGSVGIndex), ord(Id));
  lbl.TextSettings.HorzAlign := TTextAlign.Center;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  InitSVGToBitmap;
  ShowGridFromSwitch;
  GridLayout1.ItemWidth := CTileSize;
  GridLayout1.Itemheight := CTileSize;
  GridLayout1.width := GridLayout1.ItemWidth * 4;
  GridLayout1.height := GridLayout1.Itemheight * 4;

  GridLayout2.ItemWidth := GridLayout1.ItemWidth;
  GridLayout2.Itemheight := GridLayout1.Itemheight;
  GridLayout2.width := GridLayout1.width;
  GridLayout2.height := GridLayout1.height;

  GridLayout3.ItemWidth := GridLayout1.ItemWidth;
  GridLayout3.Itemheight := GridLayout1.Itemheight;
  GridLayout3.width := GridLayout3.ItemWidth * 3;
  GridLayout3.height := GridLayout3.Itemheight * 3;

  GridLayout4.ItemWidth := GridLayout3.ItemWidth;
  GridLayout4.Itemheight := GridLayout3.Itemheight;
  GridLayout4.width := GridLayout3.width;
  GridLayout4.height := GridLayout3.height;

  AddImage(TSVGSVGIndex.PipeDb, GridLayout1);
  AddImage(TSVGSVGIndex.PipeGd, GridLayout1);
  AddImage(TSVGSVGIndex.PipeGdb, GridLayout1);
  AddImage(TSVGSVGIndex.PipeGb, GridLayout1);

  AddImage(TSVGSVGIndex.PipeHb, GridLayout1);
  AddImage(TSVGSVGIndex.PipeDb, GridLayout1);
  AddImage(TSVGSVGIndex.PipeHdbg, GridLayout1);
  AddImage(TSVGSVGIndex.PipeHgb, GridLayout1);

  AddImage(TSVGSVGIndex.PipeHdb, GridLayout1);
  AddImage(TSVGSVGIndex.PipeHdbg, GridLayout1);
  AddImage(TSVGSVGIndex.PipeHg, GridLayout1);
  AddImage(TSVGSVGIndex.PipeHb, GridLayout1);

  AddImage(TSVGSVGIndex.PipeHd, GridLayout1);
  AddImage(TSVGSVGIndex.PipeHgd, GridLayout1);
  AddImage(TSVGSVGIndex.PipeGd, GridLayout1);
  AddImage(TSVGSVGIndex.PipeHg, GridLayout1);

  AddImage(TSVGSVGIndex.EauDb, GridLayout2);
  AddImage(TSVGSVGIndex.EauGd, GridLayout2);
  AddImage(TSVGSVGIndex.EauGdb, GridLayout2);
  AddImage(TSVGSVGIndex.EauGb, GridLayout2);

  AddImage(TSVGSVGIndex.EauHb, GridLayout2);
  AddImage(TSVGSVGIndex.EauDb, GridLayout2);
  AddImage(TSVGSVGIndex.EauHdbg, GridLayout2);
  AddImage(TSVGSVGIndex.EauHgb, GridLayout2);

  AddImage(TSVGSVGIndex.EauHdb, GridLayout2);
  AddImage(TSVGSVGIndex.EauHdbg, GridLayout2);
  AddImage(TSVGSVGIndex.EauHg, GridLayout2);
  AddImage(TSVGSVGIndex.EauHb, GridLayout2);

  AddImage(TSVGSVGIndex.EauHd, GridLayout2);
  AddImage(TSVGSVGIndex.EauGdh, GridLayout2);
  AddImage(TSVGSVGIndex.EauGd, GridLayout2);
  AddImage(TSVGSVGIndex.EauHg, GridLayout2);

  AddImage(TSVGSVGIndex.PipeDb, GridLayout3);
  AddImage(TSVGSVGIndex.PipeHb, GridLayout3);
  AddImage(TSVGSVGIndex.PipeGb, GridLayout3);

  AddImage(TSVGSVGIndex.PipeGd, GridLayout3);
  AddImage(TSVGSVGIndex.PipeHdbg, GridLayout3);
  AddImage(TSVGSVGIndex.PipeGd, GridLayout3);

  AddImage(TSVGSVGIndex.PipeHd, GridLayout3);
  AddImage(TSVGSVGIndex.PipeHb, GridLayout3);
  AddImage(TSVGSVGIndex.PipeHg, GridLayout3);

  AddImage(TSVGSVGIndex.EauDb, GridLayout4);
  AddImage(TSVGSVGIndex.EauHb, GridLayout4);
  AddImage(TSVGSVGIndex.EauGb, GridLayout4);

  AddImage(TSVGSVGIndex.EauGd, GridLayout4);
  AddImage(TSVGSVGIndex.EauHdbg, GridLayout4);
  AddImage(TSVGSVGIndex.EauGd, GridLayout4);

  AddImage(TSVGSVGIndex.EauHd, GridLayout4);
  AddImage(TSVGSVGIndex.EauHb, GridLayout4);
  AddImage(TSVGSVGIndex.EauHg, GridLayout4);
end;

procedure TForm2.InitSVGToBitmap;
var
  i: integer;
begin
  for i := 0 to length(SVGSVG) - 1 do
    TOlfSVGBitmapList.AddItemAt(i, SVGSVG[i]);
end;

procedure TForm2.ShowGridFromSwitch;
begin
  GridLayout1.Visible := Switch1.IsChecked;
  GridLayout2.Visible := not GridLayout1.Visible;

  GridLayout3.Visible := GridLayout1.Visible;
  GridLayout4.Visible := not GridLayout3.Visible;
end;

procedure TForm2.Switch1Switch(Sender: TObject);
begin
  ShowGridFromSwitch;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
