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
    btnSaveTitleImageToDownloadsPath: TButton;
    cadPloumtrisTitle1: TcadPloumtrisTitle;
    cadPloumtrisTitle2: TcadPloumtrisTitle;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveTitleImageToDownloadsPathClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
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
  uSVGToImages,
  uUIItemsList,
  Gamolf.RTL.Joystick,
  Gamolf.RTL.UIElements;

procedure TfrmPloumtrisTitle.AddBitmap(const ID: tsvgsvgindex);
var
  img: TImage;
begin
  img := TImage.create(self);
  img.parent := GridLayout1;
  img.Bitmap.assign(getBitmapFromSVG(ID, GridLayout1.ItemWidth,
    GridLayout1.ItemHeight, Image1.Bitmap.BitmapScale));
end;

procedure TfrmPloumtrisTitle.AddEmptyCell;
var
  l: tlayout;
begin
  l := tlayout.create(self);
  l.parent := GridLayout1;
end;

procedure TfrmPloumtrisTitle.btnSaveTitleImageToDownloadsPathClick
  (Sender: TObject);
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
  ShowMessage('Title image exported.');
end;

procedure TfrmPloumtrisTitle.FormCreate(Sender: TObject);
  function AddItem(const btn: TButton): TUIElement;
  begin
    result := UIItems.AddUIItem(btn.BoundsRect,
      procedure(const Sender: TObject)
      begin
        if not assigned(Sender) then
          exit;
        if not(Sender is TUIElement) then
          exit;
        if not assigned((Sender as TUIElement).tagobject) then
          exit;
        if not((Sender as TUIElement).tagobject is tcontrol) then
          exit;

        if assigned(((Sender as TUIElement).tagobject as tcontrol).OnClick) then
          ((Sender as TUIElement).tagobject as tcontrol)
            .OnClick(((Sender as TUIElement).tagobject as tcontrol));
      end);
    result.tagobject := btn;
    result.OnPaintProc := procedure(const Sender: TObject)
      begin
        if not assigned(Sender) then
          exit;
        if not(Sender is TUIElement) then
          exit;
        if not assigned((Sender as TUIElement).tagobject) then
          exit;
        if not((Sender as TUIElement).tagobject is tcontrol) then
          exit;

        if (Sender as TUIElement).IsFocused then
          ((Sender as TUIElement).tagobject as tcontrol).SetFocus
        else
          ((Sender as TUIElement).tagobject as tcontrol).resetFocus;
      end;
  end;

var
  item: TUIElement;
begin
  UIItems.NewLayout;
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      tthread.forcequeue(nil,
        procedure
        begin
          Close;
        end);
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.b];

  item := AddItem(btnSaveTitleImageToDownloadsPath);
  item.SetFocus;

  //

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

procedure TfrmPloumtrisTitle.FormDestroy(Sender: TObject);
begin
  UIItems.RemoveLayout;
end;

procedure TfrmPloumtrisTitle.FormKeyDown(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
begin
  UIItems.KeyDown(Key, KeyChar, Shift);
end;

end.
