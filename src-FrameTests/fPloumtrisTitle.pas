/// <summary>
/// ***************************************************************************
///
/// Ploumtris
///
/// Copyright 2024-2025 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://ploumtris.gamolf.fr/
///
/// Project site :
/// https://github.com/DeveloppeurPascal/Ploumtris-KenneyJam2024
///
/// ***************************************************************************
/// File last update : 2024-07-28T18:23:04.000+02:00
/// Signature : 87d28a206924310997d10b2db3da967f05d0ee36
/// ***************************************************************************
/// </summary>

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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  cPloumtrisTitle,
  USVGPuzzleAssets2;

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
    procedure AddBitmap(const ID: TSVGPuzzleAssets2Index);
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

procedure TfrmPloumtrisTitle.AddBitmap(const ID: TSVGPuzzleAssets2Index);
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
