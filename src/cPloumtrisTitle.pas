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
/// File last update : 2024-07-28T18:22:38.000+02:00
/// Signature : 3dcffb925cda51898f2fe3fcf2e6c5862db6ec37
/// ***************************************************************************
/// </summary>

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
