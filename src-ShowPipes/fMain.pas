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
/// File last update : 2024-07-28T18:07:50.000+02:00
/// Signature : fdc9e50550355bdd43685a2eeaf3546ec2f66691
/// ***************************************************************************
/// </summary>

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
  FMX.Objects,
  USVGPuzzleAssets2;

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
  public
    procedure ShowGridFromSwitch;
    procedure AddImage(const Id: TSVGPuzzleAssets2Index; const GL: TGridLayout);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  uSVGToImages;

{ TForm2 }

procedure TForm2.AddImage(const Id: TSVGPuzzleAssets2Index;
  const GL: TGridLayout);
var
  img: timage;
  lbl: TLabel;
begin
  img := timage.Create(self);
  img.Parent := GL;
  img.WrapMode := TImageWrapMode.Original;

  img.bitmap.Assign(getBitmapFromSVG(Id, img.width, img.height,
    img.bitmap.BitmapScale));

  lbl := TLabel.Create(self);
  lbl.Parent := img;
  lbl.Align := talignlayout.client;
  lbl.Text := GetEnumName(TypeInfo(TSVGPuzzleAssets2Index), ord(Id));
  lbl.TextSettings.HorzAlign := TTextAlign.Center;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
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

  AddImage(TSVGPuzzleAssets2Index.PipeDb, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeGd, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeGdb, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeGb, GridLayout1);

  AddImage(TSVGPuzzleAssets2Index.PipeHb, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeDb, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeHdbg, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeHgb, GridLayout1);

  AddImage(TSVGPuzzleAssets2Index.PipeHdb, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeHdbg, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeHg, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeHb, GridLayout1);

  AddImage(TSVGPuzzleAssets2Index.PipeHd, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeHgd, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeGd, GridLayout1);
  AddImage(TSVGPuzzleAssets2Index.PipeHg, GridLayout1);

  AddImage(TSVGPuzzleAssets2Index.EauDb, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauGd, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauGdb, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauGb, GridLayout2);

  AddImage(TSVGPuzzleAssets2Index.EauHb, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauDb, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauHdbg, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauHgb, GridLayout2);

  AddImage(TSVGPuzzleAssets2Index.EauHdb, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauHdbg, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauHg, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauHb, GridLayout2);

  AddImage(TSVGPuzzleAssets2Index.EauHd, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauGdh, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauGd, GridLayout2);
  AddImage(TSVGPuzzleAssets2Index.EauHg, GridLayout2);

  AddImage(TSVGPuzzleAssets2Index.PipeDb, GridLayout3);
  AddImage(TSVGPuzzleAssets2Index.PipeHb, GridLayout3);
  AddImage(TSVGPuzzleAssets2Index.PipeGb, GridLayout3);

  AddImage(TSVGPuzzleAssets2Index.PipeGd, GridLayout3);
  AddImage(TSVGPuzzleAssets2Index.PipeHdbg, GridLayout3);
  AddImage(TSVGPuzzleAssets2Index.PipeGd, GridLayout3);

  AddImage(TSVGPuzzleAssets2Index.PipeHd, GridLayout3);
  AddImage(TSVGPuzzleAssets2Index.PipeHb, GridLayout3);
  AddImage(TSVGPuzzleAssets2Index.PipeHg, GridLayout3);

  AddImage(TSVGPuzzleAssets2Index.EauDb, GridLayout4);
  AddImage(TSVGPuzzleAssets2Index.EauHb, GridLayout4);
  AddImage(TSVGPuzzleAssets2Index.EauGb, GridLayout4);

  AddImage(TSVGPuzzleAssets2Index.EauGd, GridLayout4);
  AddImage(TSVGPuzzleAssets2Index.EauHdbg, GridLayout4);
  AddImage(TSVGPuzzleAssets2Index.EauGd, GridLayout4);

  AddImage(TSVGPuzzleAssets2Index.EauHd, GridLayout4);
  AddImage(TSVGPuzzleAssets2Index.EauHb, GridLayout4);
  AddImage(TSVGPuzzleAssets2Index.EauHg, GridLayout4);
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
