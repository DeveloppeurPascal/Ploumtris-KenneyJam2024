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
/// File last update : 2024-07-28T18:05:20.000+02:00
/// Signature : 3f900f8c7d8edfcca910584a686a24ec0d83b275
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
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm1 = class(TForm)
    GridLayout1: TGridLayout;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ShowAll;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  uSVGToImages,
  USVGInputPrompts,
  USVGPuzzleAssets2;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowAll;
end;

procedure TForm1.ShowAll;
var
  img: timage;
  SVGPuzzle: TSVGPuzzleAssets2Index;
  SVGInputPrompts: TSVGInputPromptsIndex;
  lbl: TLabel;
begin
  for SVGPuzzle := low(TSVGPuzzleAssets2Index)
    to high(TSVGPuzzleAssets2Index) do
  begin
    img := timage.Create(self);
    img.Parent := GridLayout1;
    img.WrapMode := TImageWrapMode.Original;

    img.bitmap.Assign(getBitmapFromSVG(SVGPuzzle, img.width, img.height,
      img.bitmap.BitmapScale));

    lbl := TLabel.Create(self);
    lbl.Parent := img;
    lbl.Align := talignlayout.client;
    lbl.Text := GetEnumName(TypeInfo(TSVGPuzzleAssets2Index), ord(SVGPuzzle));
    lbl.TextSettings.HorzAlign := TTextAlign.Center;
  end;

  for SVGInputPrompts := low(TSVGInputPromptsIndex)
    to high(TSVGInputPromptsIndex) do
  begin
    img := timage.Create(self);
    img.Parent := GridLayout1;
    img.WrapMode := TImageWrapMode.Original;

    img.bitmap.Assign(getBitmapFromSVG(SVGInputPrompts, img.width, img.height,
      img.bitmap.BitmapScale));

    lbl := TLabel.Create(self);
    lbl.Parent := img;
    lbl.Align := talignlayout.client;
    lbl.Text := GetEnumName(TypeInfo(TSVGInputPromptsIndex),
      ord(SVGInputPrompts));
    lbl.TextSettings.HorzAlign := TTextAlign.Center;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
