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
/// File last update : 2024-07-28T18:20:16.000+02:00
/// Signature : acb5b66e85f2350acda6b2fc71b8f2158648067b
/// ***************************************************************************
/// </summary>

unit cTextButton;

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
  FMX.Objects,
  Gamolf.RTL.UIElements;

type
  TcadTextButton = class(TFrame)
    rBackground: TRectangle;
    Text1: TText;
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FrameMouseLeave(Sender: TObject);
    procedure FrameMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure rBackgroundResized(Sender: TObject);
  private
    FText: string;
    FisDown: boolean;
    FisSelected: boolean;
    FUIItem: TUIElement;
    FBackgroundColor: TAlphacolor;
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetisDown(const Value: boolean);
    procedure SetisSelected(const Value: boolean);
    procedure SetBackgroundColor(const Value: TAlphacolor);
  protected
    procedure RefreshBackground;
  public
    property Text: string read GetText write SetText;
    property IsDown: boolean read FisDown write SetisDown;
    property IsSelected: boolean read FisSelected write SetisSelected;
    property BackgroundColor: TAlphacolor read FBackgroundColor
      write SetBackgroundColor;
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    function GetUIItem: TUIElement;
  end;

implementation

{$R *.fmx}

uses
  uSVGToImages,
  uUIItemsList,
  USVGPuzzleAssets2;

{ TcadTextButton }

procedure TcadTextButton.AfterConstruction;
begin
  inherited;
  name := '';
  IsDown := false;
  FUIItem := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      if assigned(self) and assigned(onclick) then
        onclick(self);
    end);
  FUIItem.OnPaintProc := procedure(const Sender: TObject)
    begin
      IsSelected := FUIItem.IsFocused;
    end;
end;

constructor TcadTextButton.Create(AOwner: TComponent);
begin
  inherited;
  FText := 'button';
  FisDown := false;
  FisSelected := false;
  FUIItem := nil;
  FBackgroundColor := talphacolors.Black;
end;

procedure TcadTextButton.FrameMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  IsDown := true;
end;

procedure TcadTextButton.FrameMouseLeave(Sender: TObject);
begin
  IsDown := false;
end;

procedure TcadTextButton.FrameMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  IsDown := false;
end;

function TcadTextButton.GetText: string;
begin
  result := Text1.Text;
end;

function TcadTextButton.GetUIItem: TUIElement;
begin
  result := FUIItem;
end;

procedure TcadTextButton.rBackgroundResized(Sender: TObject);
begin
  RefreshBackground;
end;

procedure TcadTextButton.RefreshBackground;
var
  SVGIdx: TSVGPuzzleAssets2Index;
begin
  if FisDown then
    SVGIdx := TSVGPuzzleAssets2Index.BtnOn
  else if FisSelected then
    SVGIdx := TSVGPuzzleAssets2Index.BtnFocus
  else
    SVGIdx := TSVGPuzzleAssets2Index.BtnOff;
  rBackground.Fill.Bitmap.Bitmap.assign(getBitmapFromSVG(SVGIdx,
    rBackground.width, rBackground.height,
    rBackground.Fill.Bitmap.Bitmap.BitmapScale));
end;

procedure TcadTextButton.SetBackgroundColor(const Value: TAlphacolor);
begin
  FBackgroundColor := Value;
  RefreshBackground;
end;

procedure TcadTextButton.SetisDown(const Value: boolean);
begin
  FisDown := Value;
  RefreshBackground;
end;

procedure TcadTextButton.SetisSelected(const Value: boolean);
begin
  FisSelected := Value;
  if FisSelected then
    FUIItem.SetFocus;
  RefreshBackground;
end;

procedure TcadTextButton.SetText(const Value: string);
begin
  Text1.Text := Value;
end;

end.
