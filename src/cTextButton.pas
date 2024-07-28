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
  PuzzleAssets2,
  uUIItemsList;

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
  SVGIdx: TSVGSVGIndex;
begin
  if FisDown then
    SVGIdx := TSVGSVGIndex.BtnOn
  else if FisSelected then
    SVGIdx := TSVGSVGIndex.BtnFocus
  else
    SVGIdx := TSVGSVGIndex.BtnOff;
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
