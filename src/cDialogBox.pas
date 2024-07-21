unit cDialogBox;

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
  FMX.Objects;

type
{$SCOPEDENUMS ON}
  TDialogBoxBackgroundColor = (Bleu, Gris, Orange, Vert);
  TDialogBoxType = (Information, Confirmation, Error);

  TcadDialogBox = class(TFrame)
    rBackground: TRectangle;
    vsContent: TVertScrollBox;
    lButtons: TLayout;
    Text1: TText;
    procedure rBackgroundResized(Sender: TObject);
  private
    FBackgroundColor: TDialogBoxBackgroundColor;
    FDialogType: TDialogBoxType;
    procedure SetBackgroundColor(const Value: TDialogBoxBackgroundColor);
    procedure SetDialogType(const Value: TDialogBoxType);
    procedure SetText(const Value: string);
    function GetText: string;
  protected
    procedure RefreshBackground;
    procedure RefreshButtons;
    procedure ButtonBackClick(Sender: TObject);
  public
    property BackgroundColor: TDialogBoxBackgroundColor read FBackgroundColor
      write SetBackgroundColor;
    property DialogType: TDialogBoxType read FDialogType write SetDialogType;
    property Text: string read GetText write SetText;
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function GetNewInstance(const AParent: TFMXObject;
      const ADialogType: TDialogBoxType;
      const ABackgroundColor: TDialogBoxBackgroundColor; const AText: string)
      : TcadDialogBox;
    // TODO : ajouter des créations d'instance avec une gestion du retour sous forme de ModalResult (en event et proc) plutôt que onClick
  end;

implementation

{$R *.fmx}

uses
  uSVGToImages,
  PuzzleAssets2,
  cTextButton,
  Gamolf.RTL.Joystick,
  uUIItemsList;

{ TcadDialogBox }

procedure TcadDialogBox.AfterConstruction;
begin
  inherited;
  name := '';
  Text := '';
  align := talignlayout.center;
  width := 300;
  height := 300;
  // TODO : format carré obligatoire car SVG en proportionnel dans la zone d'affichage
  RefreshBackground;
  RefreshButtons;
end;

procedure TcadDialogBox.ButtonBackClick(Sender: TObject);
begin
  if assigned(onclick) then
    onclick(Sender);
end;

constructor TcadDialogBox.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundColor := TDialogBoxBackgroundColor.Gris;
  FDialogType := TDialogBoxType.Information;
  UIItems.NewLayout;
end;

destructor TcadDialogBox.Destroy;
begin
  UIItems.RemoveLayout;
  inherited;
end;

class function TcadDialogBox.GetNewInstance(const AParent: TFMXObject;
  const ADialogType: TDialogBoxType;
  const ABackgroundColor: TDialogBoxBackgroundColor; const AText: string)
  : TcadDialogBox;
begin
  result := TcadDialogBox.Create(AParent);
  result.parent := AParent;
  result.DialogType := ADialogType;
  result.BackgroundColor := ABackgroundColor;
  result.Text := AText;
end;

function TcadDialogBox.GetText: string;
begin
  result := Text1.Text;
end;

procedure TcadDialogBox.rBackgroundResized(Sender: TObject);
begin
  RefreshBackground;
  RefreshButtons;
end;

procedure TcadDialogBox.RefreshBackground;
var
  bmp: tbitmap;
begin
  case FBackgroundColor of
    TDialogBoxBackgroundColor.Bleu:
      bmp := getBitmapFromSVG(TSVGSVGIndex.CadreBleu, rBackground.width,
        rBackground.height, rBackground.fill.Bitmap.Bitmap.BitmapScale, 0);
    TDialogBoxBackgroundColor.Gris:
      bmp := getBitmapFromSVG(TSVGSVGIndex.CadreGris, rBackground.width,
        rBackground.height, rBackground.fill.Bitmap.Bitmap.BitmapScale, 0);
    TDialogBoxBackgroundColor.Orange:
      bmp := getBitmapFromSVG(TSVGSVGIndex.CadreOrange, rBackground.width,
        rBackground.height, rBackground.fill.Bitmap.Bitmap.BitmapScale, 0);
    TDialogBoxBackgroundColor.Vert:
      bmp := getBitmapFromSVG(TSVGSVGIndex.CadreVert, rBackground.width,
        rBackground.height, rBackground.fill.Bitmap.Bitmap.BitmapScale, 0);
  else
    raise exception.Create('Unknow background for this dialog box.');
  end;
  try
    rBackground.fill.Bitmap.Bitmap.Assign(bmp);
  finally
    bmp.free;
  end;

  rBackground.padding.left := width * 30 / 276;
  rBackground.padding.top := height * 38 / 276;
  rBackground.padding.right := padding.left;
  rBackground.padding.bottom := height * 30 / 276;
end;

procedure TcadDialogBox.RefreshButtons;
  function AddButton(const Text: string; const LeftButton: TcadTextButton)
    : TcadTextButton;
  begin
    result := TcadTextButton.Create(self);

    result.parent := lButtons;
    if assigned(LeftButton) then
    begin
      result.Position.x := LeftButton.Position.x + LeftButton.width + 5;
      result.GetUIItem.LeftItem := LeftButton.GetUIItem;
    end
    else
      result.Position.x := 0;
    result.Position.y := 0;

    result.width := 104;
    result.height := 54;
    // TODO : taille W/H à calculer au prorata de la hauteur de la zone parente

    result.Text := Text;
  end;

var
  btn: TcadTextButton;
begin
  UIItems.RemoveLayout;
  while lButtons.ChildrenCount > 0 do
    lButtons.children[0].free;
  UIItems.NewLayout;

  btn := nil;
  case FDialogType of
    TDialogBoxType.Information, TDialogBoxType.Error:
      begin
        btn := AddButton('Back', btn); // TODO : traduire textes
        btn.GetUIItem.KeyShortcuts.Add(vkEscape, #0, []);
        btn.GetUIItem.KeyShortcuts.Add(vkHardwareBack, #0, []);
        btn.GetUIItem.KeyShortcuts.Add(vkReturn, #0, []);
        btn.GetUIItem.KeyShortcuts.Add(0, ' ', []);
        btn.GetUIItem.GamePadButtons :=
          [TJoystickButtons.a, TJoystickButtons.b];
        btn.onclick := ButtonBackClick;
        btn.GetUIItem.SetFocus;

        // btn.Position.x := (lButtons.width - btn.width) / 2;
        btn.align := talignlayout.center;
        // TODO : corriger le centrage du bouton dans la zone correspondante, il apparaît décalé vers la droite
      end;
  else
    raise exception.Create('Confirmation dialog box not implemented !');
    // TODO : traiter le cas des boites en confirmation (yes/no)
  end;
end;

procedure TcadDialogBox.SetBackgroundColor(const Value
  : TDialogBoxBackgroundColor);
begin
  FBackgroundColor := Value;
  RefreshBackground;
end;

procedure TcadDialogBox.SetDialogType(const Value: TDialogBoxType);
begin
  FDialogType := Value;
  RefreshButtons;
end;

procedure TcadDialogBox.SetText(const Value: string);
begin
  Text1.Text := Value;
  Text1.Visible := not Value.IsEmpty;
end;

end.
