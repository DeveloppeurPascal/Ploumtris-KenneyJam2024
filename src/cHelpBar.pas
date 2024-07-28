unit cHelpBar;

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
  FMX.Layouts,
  USVGInputPrompts;

type
  TcadHelpBar = class(TFrame)
    lHelpBar: TLayout;
    Image1: TImage;
    rBackground: TRectangle;
    lContent: TLayout;
    procedure lHelpBarResized(Sender: TObject);
  private
  protected
    function GetNewX: single;
  public
    /// <summary>
    /// Clear the help bar content and show it
    /// </summary>
    procedure OpenHelpBar;
    /// <summary>
    /// Hide the help bar
    /// </summary>
    procedure CloseHelpBar;
    /// <summary>
    /// Add an image to the Help Bar
    /// </summary>
    procedure AddItem(const SVGIndex: TSVGInputPromptsIndex); overload;
    /// <summary>
    /// Add an image and text to the Help Bar
    /// </summary>
    procedure AddItem(const SVGIndex: TSVGInputPromptsIndex;
      const Text: string); overload;
    /// <summary>
    /// Empty the Help bar content layout
    /// </summary>
    procedure Clear;
    procedure AfterConstruction; override;
  end;

implementation

{$R *.fmx}

uses
  uSVGToImages;

{ TcadHelpBar }

// TODO : changer AddItem pour voir imageKeyboard, imageGamepad, Text (facultatif)
procedure TcadHelpBar.AddItem(const SVGIndex: TSVGInputPromptsIndex;
  const Text: string);
var
  img: TImage;
  txt: ttext;
begin
  // TODO : ajouter un filtre Keyboard/Gamepad
  // TODO : afficher l'un ou l'autre selon si un gamepad est détecté
  img := TImage.Create(self);
  img.Parent := lContent;
  img.position.x := GetNewX;
  img.position.y := 0;
  img.height := lContent.height;
  img.Width := img.height;
  img.Bitmap.assign(getBitmapFromSVG(SVGIndex, img.Width, img.height,
    Image1.Bitmap.BitmapScale));
  if not Text.isempty then
  begin
    txt := ttext.Create(self);
    txt.Parent := lContent;
    txt.position.x := GetNewX;
    txt.AutoSize := true;
    txt.TextSettings.HorzAlign := TTextAlign.leading;
    txt.TextSettings.VertAlign := TTextAlign.center;
    txt.TextSettings.Font.Style := [TFontStyle.fsBold];
    txt.Text := Text;
    txt.position.y := (lContent.height - txt.height) / 2;
  end;
  lContent.Width := GetNewX;
  rBackground.Width := lContent.Width;
end;

procedure TcadHelpBar.AddItem(const SVGIndex: TSVGInputPromptsIndex);
begin
  AddItem(SVGIndex, '');
end;

procedure TcadHelpBar.AfterConstruction;
begin
  inherited;
  CloseHelpBar;
end;

procedure TcadHelpBar.Clear;
begin
  while (lContent.ChildrenCount > 0) do
    lContent.Children[0].free;
end;

procedure TcadHelpBar.CloseHelpBar;
begin
  lHelpBar.Visible := false;
end;

function TcadHelpBar.GetNewX: single;
var
  i: integer;
  Ctrl: TControl;
  Marge: single;
begin
  result := 0;
  Marge := 0;
  for i := 0 to lContent.ChildrenCount - 1 do
    if (lContent.Children[i] is TControl) then
    begin
      Ctrl := lContent.Children[i] as TControl;
      if (result <= Ctrl.position.x) then
      begin
        result := Ctrl.position.x + Ctrl.margins.left + Ctrl.Width +
          Ctrl.margins.Right;
        if Ctrl is TImage then
          Marge := 5
        else if Ctrl is ttext then
          Marge := 20
        else
          raise exception.Create
            ('Can''t calculate a margin between help bar elements.');
      end;
    end;
  if result > 0 then
    result := result + Marge;
end;

procedure TcadHelpBar.lHelpBarResized(Sender: TObject);
begin
  // TODO : repositionner les éléments dans la zone de contenu dans leur ordre d'ajout
end;

procedure TcadHelpBar.OpenHelpBar;
begin
  Clear;
  lHelpBar.Visible := true;
end;

end.
