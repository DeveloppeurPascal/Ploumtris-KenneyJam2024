unit fTextButtons;

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
  FMX.Layouts;

type
  TfrmTextButtons = class(TForm)
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
  private
  protected
    procedure ButtonClick(Sender: TObject);
  public
  end;

implementation

{$R *.fmx}

uses
  Gamolf.RTL.Joystick,
  uUIElements,
  uUIItemsList,
  cTextButton;

procedure TfrmTextButtons.ButtonClick(Sender: TObject);
begin
  if assigned(Sender) and (Sender is TcadTextButton) then
    showmessage((Sender as TcadTextButton).Text);
end;

procedure TfrmTextButtons.FormCreate(Sender: TObject);
var
  item: tuiitem;
  i: integer;
  btn: TcadTextButton;
begin
  UIItems.NewLayout;
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      Close;
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.b];

  Layout1.Width := 104;
  Layout1.height := 0;
  for i := 1 to 5 do
  begin
    if (Layout1.height > 0) then
      Layout1.height := Layout1.height + 5;
    btn := TcadTextButton.Create(self);
    btn.parent := Layout1;
    btn.Position.x := 0;
    btn.Width := Layout1.Width;
    btn.height := 54;
    btn.Position.y := Layout1.height;
    btn.Text := 'Button ' + i.tostring;
    btn.onClick := ButtonClick;
    Layout1.height := btn.Position.y + btn.height;
    if i = 1 then
    begin
      btn.isSelected := true;
      item := btn.GetUIItem;
    end
    else
    begin
      item.BottomItem := btn.GetUIItem;
      item := btn.GetUIItem;
    end;
    item.TagObject := btn;
  end;
end;

procedure TfrmTextButtons.FormDestroy(Sender: TObject);
begin
  UIItems.RemoveLayout;
end;

procedure TfrmTextButtons.FormKeyDown(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
begin
  UIItems.KeyDown(Key, KeyChar, Shift);
end;

end.
