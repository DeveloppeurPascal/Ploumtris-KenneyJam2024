unit dmGamepad;

interface

uses
  System.SysUtils,
  System.Classes,
  Gamolf.RTL.Joystick;

type
  TDataModule1 = class(TDataModule)
    GamepadManager1: TGamepadManager;
    procedure GamepadManager1ButtonDown(const GamepadID: Integer;
      const Button: TJoystickButtons);
    procedure GamepadManager1DirectionPadChange(const GamepadID: Integer;
      const Value: TJoystickDPad);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  FMX.Controls,
  uUIItemsList,
  Gamolf.RTL.UIElements;

{$R *.dfm}

procedure TDataModule1.GamepadManager1ButtonDown(const GamepadID: Integer;
  const Button: TJoystickButtons);
var
  handled: boolean;
  item: TUIElement;
begin
  UIItems.GamepadButtonDown(Button, handled);
  if not handled then
    if Button = TJoystickButtons.a then
    begin
      item := UIItems.Focused;
      if assigned(item) and assigned(item.TagObject) and
        (item.TagObject is TCOntrol) and
        assigned((item.TagObject as TCOntrol).OnClick) then
        (item.TagObject as TCOntrol).OnClick(item.TagObject as TCOntrol);
      // TODO : d�placer le traitement de "A" dans l'�cran pour faire un traitement adapt� � son fonctionnement
    end;
end;

procedure TDataModule1.GamepadManager1DirectionPadChange(const GamepadID
  : Integer; const Value: TJoystickDPad);
begin
  UIItems.GamepadMove(Value);
end;

end.
