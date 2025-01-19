/// <summary>
/// ***************************************************************************
///
/// Ploumtris
///
/// Copyright 2024-2025 Patrick Pr�martin under AGPL 3.0 license.
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
/// File last update : 2024-07-29T10:30:28.000+02:00
/// Signature : bf58d110c735fc6e5af89bf5115a8e0f5bfb837c
/// ***************************************************************************
/// </summary>

unit dmGamepad;

interface

uses
  System.SysUtils,
  System.Classes,
  Gamolf.RTL.Joystick,
  Gamolf.RTL.Joystick.Deprecated;

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
