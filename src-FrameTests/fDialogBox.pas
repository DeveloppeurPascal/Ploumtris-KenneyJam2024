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
/// File last update : 2024-07-28T15:17:04.000+02:00
/// Signature : 9647b7262bf90b831efb14fb0e5a98627fc6d55b
/// ***************************************************************************
/// </summary>

unit fDialogBox;

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
  FMX.Layouts,
  cDialogBox,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TfrmDialogBox = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    btnInfoDialogBleu: TButton;
    btnInfoDialogGris: TButton;
    btnInfoDialogOrange: TButton;
    btnInfoDialogVert: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure btnInfoDialogBleuClick(Sender: TObject);
    procedure btnInfoDialogGrisClick(Sender: TObject);
    procedure btnInfoDialogOrangeClick(Sender: TObject);
    procedure btnInfoDialogVertClick(Sender: TObject);
  private
    fDialogBox: TcadDialogBox;
    procedure SetDialogBox(const Value: TcadDialogBox);
  protected
  public
    property DialogBox: TcadDialogBox read fDialogBox write SetDialogBox;
    procedure DialogBoxButtonClick(Sender: TObject);
  end;

implementation

{$R *.fmx}

uses
  Gamolf.RTL.Joystick,
  uUIItemsList,
  Gamolf.RTL.UIElements;

procedure TfrmDialogBox.btnInfoDialogBleuClick(Sender: TObject);
begin
  DialogBox := TcadDialogBox.GetNewInstance(self, TDialogBoxType.Information,
    TDialogBoxBackgroundColor.Bleu, 'boite de dialogue d''information bleue');
  DialogBox.OnClick := DialogBoxButtonClick;
end;

procedure TfrmDialogBox.btnInfoDialogGrisClick(Sender: TObject);
begin
  DialogBox := TcadDialogBox.GetNewInstance(self, TDialogBoxType.Information,
    TDialogBoxBackgroundColor.Gris, 'boite de dialogue d''information grise');
  DialogBox.OnClick := DialogBoxButtonClick;
end;

procedure TfrmDialogBox.btnInfoDialogOrangeClick(Sender: TObject);
begin
  DialogBox := TcadDialogBox.GetNewInstance(self, TDialogBoxType.Information,
    TDialogBoxBackgroundColor.Orange,
    'boite de dialogue d''information orange');
  DialogBox.OnClick := DialogBoxButtonClick;
end;

procedure TfrmDialogBox.btnInfoDialogVertClick(Sender: TObject);
begin
  DialogBox := TcadDialogBox.GetNewInstance(self, TDialogBoxType.Information,
    TDialogBoxBackgroundColor.Vert, 'boite de dialogue d''information verte');
  DialogBox.OnClick := DialogBoxButtonClick;
end;

procedure TfrmDialogBox.DialogBoxButtonClick(Sender: TObject);
begin
  tthread.forcequeue(nil,
    procedure
    begin
      if assigned(fDialogBox) then
        freeandnil(fDialogBox);
    end);
end;

procedure TfrmDialogBox.FormCreate(Sender: TObject);
var
  item: TUIElement;
begin
  UIItems.NewLayout;
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      tthread.forcequeue(nil,
        procedure
        begin
          Close;
        end);
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.b];

  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      if assigned(btnInfoDialogBleu) and assigned(btnInfoDialogBleu.OnClick)
      then
        btnInfoDialogBleu.OnClick(Sender);
    end);
  item.OnPaintProc := procedure(const Sender: TObject)
    begin
      if (Sender is TUIElement) then
        if (Sender as TUIElement).IsFocused then
          btnInfoDialogBleu.SetFocus
        else
          btnInfoDialogBleu.ResetFocus;
    end;
  item.TagObject := btnInfoDialogBleu;
  item.SetFocus;

  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      if assigned(btnInfoDialogGris) and assigned(btnInfoDialogGris.OnClick)
      then
        btnInfoDialogGris.OnClick(Sender);
    end);
  item.OnPaintProc := procedure(const Sender: TObject)
    begin
      if (Sender is TUIElement) then
        if (Sender as TUIElement).IsFocused then
          btnInfoDialogGris.SetFocus
        else
          btnInfoDialogGris.ResetFocus;
    end;
  item.TagObject := btnInfoDialogGris;

  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      if assigned(btnInfoDialogOrange) and assigned(btnInfoDialogOrange.OnClick)
      then
        btnInfoDialogOrange.OnClick(Sender);
    end);
  item.OnPaintProc := procedure(const Sender: TObject)
    begin
      if (Sender is TUIElement) then
        if (Sender as TUIElement).IsFocused then
          btnInfoDialogOrange.SetFocus
        else
          btnInfoDialogOrange.ResetFocus;
    end;
  item.TagObject := btnInfoDialogOrange;

  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      if assigned(btnInfoDialogVert) and assigned(btnInfoDialogVert.OnClick)
      then
        btnInfoDialogVert.OnClick(Sender);
    end);
  item.OnPaintProc := procedure(const Sender: TObject)
    begin
      if (Sender is TUIElement) then
        if (Sender as TUIElement).IsFocused then
          btnInfoDialogVert.SetFocus
        else
          btnInfoDialogVert.ResetFocus;
    end;
  item.TagObject := btnInfoDialogVert;

  UIItems.GetElementByTagObject(btnInfoDialogBleu).BottomItem :=
    UIItems.GetElementByTagObject(btnInfoDialogOrange);
  UIItems.GetElementByTagObject(btnInfoDialogBleu).RightItem :=
    UIItems.GetElementByTagObject(btnInfoDialogGris);
  UIItems.GetElementByTagObject(btnInfoDialogGris).BottomItem :=
    UIItems.GetElementByTagObject(btnInfoDialogVert);
  UIItems.GetElementByTagObject(btnInfoDialogOrange).RightItem :=
    UIItems.GetElementByTagObject(btnInfoDialogVert);

  fDialogBox := nil;
end;

procedure TfrmDialogBox.FormDestroy(Sender: TObject);
begin
  UIItems.RemoveLayout;
end;

procedure TfrmDialogBox.FormKeyDown(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
begin
  UIItems.KeyDown(Key, KeyChar, Shift);
end;

procedure TfrmDialogBox.SetDialogBox(const Value: TcadDialogBox);
begin
  if assigned(fDialogBox) then
    fDialogBox.free;

  fDialogBox := Value;
end;

end.
