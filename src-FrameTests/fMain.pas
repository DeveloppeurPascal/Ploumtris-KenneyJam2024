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
/// File last update : 2024-07-28T18:20:34.000+02:00
/// Signature : f6677ea83f6ecd19469655fc0c218ac604d16087
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
  FMX.Edit;

type
  TfrmMain = class(TForm)
    btnTextButtons: TButton;
    Timer1: TTimer;
    lblGamepadOnOff: TLabel;
    btnImageButtons: TButton;
    btnTrackbar: TButton;
    btnCheckbox: TButton;
    btnDialogBox: TButton;
    btnPloumtrisTitle: TButton;
    procedure btnTextButtonsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnImageButtonsClick(Sender: TObject);
    procedure btnDialogBoxClick(Sender: TObject);
    procedure btnCheckboxClick(Sender: TObject);
    procedure btnTrackbarClick(Sender: TObject);
    procedure btnPloumtrisTitleClick(Sender: TObject);
  private
  protected
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  fTextButtons,
  uUIItemsList,
  Gamolf.RTL.Joystick,
  fCheckBox,
  fDialogBox,
  fImageButtons,
  fTrackBar,
  fPloumtrisTitle,
  Gamolf.RTL.UIElements;

procedure TfrmMain.btnCheckboxClick(Sender: TObject);
var
  frm: TfrmCheckBox;
begin
  frm := TfrmCheckBox.Create(self);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.btnDialogBoxClick(Sender: TObject);
var
  frm: TfrmDialogBox;
begin
  frm := TfrmDialogBox.Create(self);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.btnImageButtonsClick(Sender: TObject);
var
  frm: TfrmImageButtons;
begin
  frm := TfrmImageButtons.Create(self);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.btnPloumtrisTitleClick(Sender: TObject);
var
  frm: TfrmPloumtrisTitle;
begin
  frm := TfrmPloumtrisTitle.Create(self);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.btnTextButtonsClick(Sender: TObject);
var
  frm: tfrmTextButtons;
begin
  frm := tfrmTextButtons.Create(self);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.btnTrackbarClick(Sender: TObject);
var
  frm: TfrmTrackBar;
begin
  frm := TfrmTrackBar.Create(self);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
  function AddItem(const btn: TButton): TUIElement;
  begin
    result := UIItems.AddUIItem(btn.BoundsRect,
      procedure(const Sender: TObject)
      begin
        if not assigned(Sender) then
          exit;
        if not(Sender is TUIElement) then
          exit;
        if not assigned((Sender as TUIElement).tagobject) then
          exit;
        if not((Sender as TUIElement).tagobject is tcontrol) then
          exit;

        if assigned(((Sender as TUIElement).tagobject as tcontrol).OnClick) then
          ((Sender as TUIElement).tagobject as tcontrol)
            .OnClick(((Sender as TUIElement).tagobject as tcontrol));
      end);
    result.tagobject := btn;
    result.OnPaintProc := procedure(const Sender: TObject)
      begin
        if not assigned(Sender) then
          exit;
        if not(Sender is TUIElement) then
          exit;
        if not assigned((Sender as TUIElement).tagobject) then
          exit;
        if not((Sender as TUIElement).tagobject is tcontrol) then
          exit;

        if (Sender as TUIElement).IsFocused then
          ((Sender as TUIElement).tagobject as tcontrol).SetFocus
        else
          ((Sender as TUIElement).tagobject as tcontrol).resetFocus;
      end;
  end;

var
  item: TUIElement;
begin
  lblGamepadOnOff.Visible := false;

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

  item := AddItem(btnTextButtons);
  item.SetFocus;
  item.BottomItem := AddItem(btnImageButtons);
  item := item.BottomItem;
  item.BottomItem := AddItem(btnDialogBox);
  item := item.BottomItem;
  item.BottomItem := AddItem(btnCheckbox);
  item := item.BottomItem;
  item.BottomItem := AddItem(btnTrackbar);
  item := item.BottomItem;
  item.BottomItem := AddItem(btnPloumtrisTitle);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  UIItems.RemoveLayout;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
begin
  UIItems.KeyDown(Key, KeyChar, Shift);
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  lblGamepadOnOff.Visible := TGamepadDevicesManager.Current.
    ConnectedGamepadCount > 0;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
