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
  private
  protected
    procedure InitSVGToBitmap;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  fTextButtons,
  uUIItemsList,
  uUIElements,
  Gamolf.RTL.Joystick,
  uJoystickManager,
  fCheckBox,
  fDialogBox,
  fImageButtons,
  fTrackBar,
  PuzzleAssets2,
  Olf.Skia.SVGToBitmap;

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
  function AddItem(const btn: TButton): tuiitem;
  begin
    result := UIItems.AddUIItem(btn.BoundsRect,
      procedure(const Sender: TObject)
      begin
        if not assigned(Sender) then
          exit;
        if not(Sender is tuiitem) then
          exit;
        if not assigned((Sender as tuiitem).tagobject) then
          exit;
        if not((Sender as tuiitem).tagobject is tcontrol) then
          exit;

        if assigned(((Sender as tuiitem).tagobject as tcontrol).OnClick) then
          ((Sender as tuiitem).tagobject as tcontrol)
            .OnClick(((Sender as tuiitem).tagobject as tcontrol));
      end);
    result.tagobject := btn;
    result.OnPaintProc := procedure(const Sender: TObject)
      begin
        if not assigned(Sender) then
          exit;
        if not(Sender is tuiitem) then
          exit;
        if not assigned((Sender as tuiitem).tagobject) then
          exit;
        if not((Sender as tuiitem).tagobject is tcontrol) then
          exit;

        if (Sender as tuiitem).IsFocused then
          ((Sender as tuiitem).tagobject as tcontrol).SetFocus
        else
          ((Sender as tuiitem).tagobject as tcontrol).resetFocus;
      end;
  end;

var
  item: tuiitem;
begin
  InitSVGToBitmap;

  lblGamepadOnOff.Visible := false;

  UIItems.NewLayout;
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      Close;
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

procedure TfrmMain.InitSVGToBitmap;
var
  i: integer;
begin
  for i := 0 to length(SVGSVG) - 1 do
    TOlfSVGBitmapList.AddItemAt(i, SVGSVG[i]);
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  lblGamepadOnOff.Visible := TGamepadManagerClass.Current.
    ConnectedGamepadCount > 0;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
