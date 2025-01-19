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
  Olf.FMX.AboutDialog,
  uDMLogo,
  FMX.Layouts,
  Gamolf.RTL.Joystick,
  FMX.Objects,
  cDialogBox,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  cPloumtrisTitle,
  Gamolf.RTL.GamepadDetected,
  Gamolf.FMX.HelpBar,
  FMX.Menus;

type
{$SCOPEDENUMS ON}
  TGameScreens = (None, Home, Game, GameOver, HallOfFames, Options, Credits);

  TfrmMain = class(TForm)
    OlfAboutDialog1: TOlfAboutDialog;
    lBackground: TLayout;
    lHomeScreen: TLayout;
    lGameScreen: TLayout;
    lOptionsScreen: TLayout;
    lCreditsScreen: TLayout;
    lHallOfFamesScreen: TLayout;
    GamepadManager1: TDGEGamepadManager;
    lHomeButtons: TLayout;
    rBackground: TRectangle;
    lGameZone: TLayout;
    lGameZonePlay: TLayout;
    lGameZoneRight: TLayout;
    lGameZoneBottom: TLayout;
    lGameZoneLeft: TLayout;
    GameLoop: TTimer;
    lGameOverScreen: TLayout;
    lGameTexts: TLayout;
    rGameTextsBackground: TRectangle;
    lblScore: TLabel;
    GameTitle: TcadPloumtrisTitle;
    lblVersion: TLabel;
    DGEGamepadDetected1: TDGEGamepadDetected;
    DGEFMXHelpBar1: TDGEFMXHelpBar;
    MainMenu: TMainMenu;
    mnuMacOSAppname: TMenuItem;
    mnuAbout: TMenuItem;
    procedure OlfAboutDialog1URLClick(const AURL: string);
    procedure FormCreate(Sender: TObject);
    procedure GamepadManager1ButtonDown(const GamepadID: Integer;
      const Button: TJoystickButtons);
    procedure GamepadManager1DirectionPadChange(const GamepadID: Integer;
      const Value: TJoystickDPad);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure GameLoopTimer(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
  private
    FCurrentScreen: TGameScreens;
    FCurrentLayout: TLayout;
    FDialogBox: TcadDialogBox;
    FScoreOnScreen: Integer;
    procedure SetCurrentScreen(const Value: TGameScreens);
    procedure SetScoreOnScreen(const Value: Integer);
  protected
    procedure InitAboutDialogBox;
    procedure InitHomeScreen;
    procedure CloseHomeScreen;
    procedure InitGameScreen;
    procedure CloseGameScreen;
    procedure InitGameOverScreen;
    procedure CloseGameOverScreen;
    procedure InitHallOfFamesScreen;
    procedure CloseHallOfFamesScreen;
    procedure InitOptionsScreen;
    procedure CloseOptionsScreen;
    procedure InitCreditsScreen;
    procedure CloseCreditsScreen;
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonHallOfFameClick(Sender: TObject);
    procedure ButtonCreditsClick(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
    procedure ButtonGameOverBackClick(Sender: TObject);
    procedure ButtonCreditsBackClick(Sender: TObject);
    procedure SendNewPipe;
  public
    procedure AfterConstruction; override;
    property CurrentScreen: TGameScreens read FCurrentScreen
      write SetCurrentScreen;
    property ScoreOnScreen: Integer read FScoreOnScreen write SetScoreOnScreen;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  u_urlOpen,
  uUIItemsList,
  cTextButton,
  uConsts,
  uSVGToImages,
  uPipeParts,
  uCurrentGame,
  uBackgroundMusic,
  Gamolf.FMX.Joystick,
  uConfig,
  Gamolf.RTL.UIElements,
  USVGPuzzleAssets2,
  Gamolf.RTL.Scores,
  uScores,
  USVGInputPrompts;

procedure TfrmMain.AfterConstruction;
begin
  inherited;
{$IF Defined(MACOS) and not Defined(IOS)}
{$ELSE}
  FreeAndNil(MainMenu);
{$ENDIF}
end;

procedure TfrmMain.ButtonCreditsBackClick(Sender: TObject);
begin
  CurrentScreen := TGameScreens.Home;
end;

procedure TfrmMain.ButtonCreditsClick(Sender: TObject);
begin
  CurrentScreen := TGameScreens.Credits;
end;

procedure TfrmMain.ButtonGameOverBackClick(Sender: TObject);
begin
  CurrentScreen := TGameScreens.Home;
end;

procedure TfrmMain.ButtonHallOfFameClick(Sender: TObject);
begin
  CurrentScreen := TGameScreens.HallOfFames;
end;

procedure TfrmMain.ButtonOptionsClick(Sender: TObject);
begin
  CurrentScreen := TGameScreens.Options;
end;

procedure TfrmMain.ButtonPlayClick(Sender: TObject);
begin
  CurrentScreen := TGameScreens.Game;
end;

procedure TfrmMain.ButtonQuitClick(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.CloseCreditsScreen;
begin
  DGEFMXHelpBar1.CloseHelpBar;

  tthread.forcequeue(nil,
    procedure
    begin
      FreeAndNil(FDialogBox);
    end);
end;

procedure TfrmMain.CloseGameOverScreen;
begin
  DGEFMXHelpBar1.CloseHelpBar;

  tthread.forcequeue(nil,
    procedure
    begin
      FreeAndNil(FDialogBox);
    end);
end;

procedure TfrmMain.CloseGameScreen;
begin
  DGEFMXHelpBar1.CloseHelpBar;

  CurrentGame.IsRunning := false;

  // TODO : faire enregistrement de la partie en cours (si pause)

  // TODO : faire une capture de l'écran pour l'afficher derrière l'écran GAME OVER si on a perdu

  CurrentGame.CurPipe := nil;
  while (lGameZonePlay.ChildrenCount > 0) do
    lGameZonePlay.Children[0].free;

  GameTitle.Visible := true;
end;

procedure TfrmMain.CloseHallOfFamesScreen;
begin
  DGEFMXHelpBar1.CloseHelpBar;

  // TODO : à compléter
end;

procedure TfrmMain.CloseHomeScreen;
begin
  DGEFMXHelpBar1.CloseHelpBar;

  tthread.forcequeue(nil,
    procedure
    begin
      while lHomeButtons.ChildrenCount > 0 do
        lHomeButtons.Children[0].free;
    end);
end;

procedure TfrmMain.CloseOptionsScreen;
begin
  DGEFMXHelpBar1.CloseHelpBar;

  // TODO : à compléter
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  InitAboutDialogBox;

  if assigned(MainMenu) then
    mnuAbout.Text := 'About ' + OlfAboutDialog1.Titre; // TODO : à traduire

  DGEFMXHelpBar1.IconKeyBitmapListIndex := TSVGInputPrompts.Tag;
  DGEFMXHelpBar1.IconGamepadBitmapListIndex := TSVGInputPrompts.Tag;
  DGEFMXHelpBar1.Height := 100;
  DGEFMXHelpBar1.TextSettings.Font.Size :=
    DGEFMXHelpBar1.TextSettings.Font.Size * 2;
  DGEFMXHelpBar1.TextSettings.FontColor := talphacolors.Whitesmoke;
  DGEFMXHelpBar1.BackgroundFill.Color := $FF1A1D4D;
  DGEFMXHelpBar1.HorzAlign := TDGEFMXHelpBarHorzAlign.Center;

  FDialogBox := nil;

  FCurrentLayout := nil;
  FCurrentScreen := TGameScreens.None;
  for i := 0 to ChildrenCount - 1 do
    if (Children[i] is TLayout) and string(Children[i].Name)
      .tolower.EndsWith('screen') then
      (Children[i] as TLayout).Visible := false;

  tthread.forcequeue(nil,
    procedure
    begin
      TBackgroundMusic.current.OnOff(tconfig.current.BackgroundMusic);
      CurrentScreen := TGameScreens.Home;
    end);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
var
  item: TUIElement;
begin
  if CurrentGame.IsRunning then
  begin
    if assigned(CurrentGame.CurPipe) then
    begin
      if (Key = vkLeft) and (KeyChar = #0) then
      begin
        CurrentGame.CurPipe.GoToLeft;
        Key := 0;
        KeyChar := #0;
      end
      else if (Key = vkright) and (KeyChar = #0) then
      begin
        CurrentGame.CurPipe.GoToRight;
        Key := 0;
        KeyChar := #0;
      end
      else if (Key = vkDown) and (KeyChar = #0) then
      begin
        CurrentGame.CurPipe.vy := CurrentGame.CurPipe.vy * 2;
        Key := 0;
        KeyChar := #0;
      end
      else if (Key = 0) and (KeyChar = ' ') then
      begin
        CurrentGame.CurPipe.RotateRight;
        Key := 0;
        KeyChar := #0;
      end;
    end;
  end;
  if (Key = vkF1) and (KeyChar = #0) and (Shift = []) then
  begin
    Key := 0;
    KeyChar := #0;
    mnuAboutClick(Sender);
  end
  else
    UIItems.KeyDown(Key, KeyChar, Shift);
  if ((Key = 0) and (KeyChar = ' ')) or ((Key = vkReturn) and (KeyChar = #0))
  then
  begin
    item := UIItems.Focused;
    if assigned(item) and assigned(item.TagObject) and
      (item.TagObject is TControl) and
      assigned((item.TagObject as TControl).OnClick) then
    begin
      (item.TagObject as TControl).OnClick(item.TagObject as TControl);
      Key := 0;
      KeyChar := #0;
    end
  end;
end;

procedure TfrmMain.GameLoopTimer(Sender: TObject);
begin
  if CurrentGame.IsRunning then
  begin
    if assigned(CurrentGame.CurPipe) then
    begin
      if (CurrentGame.CurPipe.GrilleY + 1 < CNBRow) and
        not assigned(CurrentGame.GetGrid(CurrentGame.CurPipe.GrilleX,
        CurrentGame.CurPipe.GrilleY + 1)) then
      begin
        CurrentGame.CurPipe.Screeny := CurrentGame.CurPipe.Screeny +
          CurrentGame.CurPipe.vy;
        CurrentGame.CurPipe.vy := CurrentGame.CurPipe.vy + 0.01;
      end
      else
      begin
        CurrentGame.CurPipe.StopFalling;
        if CurrentGame.CurPipe.GrilleY = 0 then
          CurrentScreen := TGameScreens.GameOver
        else
          SendNewPipe;
      end;
    end
    else
      SendNewPipe;

    if CurrentGame.Score > ScoreOnScreen then
      ScoreOnScreen := ScoreOnScreen + 1
    else if CurrentGame.Score < ScoreOnScreen then
      ScoreOnScreen := ScoreOnScreen - 1
  end
  else
    GameLoop.Enabled := false;
end;

procedure TfrmMain.GamepadManager1ButtonDown(const GamepadID: Integer;
const Button: TJoystickButtons);
var
  handled: boolean;
  item: TUIElement;
begin
  handled := false;
  if CurrentGame.IsRunning and (Button = TJoystickButtons.a) and
    assigned(CurrentGame.CurPipe) then
  begin
    CurrentGame.CurPipe.RotateRight;
    handled := true;
  end;
  if not handled then
    UIItems.GamepadButtonDown(Button, handled);
  if not handled then
    if Button = TJoystickButtons.a then
    begin
      item := UIItems.Focused;
      if assigned(item) and assigned(item.TagObject) and
        (item.TagObject is TControl) and
        assigned((item.TagObject as TControl).OnClick) then
      begin
        (item.TagObject as TControl).OnClick(item.TagObject as TControl);
        handled := true;
      end;
    end;
end;

procedure TfrmMain.GamepadManager1DirectionPadChange(const GamepadID: Integer;
const Value: TJoystickDPad);
begin
  if CurrentGame.IsRunning then
  begin
    if assigned(CurrentGame.CurPipe) then
    begin
      case Value of
        TJoystickDPad.Left, TJoystickDPad.TopLeft, TJoystickDPad.BottomLeft:
          CurrentGame.CurPipe.GoToLeft;
        TJoystickDPad.right, TJoystickDPad.TopRight, TJoystickDPad.BottomRight:
          CurrentGame.CurPipe.GoToRight;
        TJoystickDPad.Bottom:
          CurrentGame.CurPipe.vy := CurrentGame.CurPipe.vy * 2;
      end;
    end;
  end
  else
    UIItems.GamepadMove(Value);
end;

procedure TfrmMain.InitAboutDialogBox;
begin
  // TODO : traduire texte(s)
  OlfAboutDialog1.Licence.Text :=
    'This program is distributed as shareware. If you use it (especially for ' +
    'commercial or income-generating purposes), please remember the author and '
    + 'contribute to its development by purchasing a license.' + slinebreak +
    slinebreak +
    'This software is supplied as is, with or without bugs. No warranty is offered '
    + 'as to its operation or the data processed. Make backups!';
  OlfAboutDialog1.Description.Text :=
    'A Tetris for plumbers who love video games without water leaks.' +
    slinebreak + slinebreak +
    'This game has been created for the gamejam Kenney Jam 2024 in July 2024. The theme was "connections".'
    + slinebreak + slinebreak +
    'The development has been made by Patrick Prémartin in Delphi live on Twitch (https://www.twitch.tv/patrickpremartin).'
    + slinebreak + slinebreak +
    'The game music has been created by Loinduciel live on Twitch (https://www.twitch.tv/loinduciel).'
    + slinebreak + slinebreak + '*****************' + slinebreak +
    '* Publisher info' + slinebreak + slinebreak +
    'It is published by OLF SOFTWARE, a company registered in Paris (France) under the reference 439521725.'
    + slinebreak + slinebreak + '****************' + slinebreak +
    '* Personal data' + slinebreak + slinebreak +
    'This program is autonomous in its current version. It does not depend on the Internet and communicates nothing to the outside world.'
    + slinebreak + slinebreak + 'We have no knowledge of what you do with it.' +
    slinebreak + slinebreak +
    'No information about you is transmitted to us or to any third party.' +
    slinebreak + slinebreak +
    'We use no cookies, no tracking, no stats on your use of the application.' +
    slinebreak + slinebreak + '***************' + slinebreak + '* User support'
    + slinebreak + slinebreak +
    'If you have any questions or require additional functionality, please leave us a message on the application''s website or on its code repository.'
    + slinebreak + slinebreak + 'To find out more, visit ' +
    OlfAboutDialog1.URL;
end;

procedure TfrmMain.InitCreditsScreen;
begin
  FDialogBox := TcadDialogBox.GetNewInstance(self, TDialogBoxType.Information,
    TDialogBoxBackgroundColor.Vert, OlfAboutDialog1.Description.Text.trim +
    slinebreak + slinebreak + '**********' + slinebreak + '* License' +
    slinebreak + slinebreak + OlfAboutDialog1.Licence.Text.trim + slinebreak +
    slinebreak + 'v' + OlfAboutDialog1.VersionNumero + '-' +
    OlfAboutDialog1.VersionDate);
  FDialogBox.OnClick := ButtonCreditsBackClick;
  FDialogBox.Width := 600;
  FDialogBox.Height := 600;

  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.AddItem(CSVGKeyboardSpace, CSVGSteamButtonColorAOutline);
  DGEFMXHelpBar1.AddItem(CSVGKeyboardEscape, CSVGSteamButtonColorXOutline,
    'Select');
end;

procedure TfrmMain.InitGameOverScreen;
var
  Scores: TScoreList;
begin
  if (CurrentGame.Score > 0) then
  begin
    Scores := GetScoresList;
    Scores.Add('-', CurrentGame.Score);
  end;

  // TODO : afficher la capture de l'écran de jeu en background
  FDialogBox := TcadDialogBox.GetNewInstance(self, TDialogBoxType.Information,
    TDialogBoxBackgroundColor.Orange, 'GAME OVER' + slinebreak + slinebreak +
    'Your final score is ' + CurrentGame.Score.tostring + slinebreak +
    slinebreak);
  FDialogBox.OnClick := ButtonGameOverBackClick;
  // TODO : rendre la fenêtre un peu plus "sexy"

  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.AddItem(CSVGKeyboardSpace, CSVGSteamButtonColorAOutline);
  DGEFMXHelpBar1.AddItem(CSVGKeyboardEscape, CSVGSteamButtonColorXOutline,
    'Select');
end;

procedure TfrmMain.InitGameScreen;
var
  item: TUIElement;
  i: Integer;
  r: TRectangle;
  bmp1, bmp2: tbitmap;
begin
  GameTitle.Visible := false;

  // Gestion du bouton "X" et ESCape
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      CurrentScreen := TGameScreens.Home;
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.X];

  if (lGameZoneLeft.ChildrenCount = 0) then
  begin
    // TODO : mettre un TScaleLayout pour adapter automatiquement la taille de la zone de jeu
    lGameZone.Width := (1 + CNbCol + 1) * cpipesize;
    lGameZone.Height := (1 + CNBRow) * cpipesize;

    bmp1 := getBitmapFromSVG(TSVGPuzzleAssets2Index.EauHdb, cpipesize,
      cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale);

    bmp2 := getBitmapFromSVG(TSVGPuzzleAssets2Index.EauHgb, cpipesize,
      cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale);

    lGameZoneLeft.Width := cpipesize;
    lGameZoneRight.Width := cpipesize;
    for i := 0 to CNBRow - 1 do
    begin
      // Left pipe
      r := TRectangle.Create(self);
      r.hittest := false;
      r.parent := lGameZoneLeft;
      r.Width := cpipesize;
      r.Height := cpipesize;
      r.Position.X := 0;
      r.Position.y := r.Height * i;
      r.Stroke.Kind := TBrushKind.None;
      r.fill.Kind := TBrushKind.bitmap;
      r.fill.bitmap.WrapMode := twrapmode.TileStretch;
      r.fill.bitmap.bitmap.assign(bmp1);
      // Right pipe
      r := TRectangle.Create(self);
      r.hittest := false;
      r.parent := lGameZoneRight;
      r.Width := cpipesize;
      r.Height := cpipesize;
      r.Position.X := 0;
      r.Position.y := r.Height * i;
      r.Stroke.Kind := TBrushKind.None;
      r.fill.Kind := TBrushKind.bitmap;
      r.fill.bitmap.WrapMode := twrapmode.TileStretch;
      r.fill.bitmap.bitmap.assign(bmp2);
    end;

    // Left pipe bottom
    r := TRectangle.Create(self);
    r.hittest := false;
    r.parent := lGameZoneLeft;
    r.Width := cpipesize;
    r.Height := cpipesize;
    r.Position.X := 0;
    r.Position.y := r.Height * CNBRow;
    r.Stroke.Kind := TBrushKind.None;
    r.fill.Kind := TBrushKind.bitmap;
    r.fill.bitmap.WrapMode := twrapmode.TileStretch;
    r.fill.bitmap.bitmap.assign(getBitmapFromSVG(TSVGPuzzleAssets2Index.EauHb,
      cpipesize, cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale));

    // Right pipe bottom
    r := TRectangle.Create(self);
    r.hittest := false;
    r.parent := lGameZoneRight;
    r.Width := cpipesize;
    r.Height := cpipesize;
    r.Position.X := 0;
    r.Position.y := r.Height * CNBRow;
    r.Stroke.Kind := TBrushKind.None;
    r.fill.Kind := TBrushKind.bitmap;
    r.fill.bitmap.WrapMode := twrapmode.TileStretch;
    r.fill.bitmap.bitmap.assign(getBitmapFromSVG(TSVGPuzzleAssets2Index.EauHb,
      cpipesize, cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale));

    // bottom line
    lGameZoneBottom.Height := cpipesize;
    bmp1 := getBitmapFromSVG(TSVGPuzzleAssets2Index.pipegd, cpipesize,
      cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale);
    for i := 0 to CNbCol - 1 do
    begin
      r := TRectangle.Create(self);
      r.hittest := false;
      r.parent := lGameZoneBottom;
      r.Width := cpipesize;
      r.Height := cpipesize;
      r.Position.X := r.Width * i;
      r.Position.y := 0;
      r.Stroke.Kind := TBrushKind.None;
      r.fill.Kind := TBrushKind.bitmap;
      r.fill.bitmap.WrapMode := twrapmode.TileStretch;
      case i of
        0:
          r.fill.bitmap.bitmap.assign
            (getBitmapFromSVG(TSVGPuzzleAssets2Index.PipeDb, cpipesize,
            cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale));
        CNbCol - 1:
          r.fill.bitmap.bitmap.assign
            (getBitmapFromSVG(TSVGPuzzleAssets2Index.pipegb, cpipesize,
            cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale));
      else
        r.fill.bitmap.bitmap.assign(bmp1);
      end;
    end;
  end;

  CurrentGame.init;
  ScoreOnScreen := 0;
  CurrentGame.IsRunning := true;
  GameLoop.Enabled := true;

  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.AddItem(CSVGKeyboardArrowLeft, CSVGSteamDpadLeftOutline);
  DGEFMXHelpBar1.AddItem(CSVGKeyboardArrowRight,
    CSVGSteamDpadRightOutline, 'Move');
  DGEFMXHelpBar1.AddItem(CSVGKeyboardSpace, CSVGSteamButtonColorAOutline,
    'Rotate');
  DGEFMXHelpBar1.AddItem(CSVGKeyboardArrowDown,
    CSVGSteamDpadDownOutline, 'Fall');
  DGEFMXHelpBar1.AddItem(CSVGKeyboardEscape,
    CSVGSteamButtonColorXOutline, 'Quit');
end;

procedure TfrmMain.InitHallOfFamesScreen;
var
  item: TUIElement;
begin
  // Gestion du bouton "B" et ESCape
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      CurrentScreen := TGameScreens.Home;
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.X];

  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.AddItem(CSVGKeyboardSpace, CSVGSteamButtonColorAOutline);
  DGEFMXHelpBar1.AddItem(CSVGKeyboardEscape, CSVGSteamButtonColorXOutline,
    'Select');

  // TODO : à compléter
end;

procedure TfrmMain.InitHomeScreen;
  function AddButton(const AParent: TLayout; const AText: string;
  const AButtonPrec: TcadTextButton; const AOnClick: TNotifyEvent)
    : TcadTextButton;
  begin
    if (AParent.Height > 0) then
      AParent.Height := AParent.Height + 5;
    result := TcadTextButton.Create(self);
    result.GetUIItem.TagObject := result;
    result.parent := AParent;
    result.Position.X := 0;
    result.Width := AParent.Width;
    result.Height := 54;
    result.Position.y := AParent.Height;
    result.Text := AText;
    result.OnClick := AOnClick;
    result.BackgroundColor := 0;
    AParent.Height := result.Position.y + result.Height;
    if assigned(AButtonPrec) then
      AButtonPrec.GetUIItem.BottomItem := result.GetUIItem;
  end;

var
  item: TUIElement;
  btn: TcadTextButton;
begin
  // Gestion du bouton "B" et ESCape
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      ButtonQuitClick(nil);
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.X];

  lHomeButtons.Width := 104;
  lHomeButtons.Height := 0;

  btn := AddButton(lHomeButtons, 'Play', nil, ButtonPlayClick);
  btn.IsSelected := true;
  // TODO : btn := AddButton(lHomeButtons, 'Options', btn, ButtonOptionsClick);
  // TODO : btn := AddButton(lHomeButtons, 'Hall of fames', btn, ButtonHallOfFameClick);
  btn := AddButton(lHomeButtons, 'Credits', btn, ButtonCreditsClick);
  AddButton(lHomeButtons, 'Quit', btn, ButtonQuitClick);

  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.AddItem(CSVGKeyboardArrowUp, CSVGSteamDpadUpOutline);
  DGEFMXHelpBar1.AddItem(CSVGKeyboardArrowDown,
    CSVGSteamDpadDownOutline, 'Move');
  DGEFMXHelpBar1.AddItem(CSVGKeyboardSpace, CSVGSteamButtonColorAOutline,
    'Select');
  DGEFMXHelpBar1.AddItem(CSVGKeyboardEscape,
    CSVGSteamButtonColorXOutline, 'Quit');
end;

procedure TfrmMain.InitOptionsScreen;
var
  item: TUIElement;
begin
  // Gestion du bouton "B" et ESCape
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      CurrentScreen := TGameScreens.Home;
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.X];

  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.AddItem(CSVGKeyboardSpace, CSVGSteamButtonColorAOutline);
  DGEFMXHelpBar1.AddItem(CSVGKeyboardEscape, CSVGSteamButtonColorXOutline,
    'Select');

  // TODO : à compléter
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  OlfAboutDialog1.Execute;
end;

procedure TfrmMain.OlfAboutDialog1URLClick(const AURL: string);
begin
  url_Open_In_Browser(AURL);
end;

procedure TfrmMain.SendNewPipe;
var
  Pipe: TPipePart;
begin
  Pipe := TPipePart.Create(self);
  Pipe.parent := lGameZonePlay;
  Pipe.SVGIndex := TSVGPuzzleAssets2Index(random(CSVGPipeHgd - CSVGPipeDb + 1) +
    CSVGPipeDb);
  Pipe.ScreenX := random(CNbCol) * Pipe.Width;
  Pipe.Screeny := 0;
  Pipe.vy := CDefaultVY;
  CurrentGame.CurPipe := Pipe;
end;

procedure TfrmMain.SetCurrentScreen(const Value: TGameScreens);
var
  InitProc: TProc;
begin
  lBackground.Visible := true;

  if assigned(FCurrentLayout) then
  begin
    FCurrentLayout.Visible := false;
    case FCurrentScreen of
      TGameScreens.Home:
        CloseHomeScreen;
      TGameScreens.Game:
        CloseGameScreen;
      TGameScreens.GameOver:
        CloseGameOverScreen;
      TGameScreens.HallOfFames:
        CloseHallOfFamesScreen;
      TGameScreens.Options:
        CloseOptionsScreen;
      TGameScreens.Credits:
        CloseCreditsScreen;
    end;
    UIItems.RemoveLayout;
  end;

  FCurrentScreen := Value;

  case Value of
    TGameScreens.Home:
      begin
        FCurrentLayout := lHomeScreen;
        InitProc := InitHomeScreen;
      end;
    TGameScreens.Game:
      begin
        FCurrentLayout := lGameScreen;
        InitProc := InitGameScreen;
      end;
    TGameScreens.GameOver:
      begin
        FCurrentLayout := lGameOverScreen;
        InitProc := InitGameOverScreen;
      end;
    TGameScreens.HallOfFames:
      begin
        FCurrentLayout := lHallOfFamesScreen;
        InitProc := InitHallOfFamesScreen;
      end;
    TGameScreens.Options:
      begin
        FCurrentLayout := lOptionsScreen;
        InitProc := InitOptionsScreen;
      end;
    TGameScreens.Credits:
      begin
        FCurrentLayout := lCreditsScreen;
        InitProc := InitCreditsScreen;
      end
  else
    FCurrentLayout := nil;
    InitProc := nil;
  end;
  if assigned(FCurrentLayout) then
  begin
    UIItems.NewLayout;
    InitProc;
    FCurrentLayout.Visible := true;
    FCurrentLayout.BringToFront;
  end;
end;

procedure TfrmMain.SetScoreOnScreen(const Value: Integer);
begin
  FScoreOnScreen := Value;
  lblScore.Text := 'Score : ' + FScoreOnScreen.tostring;
  // TODO : remplacer par un affichage plus graphique
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
randomize;

end.
