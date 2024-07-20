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
  uJoystickManager,
  FMX.Objects;

type
{$SCOPEDENUMS ON}
  TGameScreens = (None, Home, Game, HallOfFames, Options, Credits);

  TfrmMain = class(TForm)
    OlfAboutDialog1: TOlfAboutDialog;
    lBackground: TLayout;
    lHome: TLayout;
    lGame: TLayout;
    lOptions: TLayout;
    lCredits: TLayout;
    lHallOfFames: TLayout;
    GamepadManager1: TGamepadManager;
    lHomeButtons: TLayout;
    rBackground: TRectangle;
    lGameZone: TLayout;
    lGameZonePlay: TLayout;
    lGameZoneRight: TLayout;
    lGameZoneBottom: TLayout;
    lGameZoneLeft: TLayout;
    GameLoop: TTimer;
    procedure OlfAboutDialog1URLClick(const AURL: string);
    procedure FormCreate(Sender: TObject);
    procedure GamepadManager1ButtonDown(const GamepadID: Integer;
      const Button: TJoystickButtons);
    procedure GamepadManager1DirectionPadChange(const GamepadID: Integer;
      const Value: TJoystickDPad);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure GameLoopTimer(Sender: TObject);
  private
    FCurrentScreen: TGameScreens;
    FCurrentLayout: TLayout;
    procedure SetCurrentScreen(const Value: TGameScreens);
  protected
    procedure InitAboutDialogBox;
    procedure InitMainFormCaption;
    procedure InitSVGToBitmap;
    procedure InitHomeScreen;
    procedure CloseHomeScreen;
    procedure InitGameScreen;
    procedure CloseGameScreen;
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
    procedure SendNewPipe;
  public
    property CurrentScreen: TGameScreens read FCurrentScreen
      write SetCurrentScreen;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  u_urlOpen,
  uUIItemsList,
  uUIElements,
  cTextButton,
  Olf.Skia.SVGToBitmap,
  PuzzleAssets2,
  uConsts,
  uSVGToImages,
  uPipeParts,
  uCurrentGame;

procedure TfrmMain.ButtonCreditsClick(Sender: TObject);
begin
  CurrentScreen := TGameScreens.Credits;
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
  // TODO : à compléter
end;

procedure TfrmMain.CloseGameScreen;
begin
  // TODO : à compléter
  CurrentGame.IsRunning := false;

  // TODO : faire enregistrement de la partie en cours (si pause)

  CurrentGame.CurPipe := nil;
  while (lGameZonePlay.ChildrenCount > 0) do
    lGameZonePlay.Children[0].free;
end;

procedure TfrmMain.CloseHallOfFamesScreen;
begin
  // TODO : à compléter
end;

procedure TfrmMain.CloseHomeScreen;
begin
  while lHomeButtons.ChildrenCount > 0 do
    lHomeButtons.Children[0].free;
end;

procedure TfrmMain.CloseOptionsScreen;
begin
  // TODO : à compléter
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitSVGToBitmap;

  InitAboutDialogBox;
  InitMainFormCaption;

  FCurrentLayout := nil;
  FCurrentScreen := TGameScreens.None;
  lHome.Visible := false;
  lGame.Visible := false;
  lHallOfFames.Visible := false;
  lOptions.Visible := false;
  lCredits.Visible := false;

  tthread.ForceQueue(nil,
    procedure
    begin
      CurrentScreen := TGameScreens.Home;
    end);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
var
  item: tuiitem;
begin
  UIItems.KeyDown(Key, KeyChar, Shift);
  if ((Key = 0) and (KeyChar = ' ')) or ((Key = vkReturn) and (KeyChar = #0))
  then
  // TODO : si écran de jeu, la touche espace doit faire une rotation de l'élement en cours
  begin
    item := UIItems.Focused;
    if assigned(item) and assigned(item.TagObject) and
      (item.TagObject is TCOntrol) and
      assigned((item.TagObject as TCOntrol).OnClick) then
      (item.TagObject as TCOntrol).OnClick(item.TagObject as TCOntrol);
  end;
end;

procedure TfrmMain.GameLoopTimer(Sender: TObject);
begin
  if CurrentGame.IsRunning then
  begin
    if assigned(CurrentGame.CurPipe) then
    begin
      CurrentGame.CurPipe.Screeny := CurrentGame.CurPipe.Screeny +
        CurrentGame.CurPipe.vy;
      CurrentGame.CurPipe.vy := CurrentGame.CurPipe.vy + 0.01;
      if (CurrentGame.CurPipe.Screeny + CurrentGame.CurPipe.height >=
        lGameZonePlay.height) then
      begin
        CurrentGame.CurPipe.Screeny := lGameZonePlay.height -
          CurrentGame.CurPipe.height;
        // TODO : traiter immobilisation de l'élément, alimentation éventuelle en eau, etc
        CurrentGame.CurPipe.vy := 0;
        SendNewPipe;
      end;
    end
    else
      SendNewPipe;
  end
  else
    GameLoop.Enabled := false;
end;

procedure TfrmMain.GamepadManager1ButtonDown(const GamepadID: Integer;
const Button: TJoystickButtons);
var
  handled: boolean;
  item: tuiitem;
begin
  UIItems.GamepadButtonDown(Button, handled);
  if not handled then
    if Button = TJoystickButtons.a then
    // TODO : selon l'écran actif (pas en zone de jeu)
    begin
      item := UIItems.Focused;
      if assigned(item) and assigned(item.TagObject) and
        (item.TagObject is TCOntrol) and
        assigned((item.TagObject as TCOntrol).OnClick) then
        (item.TagObject as TCOntrol).OnClick(item.TagObject as TCOntrol);
    end;
end;

procedure TfrmMain.GamepadManager1DirectionPadChange(const GamepadID: Integer;
const Value: TJoystickDPad);
begin
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
    slinebreak + slinebreak + '*****************' + slinebreak +
    '* Publisher info' + slinebreak + slinebreak +
    'This application was developed by Patrick Prémartin.' + slinebreak +
    slinebreak +
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
var
  item: tuiitem;
begin
  // Gestion du bouton "B" et ESCape
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      CurrentScreen := TGameScreens.Home;
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.b];

  // TODO : à compléter
end;

procedure TfrmMain.InitGameScreen;
var
  item: tuiitem;
  i: Integer;
  r: TRectangle;
  bmp1, bmp2: tbitmap;
begin
  // Gestion du bouton "B" et ESCape
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      CurrentScreen := TGameScreens.Home;
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.b];

  if (lGameZoneLeft.ChildrenCount = 0) then
  begin
    // TODO : mettre un TScaleLayout pour adapter automatiquement la taille de la zone de jeu
    lGameZone.width := (1 + CNbCol + 1) * cpipesize;
    // TODO : voir si 10 colonnes sont ok
    lGameZone.height := (1 + CNbRow) * cpipesize;
    // TODO : passer éventuellement à 20 lignes

    bmp1 := getBitmapFromSVG(TSVGSVGIndex.EauHdb, cpipesize, cpipesize,
      rBackground.fill.bitmap.bitmap.BitmapScale, 0);
    try
      bmp2 := getBitmapFromSVG(TSVGSVGIndex.EauHgb, cpipesize, cpipesize,
        rBackground.fill.bitmap.bitmap.BitmapScale, 0);
      try
        lGameZoneLeft.width := cpipesize;
        lGameZoneRight.width := cpipesize;
        for i := 0 to CNbRow - 1 do
        begin
          // Left pipe
          r := TRectangle.Create(self);
          r.parent := lGameZoneLeft;
          r.width := cpipesize;
          r.height := cpipesize;
          r.Position.x := 0;
          r.Position.y := r.height * i;
          r.Stroke.Kind := TBrushKind.None;
          r.fill.Kind := TBrushKind.bitmap;
          r.fill.bitmap.WrapMode := twrapmode.TileStretch;
          r.fill.bitmap.bitmap.assign(bmp1);
          // Right pipe
          r := TRectangle.Create(self);
          r.parent := lGameZoneRight;
          r.width := cpipesize;
          r.height := cpipesize;
          r.Position.x := 0;
          r.Position.y := r.height * i;
          r.Stroke.Kind := TBrushKind.None;
          r.fill.Kind := TBrushKind.bitmap;
          r.fill.bitmap.WrapMode := twrapmode.TileStretch;
          r.fill.bitmap.bitmap.assign(bmp2);
        end;
      finally
        bmp2.free;
      end;
    finally
      bmp1.free;
    end;

    // Left pipe bottom
    r := TRectangle.Create(self);
    r.parent := lGameZoneLeft;
    r.width := cpipesize;
    r.height := cpipesize;
    r.Position.x := 0;
    r.Position.y := r.height * CNbRow;
    r.Stroke.Kind := TBrushKind.None;
    r.fill.Kind := TBrushKind.bitmap;
    r.fill.bitmap.WrapMode := twrapmode.TileStretch;
    bmp1 := getBitmapFromSVG(TSVGSVGIndex.EauHb, cpipesize, cpipesize,
      rBackground.fill.bitmap.bitmap.BitmapScale, 0);
    try
      r.fill.bitmap.bitmap.assign(bmp1);
    finally
      bmp1.free;
    end;
    // Right pipe bottom
    r := TRectangle.Create(self);
    r.parent := lGameZoneRight;
    r.width := cpipesize;
    r.height := cpipesize;
    r.Position.x := 0;
    r.Position.y := r.height * CNbRow;
    r.Stroke.Kind := TBrushKind.None;
    r.fill.Kind := TBrushKind.bitmap;
    r.fill.bitmap.WrapMode := twrapmode.TileStretch;
    bmp1 := getBitmapFromSVG(TSVGSVGIndex.EauHb, cpipesize, cpipesize,
      rBackground.fill.bitmap.bitmap.BitmapScale, 0);
    try
      r.fill.bitmap.bitmap.assign(bmp1);
    finally
      bmp1.free;
    end;

    // bottom line
    lGameZoneBottom.height := cpipesize;
    bmp1 := getBitmapFromSVG(TSVGSVGIndex.pipegd, cpipesize, cpipesize,
      rBackground.fill.bitmap.bitmap.BitmapScale, 0);
    try
      for i := 0 to CNbCol - 1 do
      begin
        r := TRectangle.Create(self);
        r.parent := lGameZoneBottom;
        r.width := cpipesize;
        r.height := cpipesize;
        r.Position.x := r.width * i;
        r.Position.y := 0;
        r.Stroke.Kind := TBrushKind.None;
        r.fill.Kind := TBrushKind.bitmap;
        r.fill.bitmap.WrapMode := twrapmode.TileStretch;
        case i of
          0:
            begin
              bmp2 := getBitmapFromSVG(TSVGSVGIndex.PipeDb, cpipesize,
                cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale, 0);
              try
                r.fill.bitmap.bitmap.assign(bmp2);
              finally
                bmp2.free;
              end;
            end;
          CNbCol - 1:
            begin
              bmp2 := getBitmapFromSVG(TSVGSVGIndex.pipegb, cpipesize,
                cpipesize, rBackground.fill.bitmap.bitmap.BitmapScale, 0);
              try
                r.fill.bitmap.bitmap.assign(bmp2);
              finally
                bmp2.free;
              end;
            end;
        else
          r.fill.bitmap.bitmap.assign(bmp1);
        end;
      end;
    finally
      bmp1.free;
    end;
  end;

  // TODO : à compléter
  CurrentGame.IsRunning := true;
  GameLoop.Enabled := true;
end;

procedure TfrmMain.InitHallOfFamesScreen;
var
  item: tuiitem;
begin
  // Gestion du bouton "B" et ESCape
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      CurrentScreen := TGameScreens.Home;
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.b];

  // TODO : à compléter
end;

procedure TfrmMain.InitHomeScreen;
  function AddButton(const AParent: TLayout; const AText: string;
  const AButtonPrec: TcadTextButton; const AOnClick: TNotifyEvent)
    : TcadTextButton;
  begin
    if (AParent.height > 0) then
      AParent.height := AParent.height + 5;
    result := TcadTextButton.Create(self);
    result.GetUIItem.TagObject := result;
    result.parent := AParent;
    result.Position.x := 0;
    result.width := AParent.width;
    result.height := 54;
    result.Position.y := AParent.height;
    result.Text := AText;
    result.OnClick := AOnClick;
    result.BackgroundColor := 0;
    AParent.height := result.Position.y + result.height;
    if assigned(AButtonPrec) then
      AButtonPrec.GetUIItem.BottomItem := result.GetUIItem;
  end;

var
  item: tuiitem;
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
  item.GamePadButtons := [TJoystickButtons.b];

  lHomeButtons.width := 104;
  lHomeButtons.height := 0;

  btn := AddButton(lHomeButtons, 'Play', nil, ButtonPlayClick);
  btn.IsSelected := true;
  btn := AddButton(lHomeButtons, 'Options', btn, ButtonOptionsClick);
  btn := AddButton(lHomeButtons, 'Hall of fames', btn, ButtonHallOfFameClick);
  btn := AddButton(lHomeButtons, 'Credits', btn, ButtonCreditsClick);
  btn := AddButton(lHomeButtons, 'Quit', btn, ButtonQuitClick);
end;

procedure TfrmMain.InitMainFormCaption;
begin
{$IFDEF DEBUG}
  caption := '[DEBUG] ';
{$ELSE}
  caption := '';
{$ENDIF}
  caption := caption + OlfAboutDialog1.Titre + ' v' +
    OlfAboutDialog1.VersionNumero;
end;

procedure TfrmMain.InitOptionsScreen;
var
  item: tuiitem;
begin
  // Gestion du bouton "B" et ESCape
  item := UIItems.AddUIItem(
    procedure(const Sender: TObject)
    begin
      CurrentScreen := TGameScreens.Home;
    end);
  item.KeyShortcuts.Add(vkescape, #0, []);
  item.KeyShortcuts.Add(vkHardwareBack, #0, []);
  item.GamePadButtons := [TJoystickButtons.b];

  // TODO : à compléter
end;

procedure TfrmMain.InitSVGToBitmap;
var
  i: Integer;
begin
  for i := 0 to length(SVGSVG) - 1 do
    TOlfSVGBitmapList.AddItemAt(i, SVGSVG[i]);
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
  Pipe.SVGIndex := TSVGSVGIndex(random(CSVGPipeHgd - CSVGPipeDb + 1) +
    CSVGPipeDb);
  Pipe.ScreenX := random(CNbCol) * Pipe.width;
  Pipe.Screeny := 0;
  Pipe.vy := 1;
  CurrentGame.CurPipe := Pipe;
end;

procedure TfrmMain.SetCurrentScreen(const Value: TGameScreens);
var
  InitProc: TProc;
begin
  lBackground.Visible := true;

  if assigned(FCurrentLayout) then
  begin
    // TODO : ajouter une animation de masquage ?
    FCurrentLayout.Visible := false;
    case Value of
      TGameScreens.Home:
        CloseHomeScreen;
      TGameScreens.Game:
        CloseGameScreen;
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
        FCurrentLayout := lHome;
        InitProc := InitHomeScreen;
      end;
    TGameScreens.Game:
      begin
        FCurrentLayout := lGame;
        InitProc := InitGameScreen;
      end;
    TGameScreens.HallOfFames:
      begin
        FCurrentLayout := lHallOfFames;
        InitProc := InitHallOfFamesScreen;
      end;
    TGameScreens.Options:
      begin
        FCurrentLayout := lOptions;
        InitProc := InitOptionsScreen;
      end;
    TGameScreens.Credits:
      begin
        FCurrentLayout := lCredits;
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
    // TODO : ajouter une animation d'affichage ?
    FCurrentLayout.Visible := true;
    FCurrentLayout.BringToFront;
  end;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
randomize;

end.
