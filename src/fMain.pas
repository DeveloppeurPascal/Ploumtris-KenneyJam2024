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
  uDMLogo, FMX.Layouts;

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
    procedure OlfAboutDialog1URLClick(const AURL: string);
    procedure FormCreate(Sender: TObject);
  private
    FCurrentScreen: TGameScreens;
    FCurrentLayout: TLayout;
    procedure SetCurrentScreen(const Value: TGameScreens);
  protected
    procedure InitAboutDialogBox;
    procedure InitMainFormCaption;
    procedure InitHomeScreen;
    procedure InitGameScreen;
    procedure InitHallOfFamesScreen;
    procedure InitOptionsScreen;
    procedure InitCreditsScreen;
  public
    property CurrentScreen: TGameScreens read FCurrentScreen
      write SetCurrentScreen;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  u_urlOpen;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
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
begin
  // TODO : à compléter
end;

procedure TfrmMain.InitGameScreen;
begin
  // TODO : à compléter
end;

procedure TfrmMain.InitHallOfFamesScreen;
begin
  // TODO : à compléter
end;

procedure TfrmMain.InitHomeScreen;
begin
  // TODO : à compléter
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
begin
  // TODO : à compléter
end;

procedure TfrmMain.OlfAboutDialog1URLClick(const AURL: string);
begin
  url_Open_In_Browser(AURL);
end;

procedure TfrmMain.SetCurrentScreen(const Value: TGameScreens);
begin
  lBackground.Visible := true;

  if assigned(FCurrentLayout) then
    FCurrentLayout.Visible := false;
  // TODO : ajouter une animation de masquage ?

  FCurrentScreen := Value;

  case Value of
    TGameScreens.Home:
      begin
        FCurrentLayout := lHome;
        InitHomeScreen;
      end;
    TGameScreens.Game:
      begin
        FCurrentLayout := lGame;
        InitGameScreen;
      end;
    TGameScreens.HallOfFames:
      begin
        FCurrentLayout := lHallOfFames;
        InitHallOfFamesScreen;
      end;
    TGameScreens.Options:
      begin
        FCurrentLayout := lOptions;
        InitOptionsScreen;
      end;
    TGameScreens.Credits:
      begin
        FCurrentLayout := lCredits;
        InitCreditsScreen;
      end
  else
    FCurrentLayout := nil;
  end;
  if assigned(FCurrentLayout) then
  begin
    // TODO : ajouter une animation d'affichage ?
    FCurrentLayout.Visible := true;
    FCurrentLayout.BringToFront;
  end;
end;

end.
