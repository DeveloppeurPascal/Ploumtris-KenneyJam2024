unit cGameControllerStatus;

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
  Gamolf.RTL.Joystick,
  FMX.Ani,
  FMX.Layouts;

type
  TcadGameControllerStatus = class(TFrame)
    Rectangle1: TRectangle;
    FloatAnimation1: TFloatAnimation;
    GamepadManager1: TGamepadManager;
    Layout1: TLayout;
    procedure GamepadManager1NewGamepadDetected(const GamepadID: Integer);
    procedure FloatAnimation1Finish(Sender: TObject);
    procedure GamepadManager1GamepadLost(const GamepadID: Integer);
  private
  public
    procedure AfterConstruction; override;
  end;

implementation

{$R *.fmx}

uses
  USVGInputPrompts,
  uSVGToImages;

procedure TcadGameControllerStatus.AfterConstruction;
begin
  inherited;
  Rectangle1.Visible := false;
  FloatAnimation1.Enabled := false;
  Rectangle1.Opacity := 1;
end;

procedure TcadGameControllerStatus.FloatAnimation1Finish(Sender: TObject);
begin
  Rectangle1.Visible := false;
  Rectangle1.Opacity := 1;
end;

procedure TcadGameControllerStatus.GamepadManager1GamepadLost(const GamepadID
  : Integer);
begin
  Rectangle1.fill.Bitmap.Bitmap.assign
    (getBitmapFromSVG(TSVGInputPromptsIndex.ControllerDisconnected,
    Rectangle1.width, Rectangle1.Height,
    Rectangle1.fill.Bitmap.Bitmap.BitmapScale));
  Rectangle1.Visible := Visible;
  FloatAnimation1.Start;
end;

procedure TcadGameControllerStatus.GamepadManager1NewGamepadDetected
  (const GamepadID: Integer);
begin
  Rectangle1.fill.Bitmap.Bitmap.assign
    (getBitmapFromSVG(TSVGInputPromptsIndex.ControllerGeneric, Rectangle1.width,
    Rectangle1.Height, Rectangle1.fill.Bitmap.Bitmap.BitmapScale));
  Rectangle1.Visible := Visible;
  FloatAnimation1.Start;
end;

end.
