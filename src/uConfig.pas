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
/// File last update : 2025-01-19T16:46:30.000+01:00
/// Signature : e45d976876532df10efc16e954c908b43a60e496
/// ***************************************************************************
/// </summary>

unit uConfig;

interface

uses
  Olf.RTL.Params;

type
  TConfig = class
  private
    FParams: TParamsFile;
    function GetBackgroundMusic: boolean;
    function GetBackgroundMusicVolume: integer;
    procedure SetBackgroundMusic(const Value: boolean);
    procedure SetBackgroundMusicVolume(const Value: integer);
    function GetSoundEffects: boolean;
    function GetSoundEffectsVolume: integer;
    procedure SetSoundEffects(const Value: boolean);
    procedure SetSoundEffectsVolume(const Value: integer);
    function GetInterfaceTactileOnOff: boolean;
    procedure SetInterfaceTactileOnOff(const Value: boolean);
  protected
  public
    property BackgroundMusic: boolean read GetBackgroundMusic
      write SetBackgroundMusic;
    property BackgroundMusicVolume: integer read GetBackgroundMusicVolume
      write SetBackgroundMusicVolume;
    property SoundEffects: boolean read GetSoundEffects write SetSoundEffects;
    property SoundEffectsVolume: integer read GetSoundEffectsVolume
      write SetSoundEffectsVolume;
    property InterfaceTactileOnOff: boolean read GetInterfaceTactileOnOff
      write SetInterfaceTactileOnOff;
    class function Current: TConfig;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.Types,
  System.IOUtils,
  FMX.Platform,
  Olf.RTL.CryptDecrypt;

var
  ConfigInstance: TConfig;

  { TConfig }

constructor TConfig.Create;
begin
  FParams := TParamsFile.Create;
  FParams.InitDefaultFileNameV2('Gamolf', 'Ploumtris', false);
{$IF Defined(RELEASE)}
  FParams.onCryptProc := function(Const AParams: string): TStream
    var
      Keys: TByteDynArray;
      ParStream: TStringStream;
    begin
      ParStream := TStringStream.Create(AParams);
      try
{$I '..\_PRIVATE\src\paramsxorkey.inc'}
        result := TOlfCryptDecrypt.XORCrypt(ParStream, Keys);
      finally
        ParStream.free;
      end;
    end;
  FParams.onDecryptProc := function(Const AStream: TStream): string
    var
      Keys: TByteDynArray;
      Stream: TStream;
      StringStream: TStringStream;
    begin
{$I '..\_PRIVATE\src\paramsxorkey.inc'}
      result := '';
      Stream := TOlfCryptDecrypt.XORdeCrypt(AStream, Keys);
      try
        if assigned(Stream) and (Stream.Size > 0) then
        begin
          StringStream := TStringStream.Create;
          try
            Stream.Position := 0;
            StringStream.CopyFrom(Stream);
            result := StringStream.DataString;
          finally
            StringStream.free;
          end;
        end;
      finally
        Stream.free;
      end;
    end;
{$ENDIF}
  FParams.Load;
end;

class function TConfig.Current: TConfig;
begin
  if not assigned(ConfigInstance) then
  begin
    ConfigInstance := TConfig.Create;
  end;
  result := ConfigInstance;
end;

destructor TConfig.Destroy;
begin
  FParams.free;
  inherited;
end;

function TConfig.GetBackgroundMusic: boolean;
begin
  result := FParams.getValue('MusicOnOff', true);
end;

function TConfig.GetBackgroundMusicVolume: integer;
begin
  result := FParams.getValue('MusicVol', 100);
end;

function TConfig.GetInterfaceTactileOnOff: boolean;
var
  DefaultValue: boolean;
  svc: IFMXDeviceService;
begin
  if (TPlatformServices.Current.SupportsPlatformService(IFMXDeviceService, svc))
  then
    DefaultValue := (TDeviceFeature.HasTouchScreen in svc.GetFeatures)
  else
{$IF Defined(iOS) or Defined(ANDROID)}
    DefaultValue := true;
{$ELSE}
    DefaultValue := false;
{$ENDIF}
  result := tParams.getValue('TouchOnOff', DefaultValue);
end;

function TConfig.GetSoundEffects: boolean;
begin
  result := FParams.getValue('SoundsOnOff', true);
end;

function TConfig.GetSoundEffectsVolume: integer;
begin
  result := FParams.getValue('SoundsVol', 100);
end;

procedure TConfig.SetBackgroundMusic(const Value: boolean);
begin
  FParams.setValue('MusicOnOff', Value);
  FParams.Save;
end;

procedure TConfig.SetBackgroundMusicVolume(const Value: integer);
begin
  FParams.setValue('MusicVol', Value);
  FParams.Save;
end;

procedure TConfig.SetInterfaceTactileOnOff(const Value: boolean);
begin
  FParams.setValue('TouchOnOff', Value);
  FParams.Save;
end;

procedure TConfig.SetSoundEffects(const Value: boolean);
begin
  FParams.setValue('SoundsOnOff', Value);
  FParams.Save;
end;

procedure TConfig.SetSoundEffectsVolume(const Value: integer);
begin
  FParams.setValue('SoundsVol', Value);
  FParams.Save;
end;

initialization

ConfigInstance := nil;

finalization

ConfigInstance.free;

end.
