object DataModule1: TDataModule1
  Height = 480
  Width = 640
  object GamepadManager1: TGamepadManager
    SynchronizedEvents = True
    OnButtonDown = GamepadManager1ButtonDown
    OnDirectionPadChange = GamepadManager1DirectionPadChange
    Left = 144
    Top = 208
  end
end
