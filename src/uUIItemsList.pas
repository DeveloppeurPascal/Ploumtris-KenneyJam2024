unit uUIItemsList;

interface

uses
  Gamolf.RTL.UIElements;

function UIItems: TUIElementsList;

implementation

var
  GUIItems: TUIElementsList;

function UIItems: TUIElementsList;
begin
  result := GUIItems;
end;

initialization

GUIItems := TUIElementsList.create;

finalization

GUIItems.free;

end.
