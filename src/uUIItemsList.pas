unit uUIItemsList;

interface

uses
  uUIElements;

function UIItems: TUIItemsList;

implementation

var
  GUIItems: TUIItemsList;

function UIItems: TUIItemsList;
begin
  result := GUIItems;
end;

initialization

GUIItems := TUIItemsList.create;

finalization

GUIItems.free;

end.
