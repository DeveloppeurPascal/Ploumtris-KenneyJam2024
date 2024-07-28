unit USVGInputPrompts;

// ****************************************
// * SVG from folder :
// * C:\Users\patrickpremartin\Documents\Embarcadero\Studio\Projets\Ploumtris\assets\kenney.nl\InputPrompts\uSVGInputPrompts.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit (1.0)
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 28/07/2024 17:52:21
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGControllerDisconnected = 0;
  CSVGControllerGeneric = 1;
  CSVGKeyboardA = 2;
  CSVGKeyboardArrowDown = 3;
  CSVGKeyboardArrowLeft = 4;
  CSVGKeyboardArrowRight = 5;
  CSVGKeyboardArrowUp = 6;
  CSVGKeyboardD = 7;
  CSVGKeyboardEscape = 8;
  CSVGKeyboardReturn = 9;
  CSVGKeyboardS = 10;
  CSVGKeyboardSpace = 11;
  CSVGKeyboardW = 12;
  CSVGXboxButtonColorA = 13;
  CSVGXboxButtonColorB = 14;
  CSVGXboxButtonColorX = 15;
  CSVGXboxDpadRoundDown = 16;
  CSVGXboxDpadRoundHorizontal = 17;
  CSVGXboxDpadRoundVertical = 18;

type
{$SCOPEDENUMS ON}
  TSVGInputPromptsIndex = (
    ControllerDisconnected = CSVGControllerDisconnected,
    ControllerGeneric = CSVGControllerGeneric,
    KeyboardA = CSVGKeyboardA,
    KeyboardArrowDown = CSVGKeyboardArrowDown,
    KeyboardArrowLeft = CSVGKeyboardArrowLeft,
    KeyboardArrowRight = CSVGKeyboardArrowRight,
    KeyboardArrowUp = CSVGKeyboardArrowUp,
    KeyboardD = CSVGKeyboardD,
    KeyboardEscape = CSVGKeyboardEscape,
    KeyboardReturn = CSVGKeyboardReturn,
    KeyboardS = CSVGKeyboardS,
    KeyboardSpace = CSVGKeyboardSpace,
    KeyboardW = CSVGKeyboardW,
    XboxButtonColorA = CSVGXboxButtonColorA,
    XboxButtonColorB = CSVGXboxButtonColorB,
    XboxButtonColorX = CSVGXboxButtonColorX,
    XboxDpadRoundDown = CSVGXboxDpadRoundDown,
    XboxDpadRoundHorizontal = CSVGXboxDpadRoundHorizontal,
    XboxDpadRoundVertical = CSVGXboxDpadRoundVertical);

  TSVGInputPrompts = class
  private
  class var
    FTag: integer;
    FTagBool: Boolean;
    FTagFloat: Single;
    FTagObject: TObject;
    FTagString: string;
    class procedure SetTag(const Value: integer); static;
    class procedure SetTagBool(const Value: Boolean); static;
    class procedure SetTagFloat(const Value: Single); static;
    class procedure SetTagObject(const Value: TObject); static;
    class procedure SetTagString(const Value: string); static;
  public const
    ControllerDisconnected = CSVGControllerDisconnected;
    ControllerGeneric = CSVGControllerGeneric;
    KeyboardA = CSVGKeyboardA;
    KeyboardArrowDown = CSVGKeyboardArrowDown;
    KeyboardArrowLeft = CSVGKeyboardArrowLeft;
    KeyboardArrowRight = CSVGKeyboardArrowRight;
    KeyboardArrowUp = CSVGKeyboardArrowUp;
    KeyboardD = CSVGKeyboardD;
    KeyboardEscape = CSVGKeyboardEscape;
    KeyboardReturn = CSVGKeyboardReturn;
    KeyboardS = CSVGKeyboardS;
    KeyboardSpace = CSVGKeyboardSpace;
    KeyboardW = CSVGKeyboardW;
    XboxButtonColorA = CSVGXboxButtonColorA;
    XboxButtonColorB = CSVGXboxButtonColorB;
    XboxButtonColorX = CSVGXboxButtonColorX;
    XboxDpadRoundDown = CSVGXboxDpadRoundDown;
    XboxDpadRoundHorizontal = CSVGXboxDpadRoundHorizontal;
    XboxDpadRoundVertical = CSVGXboxDpadRoundVertical;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGInputPromptsIndex) : string; overload;
    class function Count : Integer;
    class constructor Create;
  end;

var
  SVGInputPrompts : array of String;

implementation

uses
  System.SysUtils;

{ TSVGInputPrompts }

class constructor TSVGInputPrompts.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGInputPrompts.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGInputPrompts.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGInputPrompts.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGInputPrompts.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGInputPrompts.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGInputPrompts.SVG(const Index: Integer): string;
begin
  if (index < Count) then
    result := SVGInputPrompts[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGInputPrompts.SVG(const Index : TSVGInputPromptsIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGInputPrompts.Count: Integer;
begin
  result := length(SVGInputPrompts);
end;

initialization

SetLength(SVGInputPrompts, 19);

{$TEXTBLOCK NATIVE XML}
SVGInputPrompts[CSVGControllerDisconnected] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
<path stroke="none" fill="#FCFCFC" d="M34 38 L23 38 15.35 45.65 Q14.7 46.4 13.75 46.75 L11.85 46.95 Q10.15 46.85 9 45.45 7.85 44.05 8 42.4 L11.15 22.7 Q11.5 20.6 12.7 18.8 13.85 17 15.65 15.8 16.95 14.9 18.5 14.45 20.05 13.95 21.65 14 L42.75 14 Q45.95 14.05 48.7 16 51.4 18 52.4 21 L53.1 24.25 54.75 34.55 Q53.95 34 53 34 L49 34 Q47.75 34 46.9 34.9 L45 36.75 43.15 34.9 Q42.25 34 41 34 L37 34 Q35.75 34 34.9 34.9 34 35.75 34 37 L34 38 M55.9 41.85 Q56.1 42.75 55.9 43.65 55.65 44.75 54.95 45.5 L54.35 46.1 53.25 45 55.15 43.15 55.9 41.85 M44 31 Q44 30.15 43.4 29.55 42.85 29 42 29 41.15 29 40.55 29.55 40 30.15 40 31 40 31.85 40.55 32.4 41.15 33 42 33 42.85 33 43.4 32.4 44 31.85 44 31 M48 27 Q48 26.15 47.4 25.55 46.85 25 46 25 45.15 25 44.55 25.55 44 26.15 44 27 44 27.85 44.55 28.4 45.15 29 46 29 46.85 29 47.4 28.4 48 27.85 48 27 M44 23 Q44 22.15 43.4 21.55 42.85 21 42 21 41.15 21 40.55 21.55 40 22.15 40 23 40 23.85 40.55 24.4 41.15 25 42 25 42.85 25 43.4 24.4 44 23.85 44 23 M40 27 Q40 26.15
39.4 25.55 38.85 25 38 25 37.15 25 36.55 25.55 36 26.15 36 27 36 27.85 36.55 28.4 37.15 29 38 29 38.85 29 39.4 28.4 40 27.85 40 27 M20 21 L20 25 16 25 16 29 20 29 20 33 24 33 24 29 28 29 28 25 24 25 24 21 20 21"/>
    <path stroke="none" fill="#FFFFFF" fill-opacity="0" d="M54.75 34.55 L55.15 34.9 Q56 35.75 56 37 L56 41 55.9 41.85 55.15 43.15 53.25 45 54.35 46.1 55.15 46.9 Q56 47.75 56 49 L56 53 Q56 54.25 55.15 55.15 54.25 56 53 56 L49 56 Q47.75 56 46.9 55.15 L45 53.25 43.15 55.15 Q42.25 56 41 56 L37 56 Q35.75 56 34.9 55.15 34 54.25 34 53 L34 49 Q34 47.75 34.9 46.9 L36.75 45 34.9 43.15 Q34 42.25 34 41 L34 38 34 37 Q34 35.75 34.9 34.9 35.75 34 37 34 L41 34 Q42.25 34 43.15 34.9 L45 36.75 46.9 34.9 Q47.75 34 49 34 L53 34 Q53.95 34 54.75 34.55 M37 41 L41 45 37 49 37 53 41 53 45 49 49 53 53 53 53 49 49 45 53 41 53 37 49 37 45 41 41 37 37 37 37 41"/>
    <path stroke="none" fill="#FFFFFF" d="M37 41 L37 37 41 37 45 41 49 37 53 37 53 41 49 45 53 49 53 53 49 53 45 49 41 53 37 53 37 49 41 45 37 41"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGControllerGeneric] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
<path stroke="none" fill="#FCFCFC" d="M15.65 15.8 Q16.95 14.9 18.5 14.45 20.05 13.95 21.65 14 L42.75 14 Q45.95 14.05 48.7 16 51.4 18 52.4 21 L53.1 24.25 55.85 41.6 Q56.1 42.65 55.9 43.65 55.65 44.75 54.95 45.5 54.1 46.55 52.7 46.9 L50.1 46.65 Q49.5 46.45 48.85 45.85 L47.75 44.75 44.35 41.35 41 38 23 38 15.35 45.65 Q14.7 46.4 13.75 46.75 L11.85 46.95 Q10.15 46.85 9 45.45 7.85 44.05 8 42.4 L11.15 22.7 Q11.5 20.6 12.7 18.8 13.85 17 15.65 15.8 M40 27 Q40 26.15 39.4 25.55 38.85 25 38 25 37.15 25 36.55 25.55 36 26.15 36 27 36 27.85 36.55 28.4 37.15 29 38 29 38.85 29 39.4 28.4 40 27.85 40 27 M20 21 L20 25 16 25 16 29 20 29 20 33 24 33 24 29 28 29 28 25 24 25 24 21 20 21 M44 31 Q44 30.15 43.4 29.55 42.85 29 42 29 41.15 29 40.55 29.55 40 30.15 40 31 40 31.85 40.55 32.4 41.15 33 42 33 42.85 33 43.4 32.4 44 31.85 44 31 M48 27 Q48 26.15 47.4 25.55 46.85 25 46 25 45.15 25 44.55 25.55 44 26.15 44 27 44 27.85 44.55 28.4 45.15 29 46 29 46.85 29 47.4 28.4 48 27.85 48 27 M44 23 Q44 22.15 43.4 21.55
42.85 21 42 21 41.15 21 40.55 21.55 40 22.15 40 23 40 23.85 40.55 24.4 41.15 25 42 25 42.85 25 43.4 24.4 44 23.85 44 23"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardA] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8 M38 42 L42 42 34 22 30 22 22 42 26 42 27.6 38 36.4 38 38 42 M32 27 L34.8 34 29.2 34 32 27"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardArrowDown] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M8 48 L8 16 Q8 8 16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 M32 42 L40 34 40 32 36 32 36 22 28 22 28 32 24 32 24 34 32 42"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardArrowLeft] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8 M22 32 L30 40 32 40 32 36 42 36 42 28 32 28 32 24 30 24 22 32"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardArrowRight] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M42 32 L34 24 32 24 32 28 22 28 22 36 32 36 32 40 34 40 42 32 M48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardArrowUp] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8 L48 8 Q56 8 56 16 M32 22 L24 30 24 32 28 32 28 42 36 42 36 32 40 32 40 30 32 22"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardD] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8 M28 26 L30.15 26 Q32.5 26.05 34.25 27.8 36 29.55 36 32 36 34.45 34.25 36.25 32.5 37.95 30.15 38 L28 38 28 26 M24 22 L24 42 30.25 42 Q34.25 41.9 37.1 39.1 L38.05 38 Q40 35.4 40 32 40 28.55 38.05 26 L37.1 24.95 Q34.25 22.1 30.25 22 L24 22"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardEscape] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8 M44.8 37 Q46.05 35.8 46 34.3 L46 32.8 43 32.8 43 34.3 42.75 34.8 42 35 41.2 34.8 41 34.3 41 28.75 41.2 28.3 41.25 28.25 Q41.55 28 42 28 42.45 28 42.8 28.3 L43 28.75 43 30.25 46 30.25 46 28.75 Q46 27.15 44.75 26.05 43.55 25 42 25 40.4 25 39.25 26.05 L39.2 26.05 Q38 27.15 38 28.75 L38 34.3 Q38 35.8 39.15 36.95 L39.2 37.05 39.3 37.1 Q40.4 38 42 38 43.55 38 44.7 37.1 L44.8 37 M18 25 L18 38 26 38 26 35 21 35 21 33 26 33 26 30 21 30 21 28 26 28 26 25 18 25 M32 28 L36 28 36 25 32 25 Q30.4 25 29.2 26.2 28 27.4 28 29 28 30.6 29.2 31.8 30.4 33 31.95 33 L32.05 33 Q32.4 33 32.7 33.3 L33 34 32.7 34.7 32 35 28 35 28 38 32 38 Q33.6 38 34.8 36.8 36 35.6 36 34 36 32.4 34.8 31.2 33.6 30 32.05 30 L31.95 30 31.3 29.7 Q31 29.4 31 29 31 28.6 31.3 28.3 31.6 28 32 28"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardReturn] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 40 8 38.95 8 36 Q8 28 16 28 L28 28 28 16 Q28 8 36 8 L38.95 8 40 8 48 8 M29 41 L35 47 37 47 37 43 44.1 43 Q45.3 42.95 46.1 42.1 46.95 41.3 47 40.1 L47 29 43 29 43 39 37 39 37 35 35 35 29 41"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardS] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M34 30 L30 30 Q29.2 30 28.6 29.4 28 28.8 28 28 28 27.2 28.55 26.65 L28.65 26.55 Q29.2 26 30 26 L34.05 26 Q34.8 26 35.35 26.55 L35.45 26.65 Q36 27.2 36 28 L39.85 26.5 39.7 26 Q39.25 24.8 38.25 23.8 L38.2 23.75 Q36.5 22.05 34.15 22 L30 22 Q27.55 22 25.8 23.75 L25.75 23.8 Q24 25.55 24 28 24 30.5 25.75 32.25 27.5 34 30 34 L34 34 Q34.8 34 35.4 34.6 36 35.2 36 36 36 36.8 35.45 37.35 L35.35 37.45 Q34.8 38 34 38 L29.95 38 Q29.2 38 28.65 37.45 L28.55 37.35 Q28 36.8 28 36 L24.2 37.55 24.35 38 Q24.75 39.2 25.75 40.2 L25.8 40.25 Q27.5 41.95 29.85 42 L34 42 Q36.45 42 38.2 40.25 L38.25 40.2 Q40 38.45 40 36 40 33.5 38.25 31.75 36.5 30 34 30 M16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardSpace] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
<path stroke="none" fill="#FFFFFF" d="M41.1 36.25 Q42 35.35 42 34.25 L41.7 33.55 Q41.4 33.25 41 33.25 40.6 33.25 40.3 33.55 40 33.85 40 34.25 L39.75 34.8 39.75 34.75 39 35 38.2 34.75 38 34.25 38 29.8 Q38 29.45 38.25 29.2 L38.2 29.3 Q38.55 29 39 29 39.45 29 39.75 29.3 L39.75 29.25 40 29.8 Q40 30.2 40.3 30.5 L41 30.8 41.7 30.5 42 29.8 Q42 28.65 41.1 27.8 L41.05 27.75 Q40.15 27 39 27 37.75 27 36.9 27.75 L36.85 27.8 Q36 28.65 36 29.8 L36 34.25 Q36 35.35 36.85 36.2 L36.9 36.3 Q37.75 37 39 37 40.15 37 41.05 36.3 L41.1 36.25 M29 29.8 L29 36 Q29 36.4 29.3 36.7 L30 37 Q30.4 37 30.7 36.7 L31 36 31 34 33 34 33 36 Q33 36.4 33.3 36.7 L34 37 Q34.4 37 34.7 36.7 L35 36 35 29.8 Q35 28.65 34.1 27.8 L34.05 27.75 Q33.15 27 32 27 30.75 27 29.9 27.75 L29.85 27.8 Q29 28.65 29 29.8 M31 29.8 Q31 29.45 31.25 29.2 L31.2 29.3 Q31.55 29 32 29 32.45 29 32.75 29.3 L32.75 29.25 33 29.8 33 32 31 32 31 29.8 M48 27 L44 27 Q43.6 27 43.3 27.3 43 27.6 43 28 L43 36 Q43 36.4 43.3 36.7 43.6 37 44 37 L48 37 Q48.4 37 48.7 36.7
L49 36 48.7 35.3 Q48.4 35 48 35 L45 35 45 33 48 33 Q48.4 33 48.7 32.7 L49 32 48.7 31.3 Q48.4 31 48 31 L45 31 45 29 48 29 Q48.4 29 48.7 28.7 L49 28 48.7 27.3 Q48.4 27 48 27 M16 18 L48 18 Q56 18 56 26 L56 38 Q56 42.9 53 44.8 51.1 46 48 46 L16 46 Q12.9 46 11 44.8 8 42.9 8 38 L8 26 Q8 18 16 18 M18 29 L20 29 20.7 28.7 21 28 20.7 27.3 Q20.4 27 20 27 L18 27 Q16.8 27 15.9 27.9 15 28.8 15 30 15 31.2 15.9 32.1 16.8 33 17.95 33 L18.05 33 Q18.4 33 18.7 33.3 L19 34 18.7 34.7 Q18.4 35 18 35 L16 35 Q15.6 35 15.3 35.3 15 35.6 15 36 15 36.4 15.3 36.7 L16 37 18 37 Q19.2 37 20.1 36.1 21 35.2 21 34 21 32.8 20.1 31.9 19.2 31 18.05 31 L17.95 31 Q17.6 31 17.3 30.7 17 30.4 17 30 17 29.6 17.3 29.3 17.6 29 18 29 M25.1 33 L26.35 32.7 26.45 32.65 27.1 32.15 27.1 32.1 Q28 31.25 28 30 28 28.75 27.1 27.85 26.25 27 25.05 27 L23 27 Q22.6 27 22.3 27.3 22 27.6 22 28 L22 36 Q22 36.4 22.3 36.7 22.6 37 23 37 L23.7 36.7 24 36 24 33 25.1 33 M25.1 31 L24 31 24 29 25.05 29 25.7 29.25 26 30 25.75 30.7 25.7 30.7 25.45 30.9 25.1
31"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGKeyboardW] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M16 8 L48 8 Q56 8 56 16 L56 48 Q56 56 48 56 L16 56 Q8 56 8 48 L8 16 Q8 8 16 8 M31 22 L28 31 25 22 21 22 21 24 27 42 29 42 32 33 35 42 37 42 43 24 43 22 39 22 36 31 33 22 31 22"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGXboxButtonColorA] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#7DB700" d="M56 32 Q56 42 48.95 48.95 42 56 32 56 22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 56 22.05 56 32 M38 42 L42 42 34 22 30 22 22 42 26 42 27.6 38 36.4 38 38 42 M32 27 L34.8 34 29.2 34 32 27"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGXboxButtonColorB] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#EF4E29" d="M56 32 Q56 42 48.95 48.95 42 56 32 56 22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 56 22.05 56 32 M37 28 Q37 28.85 36.45 29.4 L36.4 29.4 Q35.8 30 35 30 L29 30 29 26 35 26 Q35.8 26 36.35 26.55 L36.45 26.65 Q37 27.2 37 28 M41 36 Q41 33.7 39.5 32 41 30.3 41 28 41 25.55 39.25 23.8 L39.2 23.75 Q37.45 22 35 22 L25 22 25 42 35 42 Q37.45 42 39.2 40.25 L39.25 40.2 Q41 38.45 41 36 M37 36 Q37 36.8 36.45 37.35 L36.35 37.45 Q35.8 38 35 38 L29 38 29 34 35 34 Q35.8 34 36.4 34.6 L36.45 34.6 Q37 35.15 37 36"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGXboxButtonColorX] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#009FEB" d="M56 32 Q56 42 48.95 48.95 42 56 32 56 22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 56 22.05 56 32 M23 25 L29 32 23 39 23 40 25.25 42 26.4 42 32 35.5 37.65 42 38.75 42 41 40 41 39 35 32 41 25 41 24 38.75 22 37.65 22 32 28.55 26.4 22 25.25 22 23 24 23 25"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGXboxDpadRoundDown] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M56 32 Q56 42 48.95 48.95 42 56 32 56 22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 56 22.05 56 32 M44 38 L48 38 Q52 38 52 34 L52 30 Q52 26 48 26 L44 26 Q41.55 25.9 39.85 24.15 38.05 22.4 38 20 L38 16 Q38 12 34 12 L30 12 Q26 12 26 16 L26 20 Q25.95 22.4 24.15 24.15 22.45 25.9 20 26 L16 26 Q12 26 12 30 L12 34 Q12 38 16 38 L20 38 Q22.45 38.1 24.15 39.85 25.95 41.6 26 44 L26 48 Q26 52 30 52 L34 52 Q38 52 38 48 L38 44 Q38.05 41.6 39.85 39.85 41.55 38.1 44 38"/>
    <path stroke="none" fill="#E73246" d="M44 38 Q41.55 38.1 39.85 39.85 38.05 41.6 38 44 L38 48 Q38 52 34 52 L30 52 Q26 52 26 48 L26 44 Q25.95 41.6 24.15 39.85 22.45 38.1 20 38 L44 38"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGXboxDpadRoundHorizontal] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M38 20 L38 16 Q38 12 34 12 L30 12 Q26 12 26 16 L26 20 Q25.95 22.4 24.15 24.15 22.45 25.9 20 26 L16 26 Q12 26 12 30 L12 34 Q12 38 16 38 L20 38 Q22.45 38.1 24.15 39.85 25.95 41.6 26 44 L26 48 Q26 52 30 52 L34 52 Q37.9 52 38 48.25 L38 44 Q38.05 41.6 39.85 39.85 41.55 38.1 44 38 L48 38 Q52 38 52 34 L52 30 Q52 26 48 26 L44 26 Q41.55 25.9 39.85 24.15 38.05 22.4 38 20 M56 32 Q56 42 48.95 48.95 44.2 53.8 38 55.3 L32 56 Q22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 56 22.05 56 32"/>
    <path stroke="none" fill="#E73246" d="M38 20 Q38.05 22.4 39.85 24.15 41.55 25.9 44 26 L48 26 Q52 26 52 30 L52 34 Q52 38 48 38 L44 38 Q41.55 38.1 39.85 39.85 38.05 41.6 38 44 L38 20 M26 44 Q25.95 41.6 24.15 39.85 22.45 38.1 20 38 L16 38 Q12 38 12 34 L12 30 Q12 26 16 26 L20 26 Q22.45 25.9 24.15 24.15 25.95 22.4 26 20 L26 44"/>
  </g>
</svg>
''';
SVGInputPrompts[CSVGXboxDpadRoundVertical] := '''
<svg width="64" height="64" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#FFFFFF" d="M44 26 Q41.55 25.9 39.85 24.15 38.05 22.4 38 20 L38 16 Q38 12 34 12 L30 12 Q26 12 26 16 L26 20 Q25.95 22.4 24.15 24.15 22.45 25.9 20 26 L16 26 Q12 26 12 30 L12 34 Q12 38 16 38 L20 38 Q22.45 38.1 24.15 39.85 25.95 41.6 26 44 L26 48 Q26 52 30 52 L34 52 Q38 52 38 48 L38 44 Q38.05 41.6 39.85 39.85 41.55 38.1 44 38 L48 38 Q52 38 52 34 L52 30 Q52 26.15 48.25 26 L44 26 M56 32 Q56 42 48.95 48.95 42 56 32 56 22.05 56 15 48.95 8 42 8 32 8 22.05 15 15 22.05 8 32 8 42 8 48.95 15 53.75 19.8 55.3 26 56 28.85 56 32"/>
    <path stroke="none" fill="#E73246" d="M20 26 Q22.45 25.9 24.15 24.15 25.95 22.4 26 20 L26 16 Q26 12 30 12 L34 12 Q38 12 38 16 L38 20 Q38.05 22.4 39.85 24.15 41.55 25.9 44 26 L20 26 M44 38 Q41.55 38.1 39.85 39.85 38.05 41.6 38 44 L38 48 Q38 52 34 52 L30 52 Q26 52 26 48 L26 44 Q25.95 41.6 24.15 39.85 22.45 38.1 20 38 L44 38"/>
  </g>
</svg>
''';

end.
