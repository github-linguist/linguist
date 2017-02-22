#!/usr/bin/instantfpc

{$if not defined(fpc) or (fpc_fullversion < 20600)}
  {$error FPC 2.6.0 or greater required}
{$endif}

{$mode objfpc}{$H+}

uses
  Classes,SysUtils,gvector,ghashmap;

type
  TStrHashCaseInsensitive = class
    class function hash(s: String; n: Integer): Integer;
  end;

class function TStrHashCaseInsensitive.hash(s: String; n: Integer): Integer;
var
  x: Integer;
  c: Char;
begin
  x := 0;
  for c in UpCase(s) do Inc(x,Ord(c));
  Result := x mod n;
end;

type
  TConfigValues  = specialize TVector<String>;
  TConfigStorage = class(specialize THashMap<String,TConfigValues,TStrHashCaseInsensitive>)
    destructor Destroy; override;
  end;

destructor TConfigStorage.Destroy;
var
  It: TIterator;
begin
  if Size > 0 then begin
    It := Iterator;
    repeat
      It.Value.Free;
    until not It.Next;
    It.Free;
  end;
  inherited Destroy;
end;

var
  ConfigStrings,ConfigValues: TStrings;
  ConfigStorage: TConfigStorage;
  ConfigLine,ConfigName,ConfigValue: String;
  SeparatorPos: Integer;
begin
  ConfigStrings := TStringList.Create;
  ConfigValues  := TStringList.Create;
  ConfigValues.Delimiter := ',';
  ConfigValues.StrictDelimiter := true;
  ConfigStorage := TConfigStorage.Create;

  ConfigStrings.LoadFromFile('config.test');
  for ConfigLine in ConfigStrings do begin
    if Length(ConfigLine) > 0 then begin
      case ConfigLine[1] of
        '#',';': ; // ignore
        else begin
          // look for = first
          SeparatorPos := Pos('=',ConfigLine);
          // if not found, then look for space
          if SeparatorPos = 0 then begin
            SeparatorPos := Pos(' ',ConfigLine);
          end;
          // found space
          if SeparatorPos <> 0 then begin
            ConfigName := UpCase(Copy(ConfigLine,1,SeparatorPos - 1));
            ConfigValues.DelimitedText := Copy(ConfigLine,SeparatorPos + 1,Length(ConfigLine) - SeparatorPos);
          // no = or space found, take the whole line as a key name
          end else begin
            ConfigName := UpCase(Trim(ConfigLine));
          end;
          if not ConfigStorage.Contains(ConfigName) then begin
            ConfigStorage[ConfigName] := TConfigValues.Create;
          end;
          for ConfigValue in ConfigValues do begin
            ConfigStorage[ConfigName].PushBack(Trim(ConfigValue));
          end;
        end;
      end;
    end;
  end;

  WriteLn('FULLNAME = ' + ConfigStorage['FULLNAME'][0]);
  WriteLn('FAVOURITEFRUIT = ' + ConfigStorage['FAVOURITEFRUIT'][0]);
  WriteLn('NEEDSPEELING = ' + BoolToStr(ConfigStorage.Contains('NEEDSPEELING'),true));
  WriteLn('SEEDSREMOVED = ' + BoolToStr(ConfigStorage.Contains('SEEDSREMOVED'),true));
  WriteLn('OTHERFAMILY(1) = ' + ConfigStorage['OTHERFAMILY'][0]);
  WriteLn('OTHERFAMILY(2) = ' + ConfigStorage['OTHERFAMILY'][1]);

  ConfigStorage.Free;
  ConfigValues.Free;
  ConfigStrings.Free;
end.
