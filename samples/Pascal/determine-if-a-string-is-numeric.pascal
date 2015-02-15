function IsNumeric(Value: string; const AllowFloat: Boolean): Boolean;
var
  ValueInt: Integer;
  ValueFloat: Extended;
  ErrCode: Integer;
begin
// Check for integer: Val only accepts integers when passed integer param
Value := SysUtils.Trim(Value);
Val(Value, ValueInt, ErrCode);
Result := ErrCode = 0;      // Val sets error code 0 if OK
if not Result and AllowFloat then
    begin
    // Check for float: Val accepts floats when passed float param
    Val(Value, ValueFloat, ErrCode);
    Result := ErrCode = 0;    // Val sets error code 0 if OK
    end;
end;
