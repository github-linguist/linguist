Program CaesarCipher(output);

procedure encrypt(var message: string; key: integer);
  var
    i: integer;
  begin
    for i := 1 to length(message) do
      case message[i] of
        'A'..'Z': message[i] := chr(ord('A') + (ord(message[i]) - ord('A') + key) mod 26);
        'a'..'z': message[i] := chr(ord('a') + (ord(message[i]) - ord('a') + key) mod 26);
      end;
  end;

procedure decrypt(var message: string; key: integer);
  var
    i: integer;
  begin
    for i := 1 to length(message) do
      case message[i] of
       'A'..'Z': message[i] := chr(ord('A') + (ord(message[i]) - ord('A') - key + 26) mod 26);
       'a'..'z': message[i] := chr(ord('a') + (ord(message[i]) - ord('a') - key + 26) mod 26);
      end;
  end;

var
  key: integer;
  message: string;

begin
  key := 3;
  message := 'The five boxing wizards jump quickly';
  writeln ('Original message: ', message);
  encrypt(message, key);
  writeln ('Encrypted message: ', message);
  decrypt(message, key);
  writeln ('Decrypted message: ', message);
end.
