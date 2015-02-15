program Rosetta_Set;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes;

{$R *.res}
type
  CharSet = set of char;

var
  A, B, C, S: CharSet;
  M: char;

  function SetToString(const ASet: CharSet): string;
  var
    J: char;
  begin
    Result := '';
    // Test all chars
    for J in char do
      // If the char is in set, add to result
      if J in ASet then
        Result := Result + J + ', ';
    // Clear the result
    if Result > '' then
      Delete(Result, Length(Result) - 1, 2);
  end;

  procedure PrintSet(const ASet: CharSet; const ASetName: string;
  const ATitle: string = '');
  begin
    if ATitle > '' then
      WriteLn(ATitle);
    WriteLn(ASetName, ' = [', SetToString(ASet), ']', #10);
  end;

  procedure ShowEqual(const ASetA, ASetB: CharSet; const ASetNameA, ASetNameB: string);
  begin
    WriteLn(ASetNameA, ' = [', SetToString(ASetA), ']');
    WriteLn(ASetNameB, ' = [', SetToString(ASetB), ']');
    if ASetA = ASetB then
      WriteLn(ASetNameA, ' = ', ASetNameB)
    else
      WriteLn(ASetNameA, ' <> ', ASetNameB);
  end;


begin
  // Set Creation
  A := ['A', 'B', 'C', 'D', 'E', 'F'];
  B := ['E', 'F', 'G', 'H', 'I', 'J'];
  PrintSet(A, 'A', 'Set Creation');
  PrintSet(B, 'B');

  // Test m ∈ S -- "m is an element in set S"
  M := 'A';
  if M in A then
    WriteLn('"A" is in set A');

  // A ∪ B -- union; a set of all elements either in set A or in set B.
  S := A + B;
  PrintSet(S, 'S', 'S = A U B -- union; a set of all elements either in set A or in set B.');

  // A ∩ B -- intersection; a set of all elements in both set A and set B.
  S := A * B;
  PrintSet(S, 'S',
    'S = A ∩ B -- intersection; a set of all elements in both set A and set B.');

  // A \ B -- difference; a set of all elements in set A, except those in set B.
  S := A - B;
  PrintSet(S, 'S',
    'S = A \ B -- difference; a set of all elements in set A, except those in set B.');

  // A ⊆ B -- subset; true if every element in set A is also in set B.
  Writeln('A ⊆ B -- subset; true if every element in set A is also in set B.');
  if A <= B then
    WriteLn('A in B')
  else
    Writeln('A is not in B');
  Writeln;
  //A = B -- equality; true if every element of set A is in set B and vice-versa.
  Writeln('A = B -- equality; true if every element of set A is in set B and vice-versa.');

  ShowEqual(A, B, 'A', 'B');
  S := A * B;
  C := ['E', 'F'];
  ShowEqual(S, C, 'S', 'C');

  readln;

end.
