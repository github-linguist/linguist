// Match and Replace part of a string using a Regular Expression
//
// Nigel Galloway - April 11th., 2012
//
program RegularExpr;

uses
  RegExpr;

const
  myString = 'I think that I am Nigel';
  myMatch = '(I am)|(you are)';
var
  r : TRegExpr;
  myResult : String;

begin
  r := TRegExpr.Create;
  r.Expression := myMatch;
  write(myString);
  if r.Exec(myString) then writeln(' contains ' + r.Match[0]);
  myResult := r.Replace(myString, 'you are', False);
  write(myResult);
  if r.Exec(myResult) then writeln(' contains ' + r.Match[0]);
end.
