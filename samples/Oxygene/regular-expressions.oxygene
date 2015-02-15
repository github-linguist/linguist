// Match and Replace part of a string using a Regular Expression
//
// Nigel Galloway - April 15th., 2012
//
namespace re;

interface

type
  re = class
  public
    class method Main;
  end;

implementation

class method re.Main;
const
  myString = 'I think that I am Nigel';
var
  r: System.Text.RegularExpressions.Regex;
  myResult : String;
begin
  r := new System.Text.RegularExpressions.Regex('(I am)|(you are)');
  Console.WriteLine("{0} contains {1}", myString, r.Match(myString));
  myResult := r.Replace(myString, "you are");
  Console.WriteLine("{0} contains {1}", myResult, r.Match(myResult));
end;

end.
