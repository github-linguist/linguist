// Sum 2 integers read fron standard input
//
// Nigel Galloway - April 16th., 2012
//
namespace aplusb;

interface
  uses System.Text.RegularExpressions.*;

type
  aplusb = class
  public
    class method Main;
  end;

implementation

class method aplusb.Main;
var
  gc: GroupCollection;
  m : Match;
begin
  m := new Regex('^\s*(?<a>-?[1-9]\d{0,2}|0|-?1000)\s+(?<b>-?[1-9]\d{0,2}|0|-?1000)\s*$').Match(Console.ReadLine());
  if m.Success then
    begin
      gc := m.Groups;
      Console.WriteLine("{0} + {1} = {2}", gc['a'].Value, gc['b'].Value, Integer.Parse(gc['a'].Value) + Integer.Parse(gc['b'].Value));
    end
  else Console.WriteLine("Invalid Input");
end;

end.
