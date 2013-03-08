namespace Loops;

interface

uses System.Linq;

type
  ConsoleApp = class
  public
    class method Main;
    method loopsTesting;
    method fillData : sequence of Country;
  var 
    Countries : sequence of Country;
  end;

type 
  Country = public class
  public
    property Name : String;
    property Capital : String;

    constructor (setName : String; setCapital : String);
  end;
implementation

class method ConsoleApp.Main;
begin
  Console.WriteLine('Loops example');
  Console.WriteLine();

  with myConsoleApp := new ConsoleApp() do
     myConsoleApp.loopsTesting;
end;

method ConsoleApp.loopsTesting;
begin
  {---------------------------------}
  {"for" loop, taking every 5th item}
  for i : Int32 :=0 to 50 step 5 do 
  begin
    Console.Write(i); Console.Write(' ');
  end;

  Console.WriteLine(); Console.WriteLine();

  {---------------------------------}
  {"for" loop, going from high to low value}
  for i : Int32 := 10 downto 1 do
  begin
    Console.Write(i); Console.Write(' ');
  end;

  Console.WriteLine(); Console.WriteLine();

  Countries := fillData;
  
  {---------------------------------}
  {loop with defined "index" variable, which will count from 0 through the number of elements looped}
  Console.WriteLine('Countries: ');
  for each c in Countries index num do 
    Console.WriteLine(Convert.ToString(num + 1) + ') ' + c.Name);

  Console.WriteLine();

  Console.WriteLine('Cities: ');
  var ind : Integer :=0;

  {---------------------------------}
  {simple "loop" construct that loops endlessly, until broken out of}
  loop
  begin
    Console.WriteLine(Countries.ElementAt(ind).Capital);
    Inc(ind);
    if ind = Countries.Count then break;
  end;

  Console.WriteLine();

  {---------------------------------}
  {the type of 'c' is inferred automatically}
  for each c in Countries do
    Console.WriteLine(c.Capital + ' is the capital of ' + c.Name);

  Console.WriteLine();

  ind := 0;
  Console.WriteLine('Cities: ');

  {"repeat ... until" loop}
  repeat 
    Console.WriteLine(Countries.ElementAt(ind).Capital);
    Inc(ind);
  until ind = Countries.Count;

  Console.WriteLine();

  ind := 0;
  Console.WriteLine('Countries: ');

  {---------------------------------}
  {"while ... do" loop}
  while ind < Countries.Count do
  begin
    Console.WriteLine(Countries.ElementAt(ind).Name);
    Inc(ind);
  end;

  Console.ReadLine();
end;

method ConsoleApp.fillData: sequence of Country;
begin
  result := [new Country('UK', 'London'), new Country('USA', 'Washington'), new Country('Germany', 'Berlin'), 
             new Country('Ukraine', 'Kyiv'), new Country('Russia', 'Moscow'), new Country('France', 'Paris')];
             
end;

constructor Country (setName :String; setCapital: String);
begin
  Name := setName;
  Capital := setCapital;
end;

end.