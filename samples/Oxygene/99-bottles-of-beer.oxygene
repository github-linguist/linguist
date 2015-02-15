namespace ConsoleApplication2;

interface

type
  ConsoleApp = class
  public
    class method Main(args: array of String);
  end;

implementation

method bottles(number: Integer): String;
begin
    if (number = 1) then
        Result := "bottle"
    else
        Result := "bottles";
end;

class method ConsoleApp.Main(args: array of String);
begin
  for n: Integer := 99 downto 1 do
  begin
      Console.WriteLine("{0} {1} of beer on the wall,",n,bottles(n));
      Console.WriteLine("{0} {1} of beer,",n,bottles(n));
      Console.WriteLine("Take one down, and pass it around,");
      Console.WriteLine("{0} {1} of beer on the wall.",n-1,bottles(n-1));
      Console.WriteLine();
  end;
  Console.ReadKey();
end;

end.
