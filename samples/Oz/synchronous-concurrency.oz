declare
  %% Helper function to read a file lazily.
  %% Returns a lazy list of lines.
  fun {ReadLines FN}
     F = {New class $ from Open.file Open.text end init(name:FN)}
     fun lazy {ReadNext}
        case {F getS($)} of
           false then nil
        [] Line then
           Line|{ReadNext}
        end
     end
  in
     %% close file when handle becomes unreachable
     {Finalize.register F proc {$ F} {F close} end}
     {ReadNext}
  end

  Count %% Will receive the number of lines
  PrinterPort
in
  %% Printer thread
  thread
     Stream
     Counter = {NewCell 0} %% mutable variable
  in
     PrinterPort = {NewPort ?Stream}
     for Line in Stream do
        case Line of eof then
           Count = @Counter
        else
           {System.showInfo Line}
           Counter := @Counter + 1
        end
     end
  end

  %% Send all lines to printer thread; make sure that eof is sent.
  try
     for Line in {ReadLines "input.txt"} do
        {Send PrinterPort Line}
     end
  finally
     {Send PrinterPort eof}
  end

  %% Sync on Count and print its value.
  {Wait Count}
  {Show Count}
