program fifo(input, output);

type
 pNode = ^tNode;
 tNode = record
          value: integer;
          next:  pNode;
         end;

 tFifo = record
          first, last: pNode;
         end;

procedure initFifo(var fifo: tFifo);
 begin
  fifo.first := nil;
  fifo.last := nil
 end;

procedure pushFifo(var fifo: tFifo; value: integer);
 var
  node: pNode;
 begin
  new(node);
  node^.value := value;
  node^.next := nil;
  if fifo.first = nil
   then
    fifo.first := node
   else
    fifo.last^.next := node;
  fifo.last := node
 end;

function popFifo(var fifo: tFifo; var value: integer): boolean;
 var
  node: pNode;
 begin
  if fifo.first = nil
   then
    popFifo := false
   else
    begin
     node := fifo.first;
     fifo.first := fifo.first^.next;
     value := node^.value;
     dispose(node);
     popFifo := true
    end
 end;

procedure testFifo;
 var
  fifo: tFifo;
 procedure testpop(expectEmpty: boolean; expectedValue: integer);
  var
   i: integer;
  begin
   if popFifo(fifo, i)
    then
     if expectEmpty
      then
       writeln('Error! Expected empty, got ', i, '.')
      else
       if i = expectedValue
        then
         writeln('Ok, got ', i, '.')
        else
         writeln('Error! Expected ', expectedValue, ', got ', i, '.')
    else
     if expectEmpty
       then
        writeln('Ok, fifo is empty.')
       else
        writeln('Error! Expected ', expectedValue, ', found fifo empty.')
  end;
 begin
  initFifo(fifo);
  pushFifo(fifo, 2);
  pushFifo(fifo, 3);
  pushFifo(fifo, 5);
  testpop(false, 2);
  pushFifo(fifo, 7);
  testpop(false, 3);
  testpop(false, 5);
  pushFifo(fifo, 11);
  testpop(false, 7);
  testpop(false, 11);
  pushFifo(fifo, 13);
  testpop(false, 13);
  testpop(true, 0);
  pushFifo(fifo, 17);
  testpop(false, 17);
  testpop(true, 0)
 end;

begin
 writeln('Testing fifo implementation ...');
 testFifo;
 writeln('Testing finished.')
end.
