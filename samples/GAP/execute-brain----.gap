# Here . and , print and read an integer, not a character
Brainfuck := function(prog)
  local pointer, stack, leftcells, rightcells, instr, stackptr, len,
    output, input, jump, i, j, set, get;
  input := InputTextUser();
  output := OutputTextUser();
  instr := 1;
  pointer := 0;
  leftcells := [ ];
  rightcells := [ ];
  stack := [ ];
  stackptr := 0;
  len := Length(prog);
  jump := [ ];

  get := function()
    local p;
    if pointer >= 0 then
      p := pointer + 1;
      if IsBound(rightcells[p]) then
        return rightcells[p];
      else
        return 0;
      fi;
    else
      p := -pointer;
      if IsBound(leftcells[p]) then
        return leftcells[p];
      else
        return 0;
      fi;
    fi;
  end;

  set := function(value)
    local p;
    if pointer >= 0 then
      p := pointer + 1;
      if value = 0 then
        Unbind(rightcells[p]);
      else
        rightcells[p] := value;
      fi;
    else
      p := -pointer;
      if value = 0 then
        Unbind(leftcells[p]);
      else
        leftcells[p] := value;
      fi;
    fi;
  end;

  # find jumps for faster execution
  for i in [1 .. len] do
    if prog[i] = '[' then
      stackptr := stackptr + 1;
      stack[stackptr] := i;
    elif prog[i] = ']' then
      j := stack[stackptr];
      stackptr := stackptr - 1;
      jump[i] := j;
      jump[j] := i;
    fi;
  od;

  while instr <= len do
    c := prog[instr];
    if c = '<' then
      pointer := pointer - 1;
    elif c = '>' then
      pointer := pointer + 1;
    elif c = '+' then
      set(get() + 1);
    elif c = '-' then
      set(get() - 1);
    elif c = '.' then
      WriteLine(output, String(get()));
    elif c = ',' then
      set(Int(Chomp(ReadLine(input))));
    elif c = '[' then
      if get() = 0 then
        instr := jump[instr];
      fi;
    elif c = ']' then
      if get() <> 0 then
        instr := jump[instr];
      fi;
    fi;
    instr := instr + 1;
  od;
  CloseStream(input);
  CloseStream(output);
  # for debugging purposes, return last state
  return [leftcells, rightcells, pointer];
end;

# An addition
Brainfuck("+++.<+++++.[->+<]>.");
# 3
# 5
# 8
