**free
// Basic BF Interpreter in fully free RPGLE

ctl-opt main(main);
ctl-opt option(*srcstmt:*nodebugio:*nounref) dftactgrp(*no);

dcl-c MEMSIZE  30000;
dcl-c PGMSIZE  4096;
dcl-c BUFFSIZE 128;

dcl-s memory  int(3) dim(MEMSIZE) inz(*zeros);
dcl-s result  varchar(BUFFSIZE)   inz(*blanks);
dcl-s pgm     varchar(PGMSIZE);
dcl-s insPtr  int(5)  inz(1);
dcl-s memPtr  int(5)  inz(1);
dcl-s outPtr  int(5)  inz(1);


dcl-pr main extpgm('BFINT');
  *n char(PGMSIZE);
  *n char(BUFFSIZE);
end-pr;


dcl-proc main;
  dcl-pi *n;
    bfpgm     char(PGMSIZE);
    outBuffer char(BUFFSIZE);
  end-pi;

  dcl-s ins char(1) inz('');

  monitor;
    memory(*) = 1;
    pgm = sanitize(bfpgm);
    
    for insPtr = 1 to %len(pgm);
      ins = getInstruction();

      select;
        when (ins = '>');
          moveRight();
        when (ins = '<');
          moveLeft();
        when (ins = '+');
          increment();
        when (ins = '-');
          decrement();
        when (ins = '.');
          output();
        when (ins = ',');
          input();
        when (ins = '[');
          jumpFwd();
        when (ins = ']');
          jumpBack();
        other;
          // ignore bad instructions
      endsl;
    endfor;

  on-error;
    dsply ('Fatal error occurred interpreting BF source.');
    return;
  endmon;

  on-exit;
    outBuffer = result;
    *inlr = *on;
    return;
end-proc;


// strip valid characters out of source
dcl-proc sanitize;
  dcl-pi *n char(PGMSIZE);
    dirty char(PGMSIZE);
  end-pi;

  dcl-c valid '[]><+-.,';
  dcl-s buffer char(PGMSIZE) inz(*blanks);
  dcl-s i int(5) inz(0);

  for i = 1 to %len(dirty);
    if %scan(%subst(dirty:i:1):valid) <> 0;
      %subst(buffer:i:1) = %subst(dirty:i:1);
    endif;
  endfor;

  return %trim(buffer);
end-proc;


// get current instruction at instruction pointer
dcl-proc getInstruction;
  dcl-pi *n char(1) end-pi;

  return %subst(pgm:insPtr:1);
end-proc;


// increment memory pointer to next cell on the right
dcl-proc moveRight;
  if memPtr = MEMSIZE;
    memPtr = 1;
  else;
    memPtr += 1;
  endif;
end-proc;


// decrement data pointer to next cell on the left
dcl-proc moveLeft;
  if memPtr = 1;
    memPtr = MEMSIZE;
  else;
    memPtr -= 1;
  endif;
end-proc;


// increment byte at memory pointer
dcl-proc increment;
  memory(memPtr) += 1;
end-proc;


// decrement byte at memory pointer
dcl-proc decrement;
  memory(memPtr) -= 1;
end-proc;


// add byte at memory pointer to output buffer
dcl-proc output;
  dcl-s memCell int(3)  inz(*zeros);
  dcl-s outCell char(1) inz(*blanks);

  // check ASCII decimal =>  %char(%editc(memory(memPtr):'X'));
  memCell = memory(memPtr);

  exec SQL
    set :outCell = chr(:memCell - 1);

  %subst(result:outPtr:1) = outCell;
  outPtr += 1;
end-proc;


// input byte and store at memory pointer
dcl-proc input;
  dcl-s c char(16) inz(*blanks);

  dsply 'INPUT: ' '' c;
  memory(memPtr) = %int(%subst(c:1:1));
end-proc;


// jump instruction pointer forward to next instruction after matching ']' instruction
dcl-proc jumpFwd;
  dcl-s depth int(5) inz(0);
  dcl-s ins char(1) inz('');

  if memory(memPtr) = 1;
    insPtr += 1;
    ins = getInstruction();

    dow depth > 0 or ins <> ']';
      if ins = '[';
        depth += 1;
      elseif ins = ']';
        depth -= 1;
      endif;

      insPtr += 1;
      ins = getInstruction();
    enddo;
  endif;
end-proc;


// jump instruction pointer back to instruction after matching '[' instruction
dcl-proc jumpBack;
  dcl-s depth int(5) inz(0);
  dcl-s ins char(1) inz('');

  if memory(memPtr) <> 1;
    insPtr -= 1;
    ins = getInstruction();

    dow depth > 0 or ins <> '[';
      if ins = ']';
        depth += 1;
      elseif ins = '[';
        depth -= 1;
      endif;
      
      insPtr -= 1;
      ins = getInstruction();
    enddo;
    
    insPtr -= 1;
  endif;
end-proc;