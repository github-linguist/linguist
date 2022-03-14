**free
// Return IFS file as string

ctl-opt main(main);
ctl-opt option(*srcstmt:*nodebugio:*nounref) dftactgrp(*no);

dcl-pr main extpgm('IFSREAD');
  *n char(127);
  *n char(4096);
end-pr;

dcl-pr fopen pointer extproc('_C_IFS_fopen');
  fileName pointer value options(*string);
  fileMode pointer value options(*string);
end-pr;

dcl-pr fgets pointer extproc('_C_IFS_fgets');
  line    pointer value;
  size    int(10) value;
  fstream pointer value;
end-pr;

dcl-pr fclose int(10) extproc('_C_IFS_fclose');
  fstream pointer value;
end-pr;


dcl-proc main;
  dcl-pi *n;
    fpath     char(127);
    fcontents char(4096);
  end-pi;

  dcl-s fp char(127) inz(*blanks);

  fp = %trim(fpath) + x'00';

  monitor;
    fcontents = readFileContents(fp);
  on-error;
    dsply ('Fatal error occurred reading file.');
    return;
  endmon;

  on-exit;
    *inlr = *on;
    return;
end-proc;


// read file contents into string
dcl-proc readFileContents;
  dcl-pi *N varchar(4096);
    fpath char(128) value;
  end-pi;

  dcl-s fptr       pointer;
  dcl-s contents   varchar(4096);
  dcl-s lineBuffer char(128);
  dcl-s fmode      char(5);
    
  fmode = 'r' + x'00';
  fptr = fopen(%addr(fpath): %addr(fmode));
    
  if (fptr = *null);
    dsply ('Could not open file');
    return contents;
  endif;

  dow (fgets(%addr(lineBuffer): %size(lineBuffer): fptr) <> *null);
    lineBuffer = %xlate(x'00250D': '   ': lineBuffer); // LF,CR,NULL
    contents += %trimr(lineBuffer);
    clear lineBuffer;
  enddo;

  fclose(%addr(fpath));
  return contents; 
end-proc;