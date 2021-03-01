**free
  ctl-opt main(main);
  ctl-opt option(*srcstmt:*nodebugio:*nounref) dftactgrp(*no);

  dcl-pr main extpgm('HELLO') end-pr;

  dcl-proc main;
    dsply ('Hello world');

    *inlr = *on;
    return;
  end-proc;