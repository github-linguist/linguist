**free

ctl-opt main(main);
ctl-opt option(*srcstmt:*noDebugIO:*nounref) dftActGrp(*no);

dcl-ds pgmDs PSDS qualified;
  date     char(8)  pos(191);
  user     char(10) pos(244);
  jobNum   char(10) pos(254);
  procName *proc;
end-ds;

dcl-pr main extPgm('TESTPSDS') end-pr;

dcl-proc main;
  dsply (pgmDs.date);
  dsply (pgmDs.user);
  dsply (pgmDs.jobNum);
  dsply (pgmDs.procName);

  *INLR = *ON;
  return;
end-proc;