**free

dcl-pr qcmd extpgm('QCMDEXC');
  cmd    char(3000) const;
  cmdLen packed(15:5) const;
  dbcs   char(3) const options(*nopass);
end-pr;

dcl-s cmd varchar(128);

cmd = 'DSPJOB OUTPUT(*PRINT)';
qcmd(cmd: %len(cmd));

*INLR=*ON;
return;