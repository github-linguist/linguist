//IEBCOPY JOB (ACCT),'IEBCOPY',CLASS=A,MSGCLASS=X
//*
//*-----------------------------------------------------------*
//* COPY A MEMBER FROM ONE DATA SET TO ANOTHER.
//*-----------------------------------------------------------*
//IEBCOPY  EXEC PGM=IEBCOPY
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DISP=SHR,DSN=IBMUSER.TEST
//SYSUT2   DD  DISP=SHR,DSN=IBMUSER.TEST2
//SYSIN    DD  *
 C I=((SYSUT1,R)),O=SYSUT2
 S M=((TESTMEM,TEST2))
//*
