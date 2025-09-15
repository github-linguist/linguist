//IBMUSER JOB (),
//             IEFBR14,
//             CLASS=A,
//             MSGCLASS=X,
//             REGION=5M,
//             NOTIFY=IBMUSER
//DUMP    EXEC PGM=IEFBR14
//DUMPOUT   DD DSN=SYS1.TEST.SMF,DISP=(NEW,CATLG),
//             UNIT=3390,VOL=SER=USR001,SPACE=(CYL,(10,2),RLSE)