/*********************************************************************/
 /*       SOIS682 PARM DEFINITION                                    */
 /* MEMBER : SOIY682P                                                */
 /********************************************************************/
0DCL
 1 SOIS682_PARM    @@S,
   3 S682W_RC         FIXED BIN(31) @@( INIT(0)    ),
   3 S682PTR_REC      POINTER       @@( INIT(NULL) ),
   3 S682PTR_MSG      POINTER       @@( INIT(NULL) ),
   3 S682W_FUNCTION   CHAR(1)       @@( INIT(' ')  ),
   3 S682W_TRACE      CHAR(1)       @@( INIT(' ')  ),
   3 S682W_HISTORY    CHAR(1)       @@( INIT('N')  ),
   3 S682W_KEY        CHAR(13)      @@( INIT(' ')  ),
0  S682W_MESSAGE    CHAR(78) VARYING BASED(S682PTR_MSG)
                                    @@( INIT(' ')  );
0DCL                                              /* FUNCTION VALUES */
   1 S682W_ORDER_KEY  DEFINED S682W_KEY,
     5 S682W_PROD_YEAR  CHAR(4),
     5 S682W_CPL        CHAR(3),
     5 S682W_AUTO_SEQ   PIC'999999';
 DCL                              /* READ PLANT ORDER FUNCTION VALUES */
 ( S682C_READ_ONLINE CHAR(1) INIT('O'), 
   S682C_READ_KEY    CHAR(1) INIT('K'), 
   S682C_READ_ALL    CHAR(1) INIT('A')  
 ) STATIC;
 @@( DCL SOIS682 EXTERNAL ENTRY ) ;
