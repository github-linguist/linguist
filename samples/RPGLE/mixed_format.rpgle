     H/TITLE Example of a mix of free and fixed format RPGLE             
     H*                                                                 
     D*                                                                 
     DNum1             S              6P 0 INZ(4)                                        
       dcl-s Num2 packed(6) inz(10);                             
     DResult           S              6P 0 INZ(*zeros)                  
     C*                                                    
     C     Num1          ADD       Num2          Result                 
     C     Result        DSPLY                                          
     C*                                                                 
       // some quick free format                                        
       result = num1 * num2;                                            
       dsply (%char(result));                                           
     C*                       
     C* Wait lets go back to fixed format to end this                                          
     C                   SETON                                        LR