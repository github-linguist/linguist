**free                                                      
                                                            
ctl-opt main(main);                                         
ctl-opt option(*srcstmt:*nodebugio:*nounref) dftActGrp(*no);
                                                            
dcl-f PERSONDSPF workstn indDs(dspf) usropn;                

dcl-ds dspf qualified;                                      
  exit    ind pos(3);                                       
  refresh ind pos(5);                                       
end-ds;                                                     
                                                            
dcl-ds pgmDs PSDS qualified;                                
  procName *proc;                                           
end-ds;                                                     
                                                                 
dcl-pr main extpgm('DSPFPGM2') end-pr;                           
                                                                 
dcl-proc main;                                                   
  monitor;                                                       
    open PERSONDSPF;                                             
    dspfHandler();                                               
    close PERSONDSPF;                                            
  on-error *file;                                                
    dsply ('Error opening display file.');                       
  endmon;                                                        
                                                                 
  *INLR = *on;                                                   
  return;                                                        
end-proc;                                                        
                                                                 
                                                                 
// display file loop                                             
dcl-proc dspfHandler;                                            
                                                                  
  monitor;                                                        
    ##PGM = pgmDs.procName;                                       
    resetScreen();                                                
                                                                  
    doU (dspf.exit);                                              
      exfmt #PRSRCD1;                                             
                                                                  
      if (dspf.exit);                                             
        resetScreen();                                            
        leave;                                                    
      elseif (dspf.refresh);                                      
        resetScreen();                                            
      elseif (#1NAME <> *blanks and #1AGE <> *blanks);            
        #1MSG = 'Entered person: "' + %trim(#1NAME) + '"';        
      else;                                                       
        #1MSG = 'Please fill out all fields';                     
      endif;                                                      
    enddo;                                                        
  on-error;                                                       
    #1MSG = 'Unexpected error occurred handling display file';    
  endmon;                                                         
end-proc;                                                         
                                                                  
                                                                  
// reset fields on screen and any other reset logic                                    
dcl-proc resetScreen;     
  clear #1MSG;                                                         
  clear #1AGE;                                                         
  clear #1NAME;                                                        
end-proc;                                                              
      