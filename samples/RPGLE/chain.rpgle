**free                          
                                
dcl-f inventory keyed;          
                                
dcl-s id like(ITEMID);          
                                
id = 1;                         
chain (id) INVRECFMT;           
                                
if not %found;                  
  dsply ('Record not found');   
else;                           
  dsply ('Did not find record');
endif;                          
                                
*inlr = *on;                    
return;