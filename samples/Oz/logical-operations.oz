proc {PrintLogic A B}
   %% using not short-circuiting standard library functions
   {Show {And A B}}
   {Show {Or A B}}
   {Show {Not A}}

   %% using short-circuiting keywords
   {Show A andthen B}
   {Show A orelse B}
end
