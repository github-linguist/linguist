public function RREF():Matrix {
   var lead:uint, i:uint, j:uint, r:uint = 0;

   for(r = 0; r < rows; r++) {
      if(columns <= lead)
         break;
      i = r;

      while(_m[i][lead] == 0) {
         i++;

         if(rows == i) {
            i = r;
            lead++;

            if(columns == lead)
               return this;
         }
      }
      rowSwitch(i, r);
      var val:Number = _m[r][lead];

      for(j = 0; j < columns; j++)
         _m[r][j] /= val;

      for(i = 0; i < rows; i++) {
         if(i == r)
            continue;
         val = _m[i][lead];

         for(j = 0; j < columns; j++)
            _m[i][j] -= val * _m[r][j];
      }
      lead++;
   }
   return this;
}
