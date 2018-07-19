function GetFactors($n){
   $factors = array(1, $n);
   for($i = 2; $i * $i <= $n; $i++){
      if($n % $i == 0){
         $factors[] = $i;
         if($i * $i != $n)
            $factors[] = $n/$i;
      }
   }
   sort($factors);
   return $factors;
}
