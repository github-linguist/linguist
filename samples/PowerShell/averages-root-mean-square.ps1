function get-rms([float[]]$nums){
   $sqsum=$nums | foreach-object { $_*$_} | measure-object -sum | select-object -expand Sum
   return [math]::sqrt($sqsum/$nums.count)
}

get-rms @(1..10)
