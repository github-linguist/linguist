#AutoIt Version: 3.2.10.0
$mystring="asdf"
$reverse_string = ""
$string_length = StringLen($mystring)

For $i = 1 to $string_length
   $last_n_chrs = StringRight($mystring, $i)
   $nth_chr = StringTrimRight($last_n_chrs, $i-1)
   $reverse_string= $reverse_string & $nth_chr
Next

MsgBox(0, "Reversed string is:", $reverse_string)
