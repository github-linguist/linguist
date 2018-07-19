<?php
function brainfuck_interpret(&$s, &$_s, &$d, &$_d, &$i, &$_i, &$o) {
   do {
     switch($s[$_s]) {
       case '+': $d[$_d] = chr(ord($d[$_d]) + 1); break;
       case '-': $d[$_d] = chr(ord($d[$_d]) - 1); break;
       case '>': $_d++; if(!isset($d[$_d])) $d[$_d] = chr(0); break;
       case '<': $_d--; break;
       case '.': $o .= $d[$_d]; break;
       case ',': $d[$_d] = $_i==strlen($i) ? chr(0) : $i[$_i++]; break;
       case '[':
         if((int)ord($d[$_d]) == 0) {
           $brackets = 1;
           while($brackets && $_s++ < strlen($s)) {
             if($s[$_s] == '[')
               $brackets++;
             else if($s[$_s] == ']')
               $brackets--;
           }
         }
         else {
             $pos = $_s++-1;
           if(brainfuck_interpret($s, $_s, $d, $_d, $i, $_i, $o))
             $_s = $pos;
         }
         break;
       case ']': return ((int)ord($d[$_d]) != 0);
    }
  } while(++$_s < strlen($s));
}

function brainfuck($source, $input='') {
  $data         = array();
  $data[0]      = chr(0);
  $data_index   = 0;
  $source_index = 0;
  $input_index  = 0;
  $output       = '';

  brainfuck_interpret($source, $source_index,
                      $data,   $data_index,
                      $input,  $input_index,
                      $output);
  return $output;
}

$code = "
    >++++++++[<+++++++++>-]<.>>+>+>++>[-]+<[>[->+<<++++>]<<]>.+++++++..+++.>
    >+++++++.<<<[[-]<[-]>]<+++++++++++++++.>>.+++.------.--------.>>+.>++++.
";
$inp = '123';
print brainfuck( $code, $inp );
