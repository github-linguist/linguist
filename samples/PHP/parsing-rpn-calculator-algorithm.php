<?php
function rpn($postFix){
    $stack = Array();
    echo "Input\tOperation\tStack\tafter\n" ;
	$token = explode(" ", trim($postFix));
	$count = count($token);
	
    for($i = 0 ; $i<$count;$i++)
	{
        echo $token[$i] ." \t";
        $tokenNum = "";
		
        if (is_numeric($token[$i])) {
            echo  "Push";
			array_push($stack,$token[$i]);
        }
        else
        {
            echo "Operate";
            $secondOperand = end($stack);
			array_pop($stack);
            $firstOperand = end($stack);
            array_pop($stack);

            if ($token[$i] == "*")
				array_push($stack,$firstOperand * $secondOperand);
            else if ($token[$i] == "/")
                array_push($stack,$firstOperand / $secondOperand);
            else if ($token[$i] == "-")
                array_push($stack,$firstOperand - $secondOperand);
            else if ($token[$i] == "+")
                array_push($stack,$firstOperand + $secondOperand);
            else if ($token[$i] == "^")
                array_push($stack,pow($firstOperand,$secondOperand));
            else {
                die("Error");
            }
        }
		echo "\t\t" . implode(" ", $stack) .  "\n";
    }
    return end($stack);
}

echo "Compute Value: " . rpn("3 4 2 * 1 5 - 2 3 ^ ^ / + ");
?>
