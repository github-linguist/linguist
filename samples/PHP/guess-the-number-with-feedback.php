<?php

session_start();

if(isset($_SESSION['number']))
{
   $number = $_SESSION['number'];
}
else
{
   $_SESSION['number'] = rand(1,10);
}


if($_POST["guess"]){
    $guess  = htmlspecialchars($_POST['guess']);

	echo $guess . "<br />";
    if ($guess < $number)
	{
        echo "Your guess is too low";
    }
	elseif($guess > $number)
	{
        echo "Your guess is too high";
    }
	elseif($guess == $number)
	{
        echo "You got the correct number!";
    }

}
?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
		<title>Guess A Number</title>
	</head>

	<body>
		<form action="<?=$_SERVER['PHP_SELF'] ?>" method="post" name="guess-a-number">
		    <label for="guess">Guess A Number:</label><br/ >
		    <input type="text" name="guess" />
		    <input name="number" type="hidden" value="<?= $number ?>" />
		    <input name="submit" type="submit" />
		</form>
	</body>
</html>
