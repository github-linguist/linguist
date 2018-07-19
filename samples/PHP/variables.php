<?php
/**
 * @author Elad Yosifon
 */


/*
 * PHP is a weak typed language,
 * no separation between variable declaration, initialization and assignment.
 *
 * variable type is defined by the value that is assigned to it.
 * a variable name must start with a "$" sign (called "sigil", not a dollar sign).
 * variable naming rules:
 *     + case-sensitive.
 *     + first character after $ must not be a number.
 *     + after the first character all alphanumeric chars and  _(underscore) sign is allowed, e.g. $i_am_a_new_var_with_the_number_0
 *
 */

# NULL typed variable
$null = NULL;				var_dump($null);	// output: null

# defining a boolean
$boolean = true;			var_dump($boolean);	// output: boolean true
$boolean = false;			var_dump($boolean);	// output: boolean false

/*
 * casting is made using (TYPE) as a prefix
 */
# bool and boolean is the same
$boolean = (bool)1;			var_dump($boolean);	// output: boolean true
$boolean = (boolean)1;			var_dump($boolean);	// output: boolean true

$boolean = (bool)0;			var_dump($boolean);	// output: boolean false
$boolean = (boolean)0;			var_dump($boolean);	// output: boolean false

# defining an integer
$int = 0;				var_dump($int);		// output: int 0

# defining a float,
$float = 0.01;				var_dump($float);	// output: float 0.01

# which is also identical to "real" and "double"
var_dump((double)$float);					// output: float 0.01
var_dump((real)$float);						// output: float 0.01

# casting back to int (auto flooring the value)
var_dump((int)$float);						// output: int 0
var_dump((int)($float+1));					// output: int 1
var_dump((int)($float+1.9));					// output: int 1

# defining a string
$string = 'string';
var_dump($string);						// output: string 'string' (length=6)

# referencing a variable (there are no pointers in PHP).
$another_string = &$string;
var_dump($another_string);
								// output: string 'string' (length=6)

$string = "I'm the same string!";
var_dump($another_string);
								// output: string 'I'm the same string!' (length=20)
# "deleting" a variable from memory
unset($another_string);

$string = 'string';
/*
 * a string can also be defined with double-quotes, HEREDOC and NOWDOC operators.
 * content inside double-quotes is being parsed before assignment.
 * concatenation operator is .=
 */
$parsed_string = "This is a $string";
var_dump($parsed_string);
								// output: string 'This is a string' (length=16)
$parsed_string .= " with another {$string}";
var_dump($parsed_string);
								// output: string 'This is a string with another string' (length=36)

# with string parsing
$heredoc = <<<HEREDOC
This is the content of \$string: {$string}
HEREDOC;
var_dump($heredoc);
								// output: string 'This is the content of $string: string' (length=38)

# without string parsing (notice the single quotes surrounding NOWDOC)
$nowdoc = <<<'NOWDOC'
This is the content of \$string: {$string}
NOWDOC;
var_dump($nowdoc);
								// output: string 'This is the content of \$string: {$string}' (length=42)

# as of PHP5, defining an object typed stdClass => standard class
$stdObject = new stdClass();	var_dump($stdObject);
								// output: object(stdClass)[1]
# defining an object typed Foo
class Foo {}
$foo = new Foo();		var_dump($foo);
								// output: object(Foo)[2]
# defining an empty array
$array = array();		var_dump($array);
								// output: array {empty}

/*
 * an array with non-integer key is also considered as an associative array(i.e. hash table)
 * can contain mixed variable types, can contain integer based keys and non-integer keys
 */
$assoc = array(
	0 => $int,
	'integer' => $int,
	1 => $float,
	'float' => $float,
	2 => $string,
	'string' => $string,
	3 => NULL, // <=== key 3 is NULL
	3, // <=== this is a value, not a key (key is 4)
	5 => $stdObject,
	'Foo' => $foo,
);
var_dump($assoc);

// output:
// =======
//	array
//	    0 => int 0
//	    'integer' => int 0
//	    1 => float 0.01
//	    'float' => float 0.01
//	    2 => string 'string' (length=6)
//	    'string' => string 'string' (length=6)
//	    3 => null
//	    4 => int 3
//	    5 =>
//	    	object(stdClass)[1]
//	    'Foo' =>
//		    object(Foo)[2]



/*
 * all variables are "global" but not reachable inside functions(unless specifically "globalized" inside)
 */

function a_function()
{
	# not reachable
	var_dump(isset($foo));				// output: boolean false

	global $foo;
	# "global" (reachable) inside a_function()'s scope
	var_dump(isset($foo));				// output: boolean true
}

a_function();

/**
 * there is another special type of variable called (Resource).
 * for more info regarding Resources:
 * @url http://php.net/manual/en/language.types.resource.php
 * @url http://php.net/manual/en/resource.php
 */
