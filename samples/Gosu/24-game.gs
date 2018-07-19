uses java.lang.Double
uses java.lang.Integer
uses java.util.ArrayList
uses java.util.List
uses java.util.Scanner
uses java.util.Stack

function doEval( scanner : Scanner, allowed : List<Integer> ) : double {
    var stk = new Stack<Double>()

    while( scanner.hasNext() ) {
        if( scanner.hasNextInt() ) {
            var n = scanner.nextInt()

            // Make sure they're allowed to use n
            if( n <= 0 || n >= 10 ) {
                print( n + " isn't allowed" )
                return 0
            }
            var idx = allowed.indexOf( n )
            if( idx == -1 ) {
                print( "You aren't allowed to use so many " + n + "s!" )
                return 0
            }

            // Add the input number to the stack
            stk.push( new Double( n ) )

            // Mark n as used
            allowed.remove( idx )
        } else {
            // It has to be an operator...
            if( stk.size() < 2 ) {
                print( "Invalid Expression: Stack underflow!" )
                return 0
            }

            // Gets the next operator as a single character token
            var s = scanner.next("[\\+-/\\*]")

            // Get the operands
            var r = stk.pop().doubleValue()
            var l = stk.pop().doubleValue()

            // Determine which operator and invoke it
            if( s.equals( "+" ) ) {
                stk.push( new Double( l + r ) )
            } else if( s.equals( "-" ) ) {
                stk.push( new Double( l - r ) )
            } else if( s.equals( "*" ) ) {
                stk.push( new Double( l * r ) )
            } else if( s.equals( "/" ) ) {
                if( r == 0.0 ) {
                    print( "Invalid Expression: Division by zero!" )
                    return 0
                }
                stk.push( new Double( l / r ) )
            } else {
                print( "Internal Error: looking for operator yielded '" + s + "'" )
                return 0
            }
        }
    }

    // Did they skip any numbers?
    if( allowed.size() != 0 ) {
        print( "You didn't use ${allowed}" )
        return 0
    }

    // Did they use enough operators?
    if( stk.size() != 1 ) {
        print( "Invalid Expression: Not enough operators!" )
        return 0
    }

    return stk.pop().doubleValue()
}

// Pick 4 random numbers from [1..9]
var nums = new ArrayList<Integer>()
var gen = new java.util.Random( new java.util.Date().getTime() )
for( i in 0..3 ) {
    nums.add( gen.nextInt(9) + 1 )
}

// Prompt the user
print( "Using addition, subtraction, multiplication and division, write an" )
print( "expression that evaluates to 24 using" )
print( "${nums.get(0)}, ${nums.get(1)}, ${nums.get(2)} and ${nums.get(3)}" )
print( "" )
print( "Please enter your expression in RPN" )

// Build a tokenizer over a line of input
var sc = new Scanner( new java.io.BufferedReader( new java.io.InputStreamReader( java.lang.System.in ) ).readLine() )

// eval the expression
var val = doEval( sc, nums )

// winner?
if( java.lang.Math.abs( val - 24.0 ) < 0.001 ) {
    print( "You win!" )
} else {
    print( "You lose!" )
}
