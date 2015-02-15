uses java.lang.Integer
uses java.lang.Double
uses java.lang.System
uses java.util.ArrayList
uses java.util.LinkedList
uses java.util.List
uses java.util.Scanner
uses java.util.Stack

function permutations<T>( lst : List<T> ) : List<List<T>> {
    if( lst.size() == 0 ) return {}
    if( lst.size() == 1 ) return { lst }

    var pivot = lst.get(lst.size()-1)

    var sublist = new ArrayList<T>( lst )
    sublist.remove( sublist.size() - 1 )

    var subPerms = permutations( sublist )

    var ret = new ArrayList<List<T>>()
    for( x in subPerms ) {
        for( e in x index i ) {
            var next = new LinkedList<T>( x )
            next.add( i, pivot )
            ret.add( next )
        }
        x.add( pivot )
        ret.add( x )
    }
    return ret
}

function readVals() : List<Integer> {
    var line = new java.io.BufferedReader( new java.io.InputStreamReader( System.in ) ).readLine()
    var scan = new Scanner( line )

    var ret = new ArrayList<Integer>()
    for( i in 0..3 ) {
        var next = scan.nextInt()
        if( 0 >= next || next >= 10 ) {
            print( "Invalid entry: ${next}" )
            return null
        }
        ret.add( next )
    }
    return ret
}

function getOp( i : int ) : char[] {
    var ret = new char[3]
    var ops = { '+', '-', '*', '/' }
    ret[0] = ops[i / 16]
    ret[1] = ops[(i / 4) % 4 ]
    ret[2] = ops[i % 4 ]
    return ret
}

function isSoln( nums : List<Integer>, ops : char[] ) : boolean {
    var stk = new Stack<Double>()
    for( n in nums ) {
        stk.push( n )
    }

    for( c in ops ) {
        var r = stk.pop().doubleValue()
        var l = stk.pop().doubleValue()
        if( c == '+' ) {
            stk.push( l + r )
        } else if( c == '-' ) {
            stk.push( l - r )
        } else if( c == '*' ) {
            stk.push( l * r )
        } else if( c == '/' ) {
            // Avoid division by 0
            if( r == 0.0 ) {
                return false
            }
            stk.push( l / r )
        }
    }

    return java.lang.Math.abs( stk.pop().doubleValue() - 24.0 ) < 0.001
}

function printSoln( nums : List<Integer>, ops : char[] ) {
    // RPN: a b c d + - *
    // Infix (a * (b - (c + d)))
    print( "Found soln: (${nums.get(0)} ${ops[0]} (${nums.get(1)} ${ops[1]} (${nums.get(2)} ${ops[2]} ${nums.get(3)})))" )
}

System.out.print( "#> " )
var vals = readVals()

var opPerms = 0..63
var solnFound = false

for( i in permutations( vals ) ) {
    for( j in opPerms ) {
        var opList = getOp( j )
        if( isSoln( i, opList ) ) {
            printSoln( i, opList )
            solnFound = true
        }
    }
}

if( ! solnFound ) {
    print( "No solution!" )
}
