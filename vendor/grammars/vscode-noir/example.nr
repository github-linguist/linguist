// Comments

/*
    /*
        Multi-Layered Comments
    */
*/


// Strings
"String with \e escape characters"

// Numerics
-5, 6666.555, 7

// Var Names
u7, u4, u567585, [53]Field
CustomType, customVariable,
_CustomUnusedType, _custumUnusedVariable
function(), Class(), _function()
//structure<Hello, World <andEveryoneElse>> // THIS BREAKS: x < y

#[builtin(hello)]
fn WithComment() {
    
}

// Import

use x::y::z::{
    w, v::a
}

// Structs
struct structure {
    Var   : type,  // First-Letter-Capitalization ignored
    Var2  : type

    x // Over
    : // Many
    y // Lines

    // Note! ":" is simply ignored by the syntax highlighter:
    x y()

    // Comma separates:
    x, y, z,

    // Types in Types
    val : Struct<More, Structs>
}

struct TupleStruct (basically same, stuff going <on>)

struct WithoutContent

if xx {} else if yy {} else {}

while () {}

let x = true
let y = false

// Example

// https://github.com/noir-lang/noir/blob/master/examples/1/src/foo.nr
mod foo;

fn hello(x : pub Field) -> Field {
    x
}

// https://github.com/noir-lang/noir/blob/master/examples/1/src/main.nr
fn main(x : Field, y : pub Field) {
    constrain x != foo::hello(y);
}

// https://github.com/noir-lang/noir/blob/master/examples/2/src/main.nr
fn main(x : Field, y : Field) {
    priv x_as_u8 = x as u8;
    priv y_as_u8 = y as u8;
    
    constrain (x_as_u8 & y_as_u8) == y;
}

// https://github.com/noir-lang/noir/blob/master/examples/3/src/main.nr
fn main(x : u8, y : Field) {
    priv _z = x + (y as u8);
}

// https://github.com/noir-lang/noir/blob/master/examples/4/src/main.nr
fn main(x : Field, y : Field) {
    const _a = 1;
    priv k = x + y;

    let j = for _k in _a..100 {
       priv z = x + k;
       z
    };

    constrain k == j[1];
}

// https://github.com/noir-lang/noir/blob/master/examples/5/src/main.nr
use dep::std;

fn main(x : Field, result : [32]Field) {
    let digest = std::hash::sha256([x as u8]);
     
    constrain digest == result;
}

// https://github.com/noir-lang/noir/blob/master/examples/6/src/main.nr
use dep::std;

fn main(x : [5]u8, result : pub [32]u8) {
     let digest = std::hash::sha256(x);
     
     constrain digest == result;
}

// https://github.com/noir-lang/noir/blob/master/examples/7/src/main.nr
use dep::std;

fn main(x : [5]u8, result : [32]u8) {
     let digest = std::hash::blake2s(x);
     constrain digest == result;
}

