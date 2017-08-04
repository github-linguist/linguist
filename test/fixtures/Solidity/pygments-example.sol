pragma solidity ^0.4.1;

/**********************************************************************
 *                             example.sol                            *
 **********************************************************************/

// Code in this contract is not meant to work (or be a good example).
// It is meant to demonstrate good syntax highlighting by pygments,
// even if otherwise hazardous.

// Comments relevant to the lexer are single-line.
/* Comments relevant to the code are multi-line. */

library Assembly {
    function junk(address _addr) returns (address _ret) {
        assembly {
            let tmp := 0

            // nested code block
            let mulmod_ := 0 { // evade collision with `mulmod`
                let tmp:=sub(mulmod_,1) // `tmp` is not a label
                mulmod_ := tmp
            }
            /* guess what mulmod_ is now... */
        _loop: // JIC, dots are invalid in labels
            let i := 0x10
        loop:
            // Escape sequences in comments are not parsed.
            /* Not sure what's going on here, but it sure is funky!
             \o/ \o/ \o/ \o/ \o/ \o/ \o/ \o/ \o/ \o/ \o/ \o/ \o/ */
            mulmod(_addr, mulmod_, 160)
            
            0x1 i sub // instructional style
            i =: tmp /* tmp not used */
            
            jumpi(loop, not(iszero(i)))
            
            mstore(0x0, _addr)
            return(0x0, 160)
        }
    }
}

contract Strings {
    // `double` is not a keyword (yet)
    string double = "This\ is a string\nwith \"escapes\",\
and it's multi-line. // no comment"; // comment ok // even nested :)
    string single = 'This\ is a string\nwith "escapes",\
and it\'s multi-line. // no comment'; // same thing, single-quote
    string hex = hex'537472696e67732e73656e6428746869732e62616c616e6365293b';

    function(){}
}

contract Types is Strings {
    using Assembly for Assembly.junk;
    
    // typesM (compiler doesn't choke on invalid)
    int8 i8; // valid
    int10 i10; // invalid
    uint256 ui256; // valid
    uint9001 ui9001; // invalid
    bytes1 b1; //valid
    bytes42 b42; // invalid - M out of range for `bytes`
    
    // typesMxN (compiler doesn't choke on invalid)
    fixed0x8 f0x8; // valid
    fixed8x0 f8x0; // invalid - N can't be 0
    ufixed42x217 uf42x217; // invalid - M and N must be multiples of 8

    // special cases (internally not types)
    string str; // dynamic array (not a value-type)
    bytes bs; // same as above
    //var v = 5; // `var` is a keyword, not a type, and compiler chokes

    address a = "0x1"; // lexer parses as string
    struct AddressMap {
        address origin;
        address result;
        address sender;
        bool touched;
    }
    mapping (address => AddressMap) touchedMe;

    function failOnNegative(int8 _arg)
        private
        constant
        returns (uint256)
    {
        /* implicit type conversion from `int8` to `uint256` */
        return _arg;
    }

    // some arithmetic operators + built-in names
    function opportunisticSend(address k) private {
        /* `touchedMe[k].result` et al are addresses, so
           `send()` available */
        touchedMe[k].origin.send(k**2 % 100 finney);
        touchedMe[k].result.send(1 wei);
        touchedMe[k].sender.send(mulmod(1 szabo, k, 42));
    }

    function() payable {
        /* inferred type: address */
        var k = msg.sender;
        /* inferred type: `ufixed0x256` */
        var v = 1/42;
        /* can't be `var` - location specifier requires explicit type */
        int storage negative = -1;

        // valid syntax, unexpected result - not our problem
        ui256 = failOnNegative(negative);
        
        // logic operators
        if ((!touchedMe[msg.sender].touched &&
             !touchedMe[tx.origin].touched) ||
            ((~(msg.sender * v + a)) % 256 == 42)
        ) {
            address memory garbled = Assembly.junk(a + msg.sender);

            /* create a new AddressMap struct in storage */
            AddressMap storage tmp;

            // TODO: highlight all known internal keywords?
            tmp.origin = tx.origin;
            tmp.result = garbled;
            tmp.sender = msg.sender;
            tmp.touched = true;

            /* does this link-by-reference as expected?.. */
            touchedMe[msg.sender] = tmp;
            touchedMe[tx.origin] = tmp;
        }
        else {
            /* weak guard against re-entry */
            touchedMe[k].touched = false;
            
            opportunisticSend(k);
            
            delete touchedMe[k];
            /* these probably do nothing... */
            delete touchedMe[msg.sender];
            delete touchedMe[tx.origin];
        }
    }
}

/**
   \brief Examples of bad practices.

   TODO: This special doxygen natspec notation is not parsed yet.

   @author Noel Maersk
 */
/// TODO: Neither is this one.

contract BadPractices {
    address constant creator; /* `internal` by default */
    address private owner; /* forbid inheritance */
    bool mutex;

    modifier critical {
        if (mutex) throw;
        mutex = true;
        _;
        mutex = false;
    }
    
    /* constructor */
    function BadPractices() {
        creator = tx.origin;
        owner = msg.sender;
    }

    /* Dangerous - function public (by default), and doesn't check
       who's calling. */
    function withdraw(uint _amount)
        critical
        returns (bool)
    { /* `mutex` set via modifier */
        if (msg.sender.call.value(_amount)())
            throw; /* Throwing on failed call is dangerous.
                      Consider returning false instead?.. */
        return true;
    } /* `mutex` reset via modifier */
    
    /* fallback */
    function () payable {
        /* `i` will be `uint8`, so this is an endless loop
           that will consume all gas and eventually throw.
         */
        for (var i = 0; i < 257; i++) {
            owner++;
        }
    }
}

/*
// Open comment to EOF. Compiler chokes on this, but it's useful
// for highlighting to show that there's an unmatched multi-line
// comment open.

contract MoreBadPractices is BadPractices {
    uint balance;

    // These would close the comment if the space was removed:
    * /
    \* /

    / * no modifiers to check ownership * /
    function () payable {
        balance += msg.value;

        / * vulnerable to re-entry * /
        if (!msg.sender.send(this.balance / 10)) throw;
        balance -= this.balance;
    }
}
