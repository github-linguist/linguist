/**
 * The Util provides utility functions
 */
pragma solidity ^0.5.0;
 
contract BaseAttack {
    bool constant public hasTarget = true;
    uint constant public cost = 0;

    function getAtkPower(address target, uint value) public returns(uint res) {
		return 4;
    }
    
}


