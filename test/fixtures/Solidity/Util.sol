/**
 * The Util provides utility functions
 */
pragma solidity ^0.5.0;
 
library Util {
    
	function addressToBytes32(address a) public pure returns(bytes32) {
	  return bytes32(uint(uint160(a)));
	}
	function bytes32ToAddress(bytes32 b) public pure returns(address) {
	  return address(uint160(uint(b)));
	}

	function max(uint a, uint b) public pure returns (uint) {
	        return a > b ? a : b;
	    }
	    
	function min(uint a, uint b) public pure returns (uint) {
	        return a < b ? a : b;
	    }	

	function genRandom() private view returns (uint) {
    	uint rand = uint(keccak256(abi.encodePacked(block.blockhash(block.number-1)))) + uint(keccak256(abi.encodePacked(block.coinbase)));
    	return uint(rand % (10 ** 20));
	}
}


