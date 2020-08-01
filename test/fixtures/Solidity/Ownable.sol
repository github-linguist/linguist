pragma solidity 0.5.1;

/// @dev Stub for access control.

contract Ownable {
    
    address public owner;
    
    modifier onlyOwner {
        require(msg.sender == owner);
        _;
    }
    
    event LogNewOwner(address sender, address newOwner);
    
    constructor() public {
        owner = msg.sender;
    }
    
    function changeOwner(address newOwner) public onlyOwner {
        require(newOwner != address(0));
        emit LogNewOwner(msg.sender, newOwner);
    }
}
