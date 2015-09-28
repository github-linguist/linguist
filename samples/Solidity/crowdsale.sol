contract token { 
    mapping (address => uint) public balance; 
    function token() {}  
    function sendToken(address receiver, uint amount) returns(bool sufficient) {  } 
}

contract CrowdSale {
    
    address public admin;
    address public beneficiary;
    uint public fundingGoal;
    uint public amountRaised;
    uint public deadline;
    uint public price;
    token public tokenReward;   
    Funder[] public funders;
    
    /* data structure to hold information about campaign contributors */
    struct Funder {
        address addr;
        uint amount;
    }
    
    /*  at initialization, setup the owner */
    function CrowdSale() {
        admin = msg.sender;
    }   
    
    function setup(address _beneficiary, uint _fundingGoal, uint _duration, uint _price, address _reward) returns (bytes32 response){
        if (msg.sender == admin && !(beneficiary > 0 && fundingGoal > 0 && deadline > 0)) {
            beneficiary = _beneficiary;
            fundingGoal = _fundingGoal;
            deadline = now + _duration * 1 days;
            price = _price;
            tokenReward = token(_reward);    
            return "campaign is set";
        } else if (msg.sender != admin) {
            return "not authorized";
        } else  {
            return "campaign cannot be changed";
        }
    }
    
    /* The function without name is the default function that is called whenever anyone sends funds to a contract without specifying any extra data or if the data does not match any of the function signatures */
    function () returns (bytes32 response) {
        if (msg.data.length != 0) return;
        var numFunders = funders.length;
        Funder f = funders[numFunders++];
        f.addr = msg.sender;
        f.amount = msg.value;
        amountRaised += f.amount;
        tokenReward.sendToken(msg.sender, f.amount/price);
        return "thanks for your contribution";
    }
        
    /* checks if the goal or time limit has been reached and ends the campaign */
    function checkGoalReached() returns (bytes32 response) {
        if (amountRaised >= fundingGoal){
            beneficiary.send(amountRaised);
            suicide(beneficiary);
        }
        else if (deadline <= block.number){
            for (uint i = 0; i < funders.length; i++) {
                funders[i].addr.send(funders[i].amount);
                funders[i].addr = 0;
                funders[i].amount = 0;
            }
            suicide(beneficiary);
            return "Deadline passed";
        }
        return "Not reached yet";
    }
}