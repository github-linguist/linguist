contract token { 
    mapping (address => uint) public balances;
    
    /* Initializes contract with 10 000 tokens to the creator of the contract */
    function token() {
        balances[msg.sender] = 10000;
    }
    
    /* Very simple trade function */
    function sendToken(address receiver, uint amount) returns(bool sufficient) {
        if (balances[msg.sender] < amount) return false;
        balances[msg.sender] -= amount;
        balances[receiver] += amount;
        return true;
    }

    /* Try it yourself: merged mining

    mapping (uint => address) miningReward;
    function claimMiningReward() {
        if (miningReward[block.number] == 0) {
            balances[block.coinbase] += 1;
            miningReward[block.number] = block.coinbase;
        }
    }

    */
}
