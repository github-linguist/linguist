contract token{
    function sendToken(address receiver,uint256 amount)returns(bool sufficient){}
    function getBalance(address account)returns(uint256 balance){}
}

contract Democracy {
    
    uint public minimumQuorum = 10;
    uint public debatingPeriod = 7 days;
    token public voterShare;
    address public founder;
    Proposal[] public proposals;
    
    struct Proposal {
        address recipient;
        uint amount;
        bytes32 data;
        bytes32 descriptionHash;
        uint creationDate;
        uint quorum;
        bool active;
        Vote[] votes;
        mapping (address => bool) voted;
    }
    
    struct Vote {
        int position;
        address voter;
    }
    
    function Democracy() {
        founder = msg.sender;   
    }
    
    function setup(address _voterShareAddress){
        if (msg.sender == founder && proposals.length == 0) {
            voterShare = token(_voterShareAddress);
        }       
    }
    
    function newProposal(address _recipient, uint _amount, bytes32 _data, bytes32 _descriptionHash) returns (uint proposalID) {
        if (voterShare.balances(msg.sender)>0) {
            proposalID = proposals.length++;
            Proposal p = proposals[proposalID];
            p.recipient = _recipient;
            p.amount = _amount;
            p.data = _data;
            p.descriptionHash = _descriptionHash;
            p.creationDate = now;
            p.active = true;
        } else {
            return 0;
        }
    }
    
    function vote(uint _proposalID, int _position) returns (uint voteID){
        if (voterShare.balances(msg.sender)>0 && (_position >= -1 || _position <= 1 )) {
            Proposal p = proposals[_proposalID];
            if (p.voted[msg.sender] != true) {
                voteID = p.votes.length++;
                Vote v = p.votes[voteID];
                v.position = _position;
                v.voter = msg.sender;   
                p.voted[msg.sender] = true;
            }
        } else {
            return 0;
        }
    }
    
    function executeProposal(uint _proposalID) returns (int result) {
        Proposal p = proposals[_proposalID];
        /* Check if debating period is over */
        if (now > (p.creationDate + debatingPeriod) && p.active){   
            
            /* tally the votes */
            for (uint i = 0; i <=  p.votes.length; i++) {
                Vote v = p.votes[i];
                int voteWeight = int(voterShare.balances(v.voter)); 
                p.quorum += uint(voteWeight);
                result += voteWeight * v.position;
            }
            /* execute result */
            if (p.quorum > minimumQuorum && result > 0 ) {
                p.recipient.call.value(p.amount)(p.data);
                p.active = false;
            } else if (p.quorum > minimumQuorum && result < 0) {
                p.active = false;
            }
        }
    }
}