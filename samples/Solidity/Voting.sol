
pragma solidity ^0.4.18;

contract Voting
{
    struct Proposal
    {
        bytes32 id;
        bytes32 description;
        uint numberOfVotes;
        address author;
    }

    struct Voter
    {
        bool authorised;
        address delegate;
        uint[] votedProposalIndices;
    }

    address public daoChair;
    Proposal [] proposals; //purposely left the [] spaced
    mapping (address => Voter) voters;

    function Voting ()
    {
        daoChair = msg.sender;
    }

    function setProposals(bytes32[] proposalDescriptions)
    {
        require(msg.sender == daoChair); //only chair can do this

        for (var index = 0; index < proposalDescriptions.length; index++) {

            bytes32 id = keccak256(proposalDescriptions[index]);
            proposals.push(Proposal(id, proposalDescriptions[index], 0, daoChair)); 
        }
    }

    function setVoters(address[] voterAddresses)
    {   
        require(msg.sender == daoChair); //only chair can do this

        uint[] memory dummyArray = new uint[](5);
        
        for (uint index = 0; index < voterAddresses.length; index++) {

            voters[voterAddresses[index]] = Voter({authorised : true, 
                                                delegate : 0x0, 
                                                votedProposalIndices : dummyArray}); 
        }
    }

    function submitProposal(bytes32 proposal)
    {
        require(voters[msg.sender].authorised == true || msg.sender == daoChair); //check that submitting address(author) is in list f voters

        //check that proposal dsnt already exist....not implementing...cos it wld waste gas
        
        bytes32 id = keccak256(proposal);
        proposals.push(Proposal(id, proposal, 0, msg.sender)); 
    }

    function viewProposal(uint proposalIndex) 
    public 
    view
        returns ( bytes32 id, uint numberOfVotes, bytes32 description, address author)
    {
        require(msg.sender == daoChair || 
                voters[msg.sender].authorised == true);  //check that viewer is either authd or chair   
        
        id = proposals[proposalIndex].id;
        numberOfVotes = proposals[proposalIndex].numberOfVotes;
        description = proposals[proposalIndex].description;
        author = proposals[proposalIndex].author;
    }

    function viewProposalsVotedByVoter(address voter) //not giving results, tho votedProposalIndices outputs well
    public 
    view
        returns ( bytes32[] id, uint[] numberOfVotes, bytes32[] description, address[] author)
    {
        if (voter == 0x0)//if no voter addr, then it has to be dt voter = msg.sendr
        {
            voter = msg.sender;
        }

        require(msg.sender == daoChair || voters[voter].authorised == true); //check that viewer is either an authd voter or chair
        
        uint[] votedIndices = voters[voter].votedProposalIndices;

        for (var index = 0; index < votedIndices.length; index++) {

            id[index] = proposals[votedIndices[index]].id;
            numberOfVotes[index] = proposals[votedIndices[index]].numberOfVotes;
            description[index] = proposals[votedIndices[index]].description;
            author[index] = proposals[votedIndices[index]].author;
        }
    }

    function submitVote(uint proposalIndex, address delegator)
    {
        //check voter is authd or been delegated to
        require(voters[msg.sender].authorised == true || msg.sender == voters[delegator].delegate);
        
        voters[delegator].delegate = 0x0; //if delegated, allow vote and reset delegate afterwards. if cost f checking wheda delegate is high, then don't.
       
        voters[msg.sender].votedProposalIndices.push(proposalIndex);
        proposals[proposalIndex].numberOfVotes = proposals[proposalIndex].numberOfVotes + 1;
    }
    
    function delegateVote(address delegatee)
    {
        require(voters[msg.sender].authorised == true);
        
        voters[msg.sender].delegate = delegatee;
        
    }
    
    function viewVoters(address voter) //redundant - just for check
    public 
    view
        returns ( bool authorised,  address delegate, uint[] votedProposalIndices)
    {
        authorised = voters[voter].authorised;
        delegate = voters[voter].delegate;
        votedProposalIndices =  voters[voter].votedProposalIndices;
    }
}
