// This program follows the license outlined, here: https://github.com/AleoHQ/leo/blob/mainnet/LICENSE.md

program vote.aleo {
    // Proposal details
    struct ProposalInfo {
        title: field,
        content: field,
        proposer: address,
    }

    // Proposal record records proposal info publicly
    record Proposal {
        owner: address,
        id: field,
        info: ProposalInfo,
    }

    // Save proposal info in public storage.
    mapping proposals: field => ProposalInfo;

    // Privacy tickets to vote
    record Ticket {
        owner: address,
        pid: field,
    }

    // Count the total tickets issued for each proposal
    mapping tickets: field => u64;

    mapping agree_votes: field => u64;

    mapping disagree_votes: field => u64;

    // Propose a new proposal to vote on.
    async transition propose(public info: ProposalInfo) -> (Proposal, Future) {
        // Authenticate proposer.
        assert_eq(self.caller, info.proposer);

        // Generate a new proposal id.
        let id: field = BHP256::hash_to_field(info.title);


        // Return a new record for the proposal.
        // Finalize the proposal id.
        return (Proposal { owner: self.caller, id, info }, finalize_propose(id));
    }
    // Create a new proposal in the "tickets" mapping.
    async function finalize_propose(public id: field) {
        Mapping::set(tickets, id, 0u64);
    }

    // Create a new ticket to vote with.
    async transition new_ticket(
        public pid: field,
        public voter: address,
    ) -> (Ticket, Future) {

        // Finalize the proposal id for the ticket.
        return (Ticket { owner: voter, pid }, finalize_new_ticket(pid));
    }
    // Create a new ticket on a proposal in the "tickets" mapping.
    async function finalize_new_ticket(public pid: field) {
        let current: u64 = Mapping::get_or_use(tickets, pid, 0u64);
        Mapping::set(tickets, pid, current + 1u64);
    }

    // Vote privately to agree with a proposal.
    async transition agree(ticket: Ticket) -> Future {
        // Finalize this vote.
        return finalize_agree(ticket.pid);
    }
    async function finalize_agree(public pid: field) {
        // Publicly increment the number of agree votes.
        let current: u64 = Mapping::get_or_use(agree_votes, pid, 0u64);
        Mapping::set(agree_votes, pid, current + 1u64);
    }

    // Vote privately to disagree with a proposal.
    async transition disagree(ticket: Ticket) -> Future {
        // Finalize this vote.
        return finalize_disagree(ticket.pid);
    }
    async function finalize_disagree(pid: field) {
        // Publicly increment the number of disagree votes.
        let current: u64 = Mapping::get_or_use(disagree_votes, pid, 0u64);
        Mapping::set(disagree_votes, pid, current + 1u64);
    }
}
