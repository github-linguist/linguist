// The 'vote.leo' program.
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
    transition propose(public info: ProposalInfo) -> Proposal {
        // Authenticate proposer.
        assert_eq(self.caller, info.proposer);

        // Generate a new proposal id.
        let id: field = BHP256::hash_to_field(info.title);


        // Return a new record for the proposal.
        // Finalize the proposal id.
        return Proposal {
            owner: self.caller,
            id,
            info,
        } then finalize(id);
    }
    // Create a new proposal in the "tickets" mapping.
    finalize propose(public id: field) {
        Mapping::set(tickets, id, 0u64);
    }

    // Create a new ticket to vote with.
    transition new_ticket(
        public pid: field,
        public voter: address,
    ) -> Ticket {

        // Finalize the proposal id for the ticket.
        return Ticket {
            owner: voter,
            pid,
        } then finalize(pid);
    }
    // Create a new ticket on a proposal in the "tickets" mapping.
    finalize new_ticket(public pid: field) {
        let current: u64 = Mapping::get_or_use(tickets, pid, 0u64);
        Mapping::set(tickets, pid, current + 1u64);
    }

    // Vote privately to agree with a proposal.
    transition agree(ticket: Ticket) {
        // Finalize this vote.
        return then finalize(ticket.pid);
    }
    finalize agree(public pid: field) {
        // Publicly increment the number of agree votes.
        let current: u64 = Mapping::get_or_use(agree_votes, pid, 0u64);
        Mapping::set(agree_votes, pid, current + 1u64);
    }

    // Vote privately to disagree with a proposal.
    transition disagree(ticket: Ticket) {
        // Finalize this vote.
        return then finalize(ticket.pid);
    }
    finalize disagree(pid: field) {
        // Publicly increment the number of disagree votes.
        let current: u64 = Mapping::get_or_use(disagree_votes, pid, 0u64);
        Mapping::set(disagree_votes, pid, current + 1u64);
    }
}
