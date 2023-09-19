program basic_bank.aleo {
    // A token, issued by a bank.
    // - 'owner'  : The address of the account that owns the record associated with this token.
    // - 'amount' : The amount of tokens owned by the account.
    record Token {
        owner: address,
        amount: u64,
    }

    // An on-chain mapping, storing the amount of tokens owned by each account
    // The account is stored as a to preserve user privacy.
    mapping balances: field => u64;

    // Returns a new Token.
    // - `owner` : The address of the account to issue the token to.
    // - `amount`: The amount of tokens to issue.
    // Requires that the function caller is the bank.
    // The bank's address is aleo1t0uer3jgtsgmx5tq6x6f9ecu8tr57rzzfnc2dgmcqldceal0ls9qf6st7a.
    transition issue(owner: address, amount: u64) -> Token {
        assert_eq(self.caller, aleo1t0uer3jgtsgmx5tq6x6f9ecu8tr57rzzfnc2dgmcqldceal0ls9qf6st7a);
        return Token {
            owner: owner,
            amount: amount,
        };
    }

    // Deposits some amount of money into the bank.
    // Returns a new Token with the remaining amount of money.
    // - `token` : A record containing tokens to deposit.
    // - `amount`: The amount of tokens to deposit.
    transition deposit(token: Token, amount: u64) -> Token {
        let difference: u64 = token.amount - amount;

        let remaining: Token = Token {
            owner: token.owner,
            amount: difference,
        };

        // Compute the hash of the token owner.
        let hash: field = BHP256::hash_to_field(token.owner);

        return remaining then finalize(hash, amount);
    }

    // Updates on-chain state by the amount of tokens deposited.
    // - `hash`  : The hash of the token owner.
    // - `amount`: The amount of tokens that were deposited.
    finalize deposit(hash: field, amount: u64) {
        let current_amount: u64 = Mapping::get_or_use(balances, hash, 0u64);
        Mapping::set(balances, hash, current_amount + amount);
    }

    // Returns a new Token containing the amount of money withdrawn.
    // - `recipient`: The address of the account to withdraw the tokens to.
    // - `amount`   : The amount of tokens to withdraw.
    // - `rate`     : The compound interest rate.
    // - `periods`  : The number of periods to compound the interest over.
    // Requires that the function caller is the bank.
    transition withdraw(recipient: address, amount: u64, rate: u64, periods: u64) -> Token {
        assert_eq(self.caller, aleo1t0uer3jgtsgmx5tq6x6f9ecu8tr57rzzfnc2dgmcqldceal0ls9qf6st7a);
        let hash: field = BHP256::hash_to_field(recipient);

        let total: u64 = calculate_interest(amount, rate, periods);

        let token: Token = Token {
            owner: recipient,
            amount: total,
        };

        return token then finalize(hash, amount);
    }

    // Updates on-chain state by the amount of tokens withdrawn.
    // - `hash`  : The hash of the token owner.
    // - `amount`: The amount of tokens that were withdrawn.
    finalize withdraw(hash: field, amount: u64) {
        let current_amount: u64 = Mapping::get_or_use(balances, hash, 0u64);
        Mapping::set(balances, hash, current_amount - amount);
    }

    // Returns the total amount of tokens after compounding interest.
    // - `principal`: The amount of tokens to compound interest over.
    // - `rate`     : The compound interest rate.
    // - `periods`  : The number of periods to compound the interest over.
    function calculate_interest(principal: u64, rate: u64, periods: u64) -> u64 {
        let amount: u64 = principal;

        for i:u64 in 0u64..100u64 {
            if i < periods {
                amount += (amount * rate) / 10000u64;
            }
        }

        return amount;
    }
}
