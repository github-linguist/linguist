import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";

import Errors "errors";
import Events "events";

shared({caller}) actor class ERC20 (
    tkName : Text,       // Name of the token.
    tkSymbol : Text,     // Symbol of the token.
    tkDecimals : Nat8,   // Number of decimals the token uses.
    tkTotalSupply : Nat, // Total token supply.
) = {
    private stable let _name : Text = tkName;
    // Returns the name of the token.
    public query func name() : async Text {
        _name;
    };

    private stable let _symbol : Text = tkSymbol;
    // Returns the symbol of the token.
    public query func symbol() : async Text {
        _symbol;
    };

    private stable let _decimals : Nat8 = tkDecimals;
    // Returns the number of decimals the token uses.
    public query func decimals() : async Nat8 {
        _decimals;
    };

    private stable let _totalSupply : Nat = tkTotalSupply;
    // Returns the total token supply.
    public query func totalSupply() : async Nat {
        _totalSupply;
    };

    private stable var _balances : [(Principal, Nat)] = [];
    private var balances = HashMap.HashMap<
        Principal, // Address owner.
        Nat,       // Amount of tokens.
    >(0, Principal.equal, Principal.hash);
    balances.put(caller, tkTotalSupply);
    public query func balanceOf(owner : Principal) : async Nat {
        _balanceOf(owner);
    };
    // Returns the account balance of another account with address 'owner'.
    private func _balanceOf(owner : Principal) : Nat {
        switch (balances.get(owner)) {
            case (null)      { 0;       };
            case (? balance) { balance; };
        };
    };

    // Transfers 'value' amount of tokens to address 'to'.
    public shared({caller}) func transfer(
        to : Principal,                  // Recipient of the tokens.
        value : Nat,                     // Amount of tokens.
        transferEvent: ?Events.Transfer, // Transfer event.
    ) : async Bool {
        let balance = _balanceOf(caller);
        if (balance < value) { throw Errors.InsufficientBalance(balance, value); };
        _transfer(caller, to, value); // Transfer value.
        ignore Events.fireTransfer(   // Fire transfer event.
            transferEvent, caller, to, value,
        );
        true;
    };
    // @pre: account balance > value
    private func _transfer(from : Principal, to : Principal, value : Nat) {
        if (value == 0) { return; };

        let fromBalance          = _balanceOf(from);
        let newFromBalance : Nat = fromBalance - value;
        if (newFromBalance == 0) { balances.delete(from);              }
        else                     { balances.put(from, newFromBalance); };
        let toBalance          = _balanceOf(to);
        let newToBalance : Nat = toBalance + value;
        balances.put(to, newToBalance);
    };

    // Transfers 'value' amount of tokens from address 'from' to address 'to'.
    // NOTE: the caller must be approved by the owner to tranfer tokens, even if the caller is the owner.
    public shared({caller}) func transferFrom(
        from: Principal,                 // Owner of the tokens.
        to : Principal,                  // Recipient of the tokens.
        value : Nat,                     // Amount of tokens.
        transferEvent: ?Events.Transfer, // Transfer event.
    ) : async Bool {
        let allowed = _allowance(from, caller);
        if (allowed < value) { throw Errors.InsufficientAllowance(allowed, value); };
        let balance = _balanceOf(from);
        if (balance < value) { throw Errors.InsufficientBalance(balance, value); };
        let newAllowed : Nat = allowed - value;
        _approve(from, caller, newAllowed);
        _transfer(from, to, value);
        true;
    };

    // Allows 'spender' to withdraw from your account multiple times, up to the 'value' amount. 
    // If this function is called again it overwrites the current allowance with 'value'.
    public shared({caller}) func approve(
        spender: Principal,              // Spender of the tokens.
        value: Nat,                      // Amount of tokens.
        approvalEvent: ?Events.Approval, // Approval event.
    ) : async Bool {
        _approve(caller, spender, value);
        ignore Events.fireApproval( // Fire approval event.
            approvalEvent, caller, spender, value,
        );
        true;
    };
    private func _approve(owner : Principal, spender: Principal, value : Nat) {
        switch (allowances.get(owner)) {
            case (null) {
                if (value == 0) { return; };

                let allowance = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
                allowance.put(spender, value);
                allowances.put(owner, allowance);
            };
            case (? allowance) {
                if (value == 0) {
                    // Delete if value is 0.
                    allowance.delete(spender);
                    if (allowance.size() == 0) {
                        allowances.delete(owner);
                    };
                } else {
                    allowance.put(spender, value);
                    allowances.put(owner, allowance);
                };
            };
        };
    };

    private stable var _allowances : [(Principal, [(Principal, Nat)])] = [];
    private var allowances = HashMap.HashMap<
        Principal, // Address owner.
        HashMap.HashMap<
            Principal, // Address spender.
            Nat,       // Amount of tokens.
        >,
    >(1, Principal.equal, Principal.hash);
    // Returns the amount which 'spender' is still allowed to withdraw from 'owner'.
    public query func allowance(owner: Principal, spender: Principal) : async Nat {
        _allowance(owner, spender);
    };
    private func _allowance(owner: Principal, spender: Principal) : Nat {
        switch(allowances.get(owner)) {
            case (?allowance_owner) {
                switch(allowance_owner.get(spender)) {
                    case (?allowance) { return allowance; };
                    case (null)       { return 0; };
                }
            };
            case (null) { return 0; };
        }
    };

    system func preupgrade() {
        _balances := Iter.toArray(balances.entries());
    };

    system func postupgrade() {
        balances := HashMap.fromIter<Principal,Nat>(
            _balances.vals(),
            0,
            Principal.equal,
            Principal.hash,
        );
        _balances := [];
    };
};
