// This is an implementation of the FA1.2 specification in ReasonLIGO
type account = {
  balance: nat,
  allowances: map(address, nat)
};

type balance_params = { callback: contract (nat), owner: address };
type allowance_params = { owner: address, spender: address, callback: contract (nat)}

type action =
  | Transfer ((address, address, nat))
  | Mint (nat)
  | Burn (nat)
  | SetBuyPrice (tez)
  | SupplyBuyPool (nat)
  | Buy (nat)
  | Approve ((address, nat))
  | RemoveApproval (address)
  | AddExtra ((string, string))
  | GetAllowance (allowance_params)
  | GetBalance (balance_params)
  | GetTotalSupply (contract(nat))
  | Pause;

type metadata = {
  token_id : nat,
  symbol : string,
  name : string,
  decimals : nat,
  extras : map (string, string)
};

type storage = {
  owner: address,
  metadata: metadata,
  buyPrice: tez,
  tokenBuyPool: nat,
  totalSupply: nat,
  ledger: big_map(address, account),
  paused: bool
};

let isAllowed = (accountFrom: address, value: nat, s: storage): bool => {
  if (Tezos.sender != accountFrom) {
    // Checking if the sender is allowed to spend in name of accountFrom
    switch (Big_map.find_opt(accountFrom, s.ledger)){
      | None => false
      | Some (acc) => {
          switch (Map.find_opt(Tezos.sender, acc.allowances)){
          | None => false
          | Some (allowanceAmount) => allowanceAmount >= value
        }
      }
    }
  } else {
    true;
  }
}

// Transfer a specific nat of tokens from accountFrom address to a destination address
// Preconditions:
//  The sender address is the account owner or is allowed to spend x in the name of accountFrom
//  The accountFrom account has a balance higher than the nat
// Postconditions:
//  The balance of accountFrom is decreased by the nat
//  The balance of destination is increased by the nat
let transfer = (accountFrom: address, accountTo: address, value: nat, s: storage): storage => {
  // If accountFrom = destination transfer is not necessary
  if(accountFrom == accountTo) {
    failwith ("SameOriginAndDestination"): storage;
  } else {
    if(!isAllowed(accountFrom, value, s)){
      failwith ("NotEnoughAllowance"): storage;
    } else {
      // Fetch src account
      let src = switch(Map.find_opt(accountFrom, s.ledger)) {
        | None => failwith ("NoAccount"): account;
        | Some (src) => src
      };
      // Check that the accountFrom can spend that much
      if(value > src.balance){
        failwith ("NotEnoughBalance"): storage;
      } else {
        // Update the accountFrom balance
        // Using the abs function to convert int to nat
        let src_ = {...src, balance: abs(src.balance - value)};

        // Fetch destination account or add empty destination account to ledger
        // Update the destination balance
        let dest: account = switch(Map.find_opt(accountTo, s.ledger)){
          | None => {
              balance: value,
              allowances: Map.empty: map(address, nat)
            }
          | Some (acc) => {...acc, balance: acc.balance + value}
        };

        // Decrease the allowance nat if necessary
        let new_allowances = switch(Map.find_opt(Tezos.sender, src_.allowances)){
          | None => src_.allowances
          | Some (dstAllowance) => 
            Map.update(Tezos.sender, Some (abs(dstAllowance - value)), src_.allowances) // ensure non negative
        };

        let new_storage = 
          {...s, 
            ledger: Big_map.update(accountFrom, Some ({...src_, allowances: new_allowances}), 
            s.ledger)};

        {...new_storage, ledger: Big_map.update(accountTo, Some (dest), new_storage.ledger)}
      }
    }
  }
}

// Mint tokens into the owner balance
// Preconditions:
//  The sender is the owner of the contract
// Postconditions:
//  The minted tokens are added in the balance of the owner
//  The totalSupply is increased by the nat of minted token
let mint = (value: nat, s: storage): storage => {
  // If the sender is not the owner fail
  if(Tezos.sender != s.owner){
    failwith ("UnauthorizedAccess"): storage;
  } else {
    let ownerAccount: account = switch(Map.find_opt(s.owner, s.ledger)){
      | None => {
          balance: value,
          allowances: Map.empty: map(address, nat)
        }
      | Some (acc) => 
        // Updates the owner balance
        {...acc, balance: acc.balance + value}
    };
    
    // returns the new storage
    {...s, ledger: Big_map.update(s.owner, Some (ownerAccount), s.ledger), totalSupply: s.totalSupply + value}
  }
}

// Burn tokens from the owner balance
// Preconditions:
//  The owner have the required balance to burn
// Postconditions:
//  The burned tokens are subtracted from the balance of the owner
//  The totalSupply is decreased by the nat of burned token 
let burn = (value: nat, s : storage):  storage => {
  // If the sender is not the owner fail
  if(Tezos.sender != s.owner){
    failwith("UnauthorizedAccess"):  storage;
  } else {
    let ownerAccount = switch(Big_map.find_opt(s.owner, s.ledger)){
      | None => {balance: 0n, allowances: Map.empty: map(address, nat)}
      | Some (acc) => acc
    };
    // Check that the owner can burn that much
    if(value > ownerAccount.balance){
      failwith ("Owner balance is too low"): storage;
    } else {
      // Update the owner balance
      // Using the abs function to convert int to nat
      let new_ownerAccount = {...ownerAccount, balance: abs(ownerAccount.balance - value)};

      {...s, 
        ledger: Big_map.update(s.owner, Some (new_ownerAccount), s.ledger), 
        totalSupply: abs(s.totalSupply - value)};
    }
  }
}

// Approve an nat to be spent by another address in the name of the sender
// Preconditions:
//  The spender account is not the sender account
// Postconditions:
//  The allowance of spender in the name of sender is value
let approve = (spender: address, value: nat, s : storage): storage => {
  // If sender is the spender approving is not necessary
  if(Tezos.sender == spender){
    failwith ("The account owner and the spender address cannot be the same!"): storage;
  } else {
    let src: account = switch(Big_map.find_opt(Tezos.sender, s.ledger)){
      | None => failwith ("NoAccount"): account;
      | Some (acc) => acc
    };
    // Checks if allowance has not previously been set to avoid attack vector
    let new_allowances = switch(Map.find_opt(spender, src.allowances)) {
      | None => Map.update(spender, Some (value), src.allowances)
      | Some (allowance) => {
        // attempt to change approval value from non-zero to non-zero 
        if(allowance > 0n && value > 0n) {
          failwith ("UnsafeAllowanceChange"): map(address, nat)
        } else {
          Map.update(spender, Some (value), src.allowances)
        }
      }
    };

    {...s, 
      ledger: Big_map.update(Tezos.sender, Some ({...src, allowances: new_allowances}), s.ledger)
    }
  }
}

// Remove an address that was earlier approved to spend tokens on behalf of the account
// Preconditions:
//  The spender account is not the sender account
//  The spender address exists in the account
// Postconditions:
// The spender address no longer exists in the account

let removeApproval = (spender: address, s: storage): storage => {
  // If sender is the spender approving is not necessary
  if(Tezos.sender == spender){
    failwith ("The account owner and the spender address cannot be the same!"): storage;
  } else {
    switch(Big_map.find_opt(Tezos.sender, s.ledger)){
      | None => failwith ("Account not found!"): storage;
      | Some (acc) => {...s, 
          ledger: Big_map.update(Tezos.sender, Some ({...acc, allowances: Map.remove(spender, acc.allowances)}), 
          s.ledger)
        }
    };
  }
}

// Set a buy price for the tokens from the contract's owner
// Preconditions:
// The function is called by the contracts's owner
// Postconditions:
// The buy price has been updated
let setBuyPrice = (buyPrice: tez, s: storage): storage => {
  if (Tezos.source != s.owner) {
    failwith ("UnauthorizedOperation"): storage;
  } else {
    { ...s, buyPrice: buyPrice }
  }
}

// Transfers tokens from owner's account to buy pool
// Preconditions:
// Owner has sufficient tokens to transfer
// Postconditions:
// Buy pool has been increased to the right amount of tokens
let supplyBuyPool = (tokenAmount: nat, s: storage): storage => {
  if (Tezos.source != s.owner) {
    failwith ("UnauthorizedOperation"): storage;
  } else {
    // checks if owner has enough tokens to supply the buy pool
    let ownerAccount = switch(Big_map.find_opt(Tezos.source, s.ledger)){
      | None => failwith ("NoAccount"): account
      | Some (acc) => if (acc.balance < tokenAmount) {
          failwith ("InsufficientTokenBalance"): account;
        } else {
          // deducts token amount from owner's balance
          {...acc, balance: abs(acc.balance - tokenAmount)}
        }
    };
    // updates the storage with new value for buy pool and new owner's account
    { ...s, 
      ledger: Big_map.update(Tezos.source, Some (ownerAccount), s.ledger), 
      tokenBuyPool: s.tokenBuyPool + tokenAmount }
  }
}

// Users can buy tokens from the contract at buy price
// Preconditions: 
// buyPrice is superior to zero
// Amount of tez is attached to the transaction
// Amount of tez is equal to token amount * buy price
// Buy pool has sufficient tokens to transfer
// Postconditions:
// Tokens have been transferred
let buy = (tokenAmount: nat, s: storage): storage => {
  // checks if amount is provided
  if(Tezos.amount == 0tez){
    failwith ("NoAmount"): storage;
  } else {
    if (s.tokenBuyPool < tokenAmount){
      failwith ("InsufficientBuyPool"): storage;
    } else {
      // checks if amount is equal to token amount * buy price
      if(Tezos.amount != tokenAmount * s.buyPrice){
        failwith ("IncorrectAmount"): storage;
      } else {
        // deducts tokens from buy pool
        let newBuyPool = abs(s.tokenBuyPool - tokenAmount);
        // transfers tokens from buy pool to user's account
        let userAccount = switch(Big_map.find_opt(Tezos.sender, s.ledger)){
          | None => { balance: tokenAmount, allowances: Map.empty: map(address, nat) }
          | Some (acc) => { ...acc, balance: acc.balance + tokenAmount }
        };
        // returns updated storage
        { ...s, 
          tokenBuyPool: newBuyPool, 
          ledger: Big_map.update(Tezos.sender, Some (userAccount), s.ledger) }
      }
    }
  }
}

// Add extra information about the token
// Preconditions:
// Only available to admin
// Postconditions:
// Extras map in metadata is updated
let addExtra = (key: string, value: string, s: storage): storage => {
  if(Tezos.source != s.owner) {
    failwith ("UnauthorizedOperation"): storage;
  } else {
   {...s, metadata: {...s.metadata, extras: Map.update(key, Some (value), s.metadata.extras)}}
  }
}

// View function that forwards the allowance nat of spender in the name of owner to a contract
// Preconditions:
//  None
// Postconditions:
//  The state is unchanged
let getAllowance = (params: allowance_params, s: storage): list(operation) => {
  // finds owner's account
  let src: account = switch(Big_map.find_opt(params.owner, s.ledger)){
    | None => failwith ("NoAccount"): account
    | Some (acc) => acc
  };
  // gets spender's allowance
  let destAllowance: nat = switch(Map.find_opt(params.spender, src.allowances)){
    | None => failwith ("NoAccount"): nat
    | Some (allowance) => allowance
  };
  // returns transaction with allowance
  [Tezos.transaction(destAllowance, 0tz, params.callback)]: list(operation);
}

// View function that forwards the balance of source to a contract
// Preconditions:
//  None
// Postconditions:
//  The state is unchanged
let getBalance = (params: balance_params, s : storage): list(operation) => {
  let src: account = switch(Big_map.find_opt(params.owner, s.ledger)){
    | None => failwith ("NoAccount"): account
    | Some (acc) => acc
  };
  // returns transaction with balance
  [Tezos.transaction(src.balance, 0tz, params.callback)]: list(operation);
}

// View function that forwards the totalSupply to a contract
// Preconditions:
//  None
// Postconditions:
//  The state is unchanged
let getTotalSupply = (contract: contract (nat), s: storage): list(operation) => {
  // returns transaction with total supply
  // this doesn't compile => [Tezos.transaction(s.totalSupply, 0tz, contr)]: list(operation);
  [Tezos.transaction(s.totalSupply, 0tz, contract)];
}

// Pause the smart contract
// Preconditions:
// Only owner
// Post conditions:
// None
let pause = (s: storage): storage => {
  if(Tezos.source != s.owner){
    failwith ("UnauthorizedOperation"): storage;
  } else {
    { ...s, paused: !s.paused }
  }
}

let main = ((p, s): (action, storage)): (list(operation), storage) => {
  if (s.paused && Tezos.source != s.owner) {
    failwith ("ContractIsPaused"): (list (operation), storage);
  } else {
    switch(p){
    | Transfer (n) => ([]: list(operation), transfer(n[0], n[1], n[2] , s))
    | Approve (n) => ([]: list(operation), approve(n[0], n[1], s))
    | RemoveApproval (n) => ([]: list(operation), removeApproval(n, s))
    | GetAllowance (p) => (getAllowance(p, s), s)
    | GetBalance (p) => (getBalance(p, s), s)
    | GetTotalSupply (n) => (getTotalSupply(n, s), s)
    | Mint (n) => ([]: list(operation), mint(n, s))
    | Burn (n) => ([]: list(operation), burn(n, s))
    | SetBuyPrice (n) => ([]: list (operation), setBuyPrice(n, s))
    | SupplyBuyPool (n) => ([]: list (operation), supplyBuyPool(n, s))
    | Buy (n) => ([]: list (operation), buy(n, s))
    | AddExtra (n) => ([]: list (operation), addExtra(n[0], n[1], s))
    | Pause => ([]: list (operation), pause(s))
  };
  }
}