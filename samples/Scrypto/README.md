# PaymentSplitter

> Note: the instructions here were made for a previous version of Scrypto and are now outdated.

Just like in the physical world, when multiple people come together to build an NFT or a DeFI project they typically want to split the profits from such project in a clear and trustworthy way. The approach to have one entity to trust with the funds to later send to other shareholders simply bears too much risk of one party going rogue and refusing to send the shareholders their stake. This calls for a better way for funds from a project to be managed and split across the different shareholders in the project. The `PaymentSplitter` is a Scrypto blueprint which uses NFTs to authenticate shareholders and allow shareholders to have a clear and trustworthy way of withdrawing their profits from a project when they wish to do so.

## Motivations

The motivation behind this blueprint is to build a `PaymentSplitter` similar to Ethereum's OpenZeppelin [implementation](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/finance/PaymentSplitter.sol) but to build it on Radix using all of the new and exciting concepts and ideas that Radix provides.

## Features
The `PaymentSplitter` blueprint comes with quite a number of features. Such as:

* Allows for an easy way to split funds between multiple different entities.
* Supports XRD as well as other fungible tokens.
* Allows the the admin of the `PaymentSplitter` to disable the addition of new shareholders (to protect current shareholders).
* Allows shareholders to withdraw their owed funds in full or in part.
* Allows shareholders to give up their shares if they choose to.

## Design Details

The `PaymentSplitter` blueprint allows multiple parties to split their shares of funds securely and in a trustless manner. It allows anybody to deposit funds into its components but only allows shareholders to withdraw these funds. As soon as funds are deposited into a `PaymentSplitter` they are split into a number of vaults which are controlled by the `PaymentSplitter`. Each shareholder has their own vault which is associated with the ID of their non-fungible badge and where their tokens are stored. The decision of making each shareholder have their own vault comes from the fact that this brings added security as the funds will be segregated; meaning that given the math is correct, it would be impossible for one shareholder to take the funds of another shareholder. A `PaymentSplitter` may be instantiated to allow for the deposit and splitting of any fungible token. 

There are two main parties in a typical `PaymentSplitter`:
* An Admin: The admin is whoever holds the admin badge. This is typically the instantiator of the splitter but could also be a different person. The admin is given the right to add shareholders to the splitter in the way that they see fit. Once a shareholder has been added to the splitter, they may never be removed again.
* Shareholders: These are people who have shareholder badges and are the entities we want to split the received funds across. Each shareholder is given a non-fungible token which stores information on the amount of shares owned by the shareholder.

While removing a shareholder is not possible in the `PaymentSplitter` blueprint, an admin could later add an disproportionate amount of shares for themselves leading to other shareholders have shares that equate to almost zero. Therefore, the `PaymentSplitter` allows for a locking mechanism which is a mechanism by which the splitter is locked and no more shareholders may be added. This mechanism protects the shareholders from such attacks (provided that the admin agrees to lock the splitter).

The core functionality of the splitter is implemented in the [lib.rs](./src/lib.rs) file in the `PaymentSplitter` struct. This implementation of the `PaymentSplitter` utilizes the new authentication system introduced with v0.4.0 of Scrypto which is a fantastic new system that allows for authentication to be automatically handled when the right badges are present in the auth zone. Despite that, some methods in the `PaymentSplitter` blueprint use the v0.3.0-style authentication system for reasons that will be explained later.

The `PaymentSplitter` component has the following methods and functions:

| Function / Method Name                       | Auth Type      | Intended User    | Description |
| -------------------------------------------- | -------------- | ---------------- | ----------- |
| `instantiate_payment_splitter`               |                |                  | This function instantiates a new `PaymentSplitter` component as well as all of the badges and resources required for it to function correctly. 
| `instantiate_custom_access_payment_splitter` |                |                  | This function instantiates a new `PaymentSplitter` component which has a custom access-rule set for its `add_shareholder` and `lock_splitter` methods.
| `deposit`                                    |                |                  | This method allows for any entity to deposit funds into the `PaymentSplitter` which would be split among the shareholders.
| `add_shareholder`                            | Auth Zone      | Admin            | This is an authenticated method which allows the `PaymentSplitter`'s admin to add a shareholder with a given amount of shares to the splitter. 
| `lock_splitter`                              | Auth Zone      | Admin            | This is an authenticated method which allows the `PaymentSplitter`'s admin to lock the splitter which would allow normal operation of the splitter minus the adding of additional shareholders.
| `withdraw`                                   | Pass By Intent | Shareholder      | This is an authenticated method which allows the `PaymentSplitter`'s shareholders to withdraw all of the funds owed to them from the splitter.
| `withdraw_by_amount`                         | Pass By Intent | Shareholder      | This is an authenticated method which allows the `PaymentSplitter`'s shareholders to withdraw a portion of the funds owed to them from the splitter.
| `withdraw_and_giveup_shares`                 | Pass By Intent | Shareholder      | This is an authenticated method which allows the `PaymentSplitter`'s shareholders to withdraw all of the funds owed to them from the splitter and give up their shares so that they go no share in any future deposit.


Version 0.3.0 of Scrypto introduced the concept of transaction manifests and the transaction worktop which is used to store resources (tokens), buckets, and badges (in the form of `BucketRef`s). Version 0.4.0 introduces an extension to the transaction worktop which is called the "Auth Zone". Similar to how the transaction worktop stores tokens and buckets, the Auth Zone in the main area where `Proof`s (formerly called `BucketRef`s) live to be used in transactions which require them. 

A method which is configured to use the new authorization system would determine whether a call to it is authorized or not depending on whether the badge required in the configuration is present in the Auth Zone or not<sup>*</sup>. If the badge is present then the call to the method is allowed, and vice-versa. 

The `PaymentSplitter` blueprint utilizes this new concept of the Auth Zone for the authorization of a number of its methods. As an example: `add_shareholder`, `lock_splitter` all require an admin badge to work; however, the admin badge is not passed as a proof in the method calls. Instead, if the Auth Zone of the transaction has the admin badge then the caller would be authorized to make the calls. The admin can get their admin badge into their auth zone by calling the `create_proof` method on their account component which returns a proof and stores it in the Auth Zone.

Similar to v0.3.0 of Scrypto where buckets and tokens returned from methods or functions would be automatically put onto the transaction worktop, in v0.4.0 all Proofs returned from function or method calls are put in the Auth Zone automatically.

This approach to authorization makes component authorization very clear and transparent to the caller. As an example, let's say that I'm a potential shareholder and I wish to see who has the power to lock this splitter. In the current version of resim, I can `show` the component state to understand who is allowed to lock the splitter:
```
$ resim show 02729d2037a333cee1734718de58a51c95c1caa52bd1a6e0cb0999
Component: 02729d2037a333cee1734718de58a51c95c1caa52bd1a6e0cb0999
Blueprint: { package_address: 018a10614b7d516906593a01b4cb611e5d47f7804f8d66e6af509c, blueprint_name: "PaymentSplitter" }
Authorization
├─ "lock_splitter" => Protected(ProofRule(Require(StaticResource(03fe2138055411553eb32641ce73e4a436164938745a083f6aa104))))
└─ "add_shareholder" => Protected(ProofRule(Require(StaticResource(03fe2138055411553eb32641ce73e4a436164938745a083f6aa104))))
```

Looking at the above output makes it very clear for a `lock_splitter` method call to succeed, then it is required that a badge with the resource address `03fe2138055411553eb32641ce73e4a436164938745a083f6aa104` is present in the auth zone. Therefore, without needing to read the source code or anything, I now know some information about this splitter. If I wanted do, I can do some more sleuthing to find out if the resource `03fe2138055411553eb32641ce73e4a436164938745a083f6aa104` is mintable or not to determine if more admins may be added to the splitter or not in the future.

In addition to the clarity and transparency of handling auth through the Auth Zone, this method of handling auth allows for complex auth to be set on methods and functions such that they may only be called when a number of conditions are satisfied. As an example, let's say that we have some organization where some method or function requires the approval of a number of people on different levels. This auth system allows us to set an auth rule similar to the following on that method:
```
supervisor_badge & manager_badge & admin_badge & super_admin_badge
```

This would mean that this function or method would only be callable when all of the four required badges are present in the auth zone. The power of this auth system does not stop here; this system also allows us to set rules which dynamically change depending on the state of the component, as an example, we can set the auth system up so that it always requires 50% or more admins to approve an action before it can be taken.

While v0.3.0 of Scrypto required multiple transaction manifest instructions for operations that required badge reuse, this version drastically reduce the amount of instructions needed as for operations handled by the Auth Zone, there is no longer a need to clone the Proof multiple times to preserve a copy of it to use for subsequent operations.

The benefits that this system of auth provides are numerous and are very clear. However, sometimes we need to use the other system of auth which was present in v0.3.0 which is the system where we need to provide a `Proof` (formerly called `BucketRef` in v0.3.0) a method or a function. This is typically the case when the method or function being called does not only require the presence of a certain badge, but it also needs to read the data of the badge to make some kind of determination. 

As an example, for the methods `add_shareholder` and `lock_splitter` the presence of an admin badge in the auth zone is enough to authorize these operations to take place; therefore, auth for these methods is handled through the Auth Zone. However, when it comes to the withdraw methods the presence of a shareholder badge is not enough to withdraw the funds. This is because these methods know that you are a shareholder but they would like to know also know *which* shareholder you are to determine how much funds are owed to you. Therefore, these methods take a `Proof` (formerly called a `BucketRef`) as an argument and based on the resources in that `Proof` they determine your eligibility to withdraw as well as the amount of funds currently available for you.

The two methods of auth that are currently available are not meant to compete with one another in terms of who is best. They're meant to coexist where each of the two methods does its intended job exceptionally well. The `PaymentSplitter` blueprint is a good example of how these two ways of handling authentication can work side by side in a single blueprint and this example helps shine a light on how the decision to use one kind of auth over the other cam about.

### Practical Considerations

In the physical world, when there is a need to split some kind of funds across multiple different people, there is typically a need for multiple people to approve the addition of shareholders to the physical-world contract before they can be added and have funds split across them as well. A key focus of the `PaymentSplitter` blueprint is to be as powerful and flexible as similar contracts can be in the physical world. Therefore, the `PaymentSplitter` has an option for the instantiator of the component to set the access rule that they would like for the addition of shareholders and the locking of the splitter. In simpler terms, the `PaymentSplitter` allow for somebody to say "my manager and his manager have to both sign before a shareholder can be added", or "my manager or his manager need to sign before a shareholder is added", or a more general rule like "5 out of 10 board members need to sign before the shareholder can be added".

The `PaymentSplitter` blueprint allows for such flexibility thanks to the new auth-zone-based authentication system introduced with v0.4.0 and the tons of abstractions that it introduces. The method which allows for the setting of a custom access rule for the `add_shareholder` and `lock_splitter` function is `instantiate_custom_access_payment_splitter`. In fact, the "default" constructor used by the `PaymentSplitter` uses the `instantiate_custom_access_payment_splitter` function with an admin badge it creates on the fly to set the access rule, this is how generic and powerful this function is.

Let's look at a few examples of how this function can be used and the power of it. Let's say that we would like to create a `PaymentSplitter` which requires both a supervisor and admin to be present before a shareholder is added. This `PaymentSplitter` may be instantiated as follows:

```rust
let supervisor_badge: ResourceAddress = ...;
let admin_badge: ResourceAddress = ...;

let payment_splitter: ComponentAddress = PaymentSplitter::instantiate_custom_access_payment_splitter(
    RADIX_TOKEN,
    rule!( require(supervisor_badge) && require(admin_badge) )
);
```

We can do many more powerful things with this concept. Let's say that we want to create a `PaymentSplitter` where a supervisor and manager need to approve on the addition of a new shareholder or the founder needs to approve on that. We can create such a component with the code snippet below:

```rust
let supervisor_badge: ResourceAddress = ...;
let manager_badge: ResourceAddress = ...;

let founder_badge: ResourceAddress = ...;

let payment_splitter: ComponentAddress = PaymentSplitter::instantiate_custom_access_payment_splitter(
    RADIX_TOKEN,
    rule!( (require(supervisor_badge) && require(manager_badge)) || require(founder_badge) )
);
```

We can take this another step further and create `PaymentSplitter`s with a truly crazy and realistic set of rules. Let's say that some entity has a board. 5 board members need to vote on an action before it happens. After the board has voted, the founder needs to also approve of the action before it can happen. If we say that the "action" is adding a new shareholder to our splitter, then this can be created with the following code:

```rust
let board_member_badge: ResourceAddress = ...;

let founder_badge: ResourceAddress = ...;

let payment_splitter: ComponentAddress = PaymentSplitter::instantiate_custom_access_payment_splitter(
    RADIX_TOKEN,
    rule!( (require_n_of(5, board_member_badge) && require(admin_badge)) )
);
```

With the above splitter, only when 5 board members have approved on the addition of a new shareholder and also the founder has approved of that, can that shareholder be added. The new system of auth introduced with v0.4.0 of Scrypto makes real life situations of authentication very easy to model and think about and provides other developers with a great deal of flexibility in terms of how they wish to use the splitter.

## Getting Started

If you wand to try out this blueprint there are three main ways to do that, the first is by using the new transaction model and the transaction manifests, the second is by using an `script.sh` file which runs all of the needed CLI commands for the example, and the third and final method is by typing in the commands manually. 

### Method 1: Using transaction manifest files

Transaction manifests is an extremely cool new feature introduced with v0.3.0 of scrypto which allows for transaction instructions to be written in an external `.rtm` file and run by resim. This feature allows for extremely powerful transactions to be created where multiple different actions take place in a single transaction. 

Lets begin by resetting the radix engine simulator by running the following command:

```sh
resim reset
```

We will need a number of accounts for the examples that we will be running. So, let's create four different accounts using the following command:

```sh
OP1=$(resim new-account)
export ADMIN_PRIV_KEY=$(echo "$OP1" | sed -nr "s/Private key: ([[:alnum:]_]+)/\1/p")
export ADMIN_PUB_KEY=$(echo "$OP1" | sed -nr "s/Public key: ([[:alnum:]_]+)/\1/p")
export ADMIN_ADDRESS=$(echo "$OP1" | sed -nr "s/Account component address: ([[:alnum:]_]+)/\1/p")

OP2=$(resim new-account)
export SHAREHOLDER1_PRIV_KEY=$(echo "$OP2" | sed -nr "s/Private key: ([[:alnum:]_]+)/\1/p")
export SHAREHOLDER1_PUB_KEY=$(echo "$OP2" | sed -nr "s/Public key: ([[:alnum:]_]+)/\1/p")
export SHAREHOLDER1_ADDRESS=$(echo "$OP2" | sed -nr "s/Account component address: ([[:alnum:]_]+)/\1/p")

OP3=$(resim new-account)
export SHAREHOLDER2_PRIV_KEY=$(echo "$OP3" | sed -nr "s/Private key: ([[:alnum:]_]+)/\1/p")
export SHAREHOLDER2_PUB_KEY=$(echo "$OP3" | sed -nr "s/Public key: ([[:alnum:]_]+)/\1/p")
export SHAREHOLDER2_ADDRESS=$(echo "$OP3" | sed -nr "s/Account component address: ([[:alnum:]_]+)/\1/p")

OP4=$(resim new-account)
export SHAREHOLDER3_PRIV_KEY=$(echo "$OP4" | sed -nr "s/Private key: ([[:alnum:]_]+)/\1/p")
export SHAREHOLDER3_PUB_KEY=$(echo "$OP4" | sed -nr "s/Public key: ([[:alnum:]_]+)/\1/p")
export SHAREHOLDER3_ADDRESS=$(echo "$OP4" | sed -nr "s/Account component address: ([[:alnum:]_]+)/\1/p")
```

Now that we have created four different accounts, let's set the first account to be the default account by running the following command:

```sh
resim set-default-account $ADMIN_ADDRESS $ADMIN_PRIV_KEY
```

Let's now publish the package to our local radix engine simulator by running the following:

```sh
resim publish .
```

Now begins the exciting stuff with the new transaction model! The very first radix transaction manifest file that we will using is a file that creates a `PaymentSplitter` component on the local simulator. To run this file, run the following command:

```
$ resim run transactions/component_creation.rtm
```

What we would like to do now is to add shareholders to our `PaymentSplitter` component and to send the shareholders the NFTs that prove their identity. If we decide not to use the new transaction manifest feature we would have twice as many transaction as shareholders we wish to add (i.e. 10 shareholders = 20 transactions) as each shareholder would require a transaction for the minting of the NFT and another transaction for the sending of the NFT. However, with the new transaction model and the introduction of the transaction manifest, we can perform all of that in a single transaction! We have created a radix transaction manifest (rtm) file for you that performs the minting and sending of the tokens all in a single transaction! You may run it through the following command:

```sh
$ resim run ./transactions/adding_shareholders.rtm
```

After the above command runs you can check the balances of the four accounts that we have and you will see that each one of those accounts now have 1 shareholder NFT.

We would now like to test the `PaymentSplitter` to make sure that it does indeed do what we expect it to do. For that purpose we will deposit some funds into the `PaymentSplitter` from account 1 and then we will withdraw them the share of the second account of the funds.

Let's run the transaction manifest file containing the instruction of the depositing of tokens through account 1.
```sh
$ resim run ./transactions/funding_the_splitter.rtm 
```

We may now switch to account 2 and try to withdraw the funds.

```sh
$ resim set-default-account $SHAREHOLDER1_ADDRESS $SHAREHOLDER1_PRIV_KEY
$ resim run ./transactions/withdrawing_owed_amount.rtm 
```

And that's it! Now if you check the balance of account 2 you would see that account 2 now has more XRD as a result of the splitting of profits made.

### Method 2: Automatic method using `script.sh`.

The `script.sh` file included with this blueprint is a quick bash script written during the implementation of the blueprint to allow for a quick way for the package to be redeployed and for accounts to be created quickly and with ease.

When you run the `script.sh` file the following will take place:

* Your resim will be reset so that any accounts previously created or packages deployed are deleted.
* Four new accounts will be created. The script will keep track of their addresses and public keys.
* The package will be published and the script will store the package address.
* The script will call the `new` function on the blueprint to create the `PaymentSplitter` component. When creating the component, a number of resource managers will be created for the badges that blueprint creates.
* The script will then add all four of the addresses that we created as shareholders by calling the `add_shareholder` method on the `PaymentSplitter` component.
* Some XRD will be deposited by account 1 (by calling the `deposit` method) and then later account 2 will attempt to withdraw their share of it (by calling the `withdraw` method).

You do not need to install any additional packages to run this `script.sh` file. This file used the standard python 3 library and packages. So, to run this example just type the following into your command line:

```sh
./build_rtm.sh
./script.sh
```

| NOTE | If you try running the `script.sh` script and you get errors such saying that some addresses were not found, then please run the `build_rtm.sh` script first and then the `script.sh`. |
|------|-----|

## Future Work and Improvements

This is certainly a simple example of what a `PaymentSplitter` might look like if built on Radix using Scrypto where NFTs are used for authentication and tracking of data. Of course, there are a number of improvements that can be made to this blueprint to make it function even better:

* Investigate the upside and downside of introducing a voting mechanism whereby a majority vote from the shareholders is needed before adding an additional shareholder.
* Allowing for a voting mechanism whereby the admin would regain the to add shareholders if a majority of the shareholders agree that the admin should be given this power back.

## Footnotes:

\* -  This is a simplified example of the use of the Auth Zone for the handling of authorization. There are many more things available such as authorization based on the amount of resource, authorization based on multiple Proofs being present, as well as other kinds of authorization supported directly by this system.