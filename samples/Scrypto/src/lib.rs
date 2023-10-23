use scrypto::prelude::*;

#[derive(NonFungibleData, ScryptoSbor)]
struct Shareholder {
    /// A struct field which defines the amount of shares owned by this shareholder
    amount_of_shares: Decimal,
}

#[blueprint]
mod payment_splitter {
    enable_method_auth! {
        roles {
            admin => updatable_by: [];
        },
        methods {
            add_shareholder => restrict_to: [admin];
            lock_splitter => restrict_to: [admin];
            withdraw => PUBLIC;
            withdraw_of_amount => PUBLIC;
            withdraw_and_giveup_shares => PUBLIC;
            deposit => PUBLIC;
        }
    }
    /// A PaymentSplitter is a Scrypto blueprint which allows for a way for funds to be distributed among shareholders
    /// in a project depending on the amount of shares that each of the shareholders own.
    struct PaymentSplitter {
        /// The payment splitter can be used to split payments any fungible token, not necessarily XRD. This state
        /// variable defines the resource address of the token that the payment splitter will accept.
        accepted_token_resource_address: ResourceAddress,

        /// When a shareholder is added to the PaymentSplitter, an NFT is minted for this shareholder to keep track of
        /// the amount of shares that they own and to authenticate the shareholders when they wish to withdraw their
        /// funds from the component.
        shareholder_badge_resource_manager: ResourceManager,

        /// The vaults which will be used to store the funds owed to each of the shareholders. This is a hashmap which
        /// maps the non-fungible-id of the `shareholder_badge` to their respective vault. Each shareholder gets their
        /// separate vault in order to eliminate any possibility of contamination of funds.
        vaults: HashMap<NonFungibleLocalId, Vault>,

        /// The PaymentSplitter allows shareholders who are no longer interested in their share to withdraw their funds
        /// and give up their shareholder badges. The shareholder's badge gets burned and there is a need for us to
        /// remove their vault from the `vaults` HashMap. However, vaults are permanent storage; this means that a vault
        /// may never be deleted or removed. So what can we do now with the vault of the shareholder who's giving up
        /// their shares? Well what we do is that we move it to a vector of dead vaults that are no longer being used.
        dead_vaults: Vec<Vault>,

        /// A state variable which controls if the payment splitter is locked or not. Once is a payment splitter is
        /// locked, it may not be unlocked again. The locking of PaymentSplitters means that no more shareholders can
        /// be added to the PaymentSplitter thus protecting the existing shareholders. The locking of payment splitters
        /// is an optional feature built into the payment splitter for added protection.
        is_locked: bool,

        /// This is an accumulator that keeps track of the total amount of shares that are owned by all of the
        /// shareholders. This is mainly used when we're trying to calculate how much of a given token is a shareholder
        /// owed.
        total_amount_of_shares: Decimal,
    }

    impl PaymentSplitter {
        /// Creates a new payment splitter component.
        ///
        /// This function creates a new PaymentSplitter component that splits payment of a given token among
        /// shareholders in proportion to the amount of shares that they own. The payment splitter does not need to use
        /// XRD tokens, it can use any token that the instantiator desires which may be configured through the argument
        /// `accepted_token_resource_address`.
        ///
        /// This function performs a number of checks before the PaymentSplitter component is created:
        ///
        /// * **Check 1:** Checks to ensure that the `accepted_token_resource_address` is of a fungible token.
        ///
        /// # Arguments
        ///
        /// * `accepted_token_resource_address` (ResourceAddress) - The resource address of the token that this payment
        /// splitter will perform splitting on.
        ///
        /// # Returns:
        ///
        /// This function returns a tuple of (ComponentAddress, Bucket), where:
        ///
        /// * `ComponentAddress` - The address of the `PaymentSplitter` component just created.
        /// * `Bucket` - A bucket containing the admin badge.
        pub fn instantiate_payment_splitter(
            accepted_token_resource_address: ResourceAddress,
        ) -> (Global<PaymentSplitter>, FungibleBucket) {
            // Loading in the resource manager for the provided resource address
            let accepted_token_resource_manager =
                ResourceManager::from_address(accepted_token_resource_address);

            match accepted_token_resource_manager.resource_type() {
                ResourceType::NonFungible { id_type: _ } => {
                    panic!("[Instantiation]: PaymentSplitters can't be made to split payments of NFTs.")
                }
                _ => {}
            }

            // Creating the admin badge which will allow for adding shareholders and locking of the payment splitter
            let admin_badge = ResourceBuilder::new_fungible(OwnerRole::None)
                .divisibility(DIVISIBILITY_NONE)
                .metadata(metadata!(
                    init {
                        "name" => "Admin Badge".to_owned(), locked;
                        "description" => 
                        "This is a PaymentSplitter admin badge used to authenticate the admin.".to_owned(), locked;
                    }
                ))
                .mint_initial_supply(1);

            // Creating the component itself through the `instantiate_custom_access_payment_splitter` function on the
            // blueprint which allows for the creation of payment-splitters which have custom access rules on them
            let payment_splitter = Self::instantiate_custom_access_payment_splitter(
                accepted_token_resource_address,
                rule!(require(admin_badge.resource_address())),
            );

            return (payment_splitter, admin_badge);
        }

        /// Creates a new payment splitter component.
        ///
        /// This function creates a new PaymentSplitter component that splits payment of a given token among
        /// shareholders in proportion to the amount of shares that they own. The payment splitter does not need to use
        /// XRD tokens, it can use any token that the instantiator desires which may be configured through the argument
        /// `accepted_token_resource_address`.
        ///
        /// A key piece of functionality which might be needed by users of the payment splitter is having their own auth
        /// rule which governs how shareholders are added to the splitter. As an example, say that you would like to
        /// create a payment splitter which requires three badges to be present in order to allow the addition of more
        /// shareholders. Then, you can use this instantiation function to create a payment splitter which is configured
        /// to perform that.
        ///
        /// This function performs a number of checks before the PaymentSplitter component is created:
        ///
        /// * **Check 1:** Checks to ensure that the `accepted_token_resource_address` is of a fungible token.
        ///
        /// # Arguments
        ///
        /// * `accepted_token_resource_address` (ResourceAddress) - The resource address of the token that this payment
        /// splitter will perform splitting on.
        /// * `withdraw_and_lock_rule` (AccessRule) - This is an AccessRule defines the access rule for two main methods
        /// in the component: `add_shareholder` and `lock_splitter`.
        ///
        /// # Returns:
        ///
        /// This function returns a tuple of (ComponentAddress, Bucket), where:
        ///
        /// * `ComponentAddress` - The address of the `PaymentSplitter` component just created.
        /// * `Bucket` - A bucket containing the admin badge.
        pub fn instantiate_custom_access_payment_splitter(
            accepted_token_resource_address: ResourceAddress,
            withdraw_and_lock_rule: AccessRule,
        ) -> Global<PaymentSplitter> {
            // Loading in the resource manager for the provided resource address
            let accepted_token_resource_manager =
                ResourceManager::from_address(accepted_token_resource_address);
            match accepted_token_resource_manager.resource_type() {
                ResourceType::NonFungible { id_type: _ } => {
                    panic!("[Instantiation]: PaymentSplitters can't be made to split payments of NFTs.")
                }
                _ => {}
            }

            // Allocating a ComponentAddress to use as an Actor Virtual Badge to mint/burn shareholder badge.
            let (address_reservation, component_address) =
                Runtime::allocate_component_address(PaymentSplitter::blueprint_id());

            // Creating the shareholder NFT which we will be using as a badge to authenticate shareholders and setting
            // the auth of the shareholder badge such that it can be moved around but can only be minted and burned by
            // the internal admin badge.
            let shareholder_badge: ResourceManager = ResourceBuilder::new_ruid_non_fungible::<
                Shareholder,
            >(OwnerRole::None)
            .metadata(metadata!(
                init {
                    "name" => "Shareholder Badge".to_owned(), locked;
                    "description" =>
                    "A non-fungible-token used to authenticate shareholders.".to_owned(), locked;
                }
            ))
            .mint_roles(mint_roles!(
                minter => rule!(require(global_caller(component_address)));
                minter_updater => rule!(deny_all);
            ))
            .burn_roles(burn_roles!(
                burner => rule!(require(global_caller(component_address)));
                burner_updater => rule!(deny_all);
            ))
            .create_with_no_initial_supply();

            let payment_splitter = Self {
                accepted_token_resource_address: accepted_token_resource_address,
                shareholder_badge_resource_manager: shareholder_badge,
                vaults: HashMap::new(),
                dead_vaults: Vec::new(),
                is_locked: false,
                total_amount_of_shares: dec!("0"),
            }
            .instantiate()
            .prepare_to_globalize(OwnerRole::None)
            .with_address(address_reservation)
            .roles(roles!(
                admin => withdraw_and_lock_rule;
            ))
            .globalize();

            return payment_splitter;
        }

        /// Adds a shareholder to the PaymentSplitter
        ///
        /// This method is used to add a shareholder to the PaymentSplitter with a given amount of shares. This is an
        /// authenticated method that only an admin with an admin badge can access and make use of. When the admin adds
        /// a shareholder, a new shareholder NFT gets minted which specifies the amount of shares owned by this
        /// shareholder.
        ///
        /// This method performs a number of checks before a shareholder is added:
        ///
        /// * **Check 1:** Checks that the PaymentSplitter is not locked.
        ///
        /// The authorization check is not handled by this method, it's handled on a component level so there is no
        /// need for this method to check for authorization.
        ///
        /// # Arguments:
        ///
        /// * `amount_of_shares` (Decimal) - A decimal of the amount of shares owned by this new shareholder.
        ///
        /// # Returns:
        ///
        /// `Bucket` - A bucket of the shareholder's badge.
        pub fn add_shareholder(&mut self, amount_of_shares: Decimal) -> Bucket {
            // Checking whether the payment splitter is locked or not.
            assert!(
                !self.is_locked,
                "[Add Shareholder]: Shareholders can not be added when the payment splitter is locked."
            );
            info!("Adding a new shareholder with {} shares", amount_of_shares);

            let shareholder_badge: Bucket = self
                .shareholder_badge_resource_manager
                .mint_ruid_non_fungible(Shareholder {
                    amount_of_shares: amount_of_shares,
                });

            let non_fungible_id: NonFungibleLocalId =
                shareholder_badge.as_non_fungible().non_fungible_local_id();

            // Creating a vault for the shareholder
            self.vaults.insert(
                non_fungible_id,
                Vault::new(self.accepted_token_resource_address),
            );
            self.total_amount_of_shares.checked_add(amount_of_shares);

            // Returning the shareholder back to the method caller
            return shareholder_badge;
        }

        /// Locks the PaymentSplitter so that no more shareholders can be added.
        ///
        /// This method is an authenticated method that can only be called by a PaymentSplitter admin to lock the
        /// splitter such that no more shareholders can be added to the splitter.
        pub fn lock_splitter(&mut self) {
            info!("[Lock Splitter]: Locking the Splitter");
            self.is_locked = true;
        }

        /// Withdraws the funds owed to the shareholder from the PaymentSplitter
        ///
        /// This is a manually authenticated method which takes in a `Proof` and ensures that it is a valid `Proof` of
        /// a shareholder from this payment splitter. Once we've verified the authenticity of the `Proof`, the funds
        /// that are owed to the shareholder are withdrawn, put into a bucket, and returned.
        ///
        /// This method performs a number of checks before withdrawing the funds:
        ///
        /// * **Check 1:** Checks to ensure that a shareholder badge was provided.
        /// * **Check 2:** Checks to ensure that a valid quantity of the shareholder badge was provided. A valid amount
        /// is any amount more than 1.
        ///
        /// # Arguments:
        ///
        /// * `shareholder_badge` (Proof) - A `Proof` containing a single shareholder badge
        ///
        /// # Returns:
        ///
        /// * `Bucket` - A bucket of the tokens owed to the shareholder
        pub fn withdraw(&mut self, shareholder_badge: Proof) -> Bucket {
            // Checking the type and quantity of the resource in the proof
            let shareholder_badge =
                shareholder_badge.check(self.shareholder_badge_resource_manager.address());

            // At this point we have verified that the caller has presented a valid shareholder badge. We may now begin
            // checking the NonFungibleId of the badge and then withdraw the amount owed to them
            let non_fungible: NonFungible<Shareholder> = shareholder_badge
                .as_non_fungible()
                .non_fungible::<Shareholder>();
            let non_fungible_id: &NonFungibleLocalId = non_fungible.local_id();

            // Withdrawing the funds associated with the `non_fungible_id` from the vaults, into a bucket, and returning
            // it to them.
            return self.vaults.get_mut(&non_fungible_id).unwrap().take_all();
        }

        /// Withdraws a specific amount of the funds owed to the shareholder from the PaymentSplitter
        ///
        /// This is a manually authenticated method which takes in a `Proof` and ensures that it is a valid `Proof` of
        /// a shareholder from this payment splitter. Once we've verified the authenticity of the `Proof`, the funds
        /// that are owed to the shareholder are withdrawn, put into a bucket, and returned.
        ///
        /// This method performs a number of checks before withdrawing the funds:
        ///
        /// * **Check 1:** Checks to ensure that a shareholder badge was provided.
        /// * **Check 2:** Checks to ensure that a valid quantity of the shareholder badge was provided. A valid amount
        /// is any amount more than 1.
        ///
        /// # Arguments:
        ///
        /// * `shareholder_badge` (Proof) - A `Proof` containing a single shareholder badge
        ///
        /// # Returns:
        ///
        /// * `Bucket` - A bucket of the tokens owed to the shareholder
        pub fn withdraw_of_amount(&mut self, amount: Decimal, shareholder_badge: Proof) -> Bucket {
            // Checking the type and quantity of the resource in the proof
            let shareholder_badge =
                shareholder_badge.check(self.shareholder_badge_resource_manager.address());

            // At this point we have verified that the caller has presented a valid shareholder badge. We may now begin
            // checking the NonFungibleId of the badge and then withdraw the amount owed to them
            let non_fungible: NonFungible<Shareholder> = shareholder_badge
                .as_non_fungible()
                .non_fungible::<Shareholder>();
            let non_fungible_id: &NonFungibleLocalId = non_fungible.local_id();

            // Getting the vault where the funds are stored and checking if enough funds exist for the withdrawal
            let vault: &mut Vault = self.vaults.get_mut(&non_fungible_id).unwrap();
            assert!(
                vault.amount() >= amount,
                "[Withdrawal]: Can't withdraw {} tokens since only {} are available.",
                amount,
                vault.amount()
            );

            // Performing the withdraw
            return vault.take(amount);
        }

        /// Withdraws the all of the funds owed to the shareholder from the PaymentSplitter and burns their shareholder
        /// badge
        ///
        /// This is a manually authenticated method which takes in a `Bucket` and ensures that it is a valid `Bucket` of
        /// a shareholder from this payment splitter. Once we've verified the authenticity of the `Bucket`, the badge
        /// is burned, and it's associated vault is moved to the space of dead vaults. The remaining funds owed to the
        /// shareholder is returned to them in a `Bucket`. After this method, the caller will lose their shareholder
        /// badge and won't be entitled to any share of the payments in the future.
        ///
        /// This method performs a number of checks before withdrawing the funds:
        ///
        /// * **Check 1:** Checks to ensure that a shareholder badge was provided.
        /// * **Check 2:** Checks to ensure that a valid quantity of the shareholder badge was provided. A valid amount
        /// is any amount more than 1.
        ///
        /// # Arguments:
        ///
        /// * `shareholder_badge` (Bucket) - A `Bucket` containing a single shareholder badge
        ///
        /// # Returns:
        ///
        /// * `Bucket` - A bucket of the tokens owed to the shareholder
        pub fn withdraw_and_giveup_shares(&mut self, shareholder_badge: Bucket) -> Bucket {
            // Checking the type and quantity of the resource in the bucket
            assert_eq!(
                shareholder_badge.resource_address(),
                self.shareholder_badge_resource_manager.address(),
                "[Withdraw Give-up Shares]: Invalid badge type presented"
            );
            assert!(
                shareholder_badge.amount() == dec!("1"),
                "[Withdraw Give-up Shares]: Invalid badge amount presented"
            );

            // Getting the amount of shares that the shareholder owns
            let non_fungible: NonFungible<Shareholder> = shareholder_badge
                .as_non_fungible()
                .non_fungible::<Shareholder>();
            let non_fungible_id: &NonFungibleLocalId = non_fungible.local_id();

            // Withdrawing the shareholder's share of tokens from the splitter
            let shareholder_token_share: Bucket =
                self.vaults.get_mut(&non_fungible_id).unwrap().take_all();

            // Loading up the resource manager of the shareholders NFT
            let shareholder_resource_manager = self.shareholder_badge_resource_manager;

            // Loading up the shareholder object from the non_fungible_id and subtracting its share from the total
            // amount of shares
            let shareholder: Shareholder =
                shareholder_resource_manager.get_non_fungible_data(&non_fungible_id);
            self.total_amount_of_shares
                .checked_sub(shareholder.amount_of_shares);

            // Burning the shareholder NFT
            shareholder_badge.burn();

            // Since there is no way for us to remove the vault, so we just put the vault in the dead vaults bucket and
            // stop worrying about it. Once the vault is there, no funds will be put in that vault.
            let vault: Vault = self.vaults.remove(&non_fungible_id).unwrap();
            self.dead_vaults.push(vault);

            return shareholder_token_share;
        }

        /// Deposits funds into the payment splitter.
        ///
        /// This method calculates the amount of funds owed to each of the shareholders out of the deposited amount and
        /// then deposits that amount into their respective vault.
        ///
        /// This method performs a number of checks before the deposit goes through:
        ///
        /// * **Check 1:** Checks to ensure that the resource address of the token in the bucket is of the
        /// `accepted_token_resource_address`.
        ///
        /// # Arguments:
        ///
        /// * `bucket` (Bucket) - A bucket of the tokens to deposit into the payment splitter
        ///
        /// # Returns:
        ///
        /// * `Bucket` - A bucket of the remaining tokens that were not split among the shareholders as a result of
        /// rounding issues.
        pub fn deposit(&mut self, mut bucket: Bucket) -> Bucket {
            // Checking if the bucket of tokens is of the same type as the accepted token or not
            assert_eq!(
                bucket.resource_address(),
                self.accepted_token_resource_address,
                "[Deposit]: Only deposits of {:?} are allowed",
                self.accepted_token_resource_address
            );

            // Iterating over the shareholder NonFungibleIds, determining how much they're owed, and putting that into
            // their vault.
            let shareholder_resource_manager = self.shareholder_badge_resource_manager;
            for (non_fungible_id, vault) in &mut self.vaults {
                let shareholder: Shareholder =
                    shareholder_resource_manager.get_non_fungible_data(&non_fungible_id);
                let amount_owed: Decimal = shareholder
                    .amount_of_shares
                    .checked_mul(bucket.amount())
                    .and_then(|d| d.checked_div(self.total_amount_of_shares))
                    .unwrap();
                // shareholder.amount_of_shares * bucket.amount() / self.total_amount_of_shares;
                vault.put(bucket.take(amount_owed));
            }

            return bucket;
        }
    }
}
