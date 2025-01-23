import FungibleToken from 0xFUNGIBLETOKENADDRESS
import FlowToken from 0xFLOWTOKENADDRESS

pub contract FlowFees {

    // Event that is emitted when tokens are deposited to the fee vault
    pub event TokensDeposited(amount: UFix64)

    // Event that is emitted when tokens are withdrawn from the fee vault
    pub event TokensWithdrawn(amount: UFix64)

    // Event that is emitted when fees are deducted
    pub event FeesDeducted(amount: UFix64, inclusionEffort: UFix64, executionEffort: UFix64)

    // Event that is emitted when fee parameters change
    pub event FeeParametersChanged(surgeFactor: UFix64, inclusionEffortCost: UFix64, executionEffortCost: UFix64)

    // Private vault with public deposit function
    access(self) var vault: @FlowToken.Vault

    pub fun deposit(from: @FungibleToken.Vault) {
        let from <- from as! @FlowToken.Vault
        let balance = from.balance
        self.vault.deposit(from: <-from)
        emit TokensDeposited(amount: balance)
    }

    /// Get the balance of the Fees Vault
    pub fun getFeeBalance(): UFix64 {
        return self.vault.balance
    }

    pub resource Administrator {
        // withdraw
        //
        // Allows the administrator to withdraw tokens from the fee vault
        pub fun withdrawTokensFromFeeVault(amount: UFix64): @FungibleToken.Vault {
            let vault <- FlowFees.vault.withdraw(amount: amount)
            emit TokensWithdrawn(amount: amount)
            return <-vault
        }

        /// Allows the administrator to change all the fee parameters at once
        pub fun setFeeParameters(surgeFactor: UFix64, inclusionEffortCost: UFix64, executionEffortCost: UFix64) {
            let newParameters = FeeParameters(surgeFactor: surgeFactor, inclusionEffortCost: inclusionEffortCost, executionEffortCost: executionEffortCost)
            FlowFees.setFeeParameters(newParameters)
        }

        /// Allows the administrator to change the fee surge factor
        pub fun setFeeSurgeFactor(_ surgeFactor: UFix64) {
            let oldParameters = FlowFees.getFeeParameters()
            let newParameters = FeeParameters(surgeFactor: surgeFactor, inclusionEffortCost: oldParameters.inclusionEffortCost, executionEffortCost: oldParameters.executionEffortCost)
            FlowFees.setFeeParameters(newParameters)
        }
    }

    /// A struct holding the fee parameters needed to calculate the fees
    pub struct FeeParameters {
        /// The surge factor is used to make transaction fees respond to high loads on the network
        pub var surgeFactor: UFix64
        /// The FLOW cost of one unit of inclusion effort. The FVM is responsible for metering inclusion effort.
        pub var inclusionEffortCost: UFix64
        /// The FLOW cost of one unit of execution effort. The FVM is responsible for metering execution effort.
        pub var executionEffortCost: UFix64

        init(surgeFactor: UFix64, inclusionEffortCost: UFix64, executionEffortCost: UFix64){
            self.surgeFactor = surgeFactor
            self.inclusionEffortCost = inclusionEffortCost
            self.executionEffortCost = executionEffortCost
        }
    }

    /// Called when a transaction is submitted to deduct the fee
    /// from the AuthAccount that submitted it
    pub fun deductTransactionFee(_ acct: AuthAccount, inclusionEffort: UFix64, executionEffort: UFix64) {
        var feeAmount = self.computeFees(inclusionEffort: inclusionEffort, executionEffort: executionEffort)

        if feeAmount == UFix64(0) {
            // If there are no fees to deduct, do not continue, 
            // so that there are no unnecessarily emitted events
            return
        }

        let tokenVault = acct.borrow<&FlowToken.Vault>(from: /storage/flowTokenVault)
            ?? panic("Unable to borrow reference to the default token vault")

        
        if feeAmount > tokenVault.balance {
            // In the future this code path will never be reached, 
            // as payers that are under account minimum balance will not have their transactions included in a collection
            //
            // Currently this is not used to fail the transaction (as that is the responsibility of the minimum account balance logic),
            // But is used to reduce the balance of the vault to 0.0, if the vault has less available balance than the transaction fees. 
            feeAmount = tokenVault.balance
        }
        
        let feeVault <- tokenVault.withdraw(amount: feeAmount)
        self.vault.deposit(from: <-feeVault)

        // The fee calculation can be reconstructed using the data from this event and the FeeParameters at the block when the event happened
        emit FeesDeducted(amount: feeAmount, inclusionEffort: inclusionEffort, executionEffort: executionEffort)
    }

    pub fun getFeeParameters(): FeeParameters {
        return self.account.copy<FeeParameters>(from: /storage/FlowTxFeeParameters) ?? panic("Error getting tx fee parameters. They need to be initialized first!")
    }

    access(self) fun setFeeParameters(_ feeParameters: FeeParameters) {
        // empty storage before writing new FeeParameters to it
        self.account.load<FeeParameters>(from: /storage/FlowTxFeeParameters)
        self.account.save(feeParameters,to: /storage/FlowTxFeeParameters)
        emit FeeParametersChanged(surgeFactor: feeParameters.surgeFactor, inclusionEffortCost: feeParameters.inclusionEffortCost, executionEffortCost: feeParameters.executionEffortCost)
    }

    
    // compute the transaction fees with the current fee parameters and the given inclusionEffort and executionEffort
    pub fun computeFees(inclusionEffort: UFix64, executionEffort: UFix64): UFix64 {
        let params = self.getFeeParameters()
        
        let totalFees = params.surgeFactor * ( inclusionEffort * params.inclusionEffortCost + executionEffort * params.executionEffortCost )
        return totalFees
    }

    init(adminAccount: AuthAccount) {
        // Create a new FlowToken Vault and save it in storage
        self.vault <- FlowToken.createEmptyVault() as! @FlowToken.Vault

        let admin <- create Administrator()
        adminAccount.save(<-admin, to: /storage/flowFeesAdmin)
    }
}