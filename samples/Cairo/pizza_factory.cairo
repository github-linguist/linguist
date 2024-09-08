use starknet::ContractAddress;

#[starknet::interface]
pub trait IPizzaFactory<TContractState> {
    fn increase_pepperoni(ref self: TContractState, amount: u32);
    fn increase_pineapple(ref self: TContractState, amount: u32);
    fn get_owner(self: @TContractState) -> ContractAddress;
    fn change_owner(ref self: TContractState, new_owner: ContractAddress);
    fn make_pizza(ref self: TContractState);
    fn count_pizza(self: @TContractState) -> u32;
}

#[starknet::contract]
pub mod PizzaFactory {
    use super::IPizzaFactory;
    use starknet::ContractAddress;
    use starknet::get_caller_address;

    #[storage]
    pub struct Storage {
        pepperoni: u32,
        pineapple: u32,
        owner: ContractAddress,
        pizzas: u32
    }

    #[constructor]
    fn constructor(ref self: ContractState, owner: ContractAddress) {
        self.pepperoni.write(10);
        self.pineapple.write(10);
        self.owner.write(owner);
    }


    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        PizzaEmission: PizzaEmission
    }

    #[derive(Drop, starknet::Event)]
    pub struct PizzaEmission {
        pub counter: u32
    }

    #[abi(embed_v0)]
    impl PizzaFactoryimpl of super::IPizzaFactory<ContractState> {
        fn increase_pepperoni(ref self: ContractState, amount: u32) {
            assert!(amount != 0, "Amount cannot be 0");
            self.pepperoni.write(self.pepperoni.read() + amount);
        }

        fn increase_pineapple(ref self: ContractState, amount: u32) {
            assert!(amount != 0, "Amount cannot be 0");
            self.pineapple.write(self.pineapple.read() + amount);
        }

        fn make_pizza(ref self: ContractState) {
            assert!(self.pepperoni.read() > 0, "Not enough pepperoni");
            assert!(self.pineapple.read() > 0, "Not enough pineapple");

            let caller: ContractAddress = get_caller_address();
            let owner: ContractAddress = self.get_owner();

            assert!(caller == owner, "Only the owner can make pizza");

            self.pepperoni.write(self.pepperoni.read() - 1);
            self.pineapple.write(self.pineapple.read() - 1);
            self.pizzas.write(self.pizzas.read() + 1);

            self.emit(PizzaEmission { counter: self.pizzas.read() });
        }

        fn get_owner(self: @ContractState) -> ContractAddress {
            self.owner.read()
        }

        fn change_owner(ref self: ContractState, new_owner: ContractAddress) {
            self.set_owner(new_owner);
        }

        fn count_pizza(self: @ContractState) -> u32 {
            self.pizzas.read()
        }
    }

    #[generate_trait]
    pub impl InternalImpl of InternalTrait {
        fn set_owner(ref self: ContractState, new_owner: ContractAddress) {
            let caller: ContractAddress = get_caller_address();
            assert!(caller == self.get_owner(), "Only the owner can set ownership");

            self.owner.write(new_owner);
        }
    }
}
