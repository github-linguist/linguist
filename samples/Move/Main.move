address 0x1 {
module Main {

    // added a dependency here!
    use 0x1::Signer;
    use 0x1::Vector;

    struct Item has store, drop {}

    struct Main has key, store {
        items: vector<Item>
    }


    /// get collection size
    /// mind keyword acquires!
    public fun size(account: &signer): u64 acquires Main {
        let owner = Signer::address_of(account);
        let collection = borrow_global<Main>(owner);

        Vector::length(&collection.items)
    }

    fun type_test() {
        let _bool: bool = true;
        let _bool: bool = false;
        let _u8: u8 = 1;
        let _u64: u64 = 1;
        let _u128: u128 = 1;
    }
}
}