// Internal imports
use alexandria_merkle_tree::merkle_tree::{
    Hasher, MerkleTree, pedersen::PedersenHasherImpl, poseidon::PoseidonHasherImpl, MerkleTreeTrait,
    MerkleTreeImpl
};


mod regular_call_merkle_tree_pedersen {
    // Internal imports
    use alexandria_merkle_tree::merkle_tree::{
        Hasher, MerkleTree, pedersen::PedersenHasherImpl, MerkleTreeTrait,
    };
    #[test]
    #[available_gas(2000000)]
    fn regular_call_merkle_tree_pedersen_test() {
        // [Setup] Merkle tree.
        let mut merkle_tree: MerkleTree<Hasher> = MerkleTreeTrait::new();
        let root = 0x15ac9e457789ef0c56e5d559809e7336a909c14ee2511503fa7af69be1ba639;
        let leaf = 0x1;
        let valid_proof = array![
            0x2, 0x68ba2a188dd231112c1cb5aaa5d18be6d84f6c8683e5c3a6638dee83e727acc
        ]
            .span();
        let leaves = array![0x1, 0x2, 0x3];

        // [Assert] Compute merkle root.
        let computed_root = merkle_tree.compute_root(leaf, valid_proof);
        assert_eq!(computed_root, root, "compute valid root failed");

        // [Assert] Compute merkle proof.
        let mut input_leaves = leaves;
        let index = 0;
        let computed_proof = merkle_tree.compute_proof(input_leaves, index);
        assert_eq!(computed_proof, valid_proof, "compute valid proof failed");

        // [Assert] Verify a valid proof.
        let result = merkle_tree.verify(root, leaf, valid_proof);
        assert!(result, "verify valid proof failed");

        // [Assert] Verify an invalid proof.
        let invalid_proof = array![
            0x2 + 1, 0x68ba2a188dd231112c1cb5aaa5d18be6d84f6c8683e5c3a6638dee83e727acc
        ]
            .span();
        let result = merkle_tree.verify(root, leaf, invalid_proof);
        assert!(!result, "verify invalid proof failed");

        // [Assert] Verify a valid proof with an invalid leaf.
        let invalid_leaf = 0x1 + 1;
        let result = merkle_tree.verify(root, invalid_leaf, valid_proof);
        assert!(!result, "wrong result");
    }
}

#[test]
#[available_gas(2000000)]
fn merkle_tree_pedersen_test() {
    // [Setup] Merkle tree.
    let mut merkle_tree: MerkleTree<Hasher> = MerkleTreeImpl::<_, PedersenHasherImpl>::new();
    let root = 0x15ac9e457789ef0c56e5d559809e7336a909c14ee2511503fa7af69be1ba639;
    let leaf = 0x1;
    let valid_proof = array![0x2, 0x68ba2a188dd231112c1cb5aaa5d18be6d84f6c8683e5c3a6638dee83e727acc]
        .span();
    let leaves = array![0x1, 0x2, 0x3];

    // [Assert] Compute merkle root.
    let computed_root = MerkleTreeImpl::<
        _, PedersenHasherImpl
    >::compute_root(ref merkle_tree, leaf, valid_proof);
    assert_eq!(computed_root, root, "compute valid root failed");

    // [Assert] Compute merkle proof.
    let mut input_leaves = leaves;
    let index = 0;
    let computed_proof = MerkleTreeImpl::<
        _, PedersenHasherImpl
    >::compute_proof(ref merkle_tree, input_leaves, index);
    assert_eq!(computed_proof, valid_proof, "compute valid proof failed");

    // [Assert] Verify a valid proof.
    let result = MerkleTreeImpl::<
        _, PedersenHasherImpl
    >::verify(ref merkle_tree, root, leaf, valid_proof);
    assert!(result, "verify valid proof failed");

    // [Assert] Verify an invalid proof.
    let invalid_proof = array![
        0x2 + 1, 0x68ba2a188dd231112c1cb5aaa5d18be6d84f6c8683e5c3a6638dee83e727acc
    ]
        .span();
    let result = MerkleTreeImpl::<
        _, PedersenHasherImpl
    >::verify(ref merkle_tree, root, leaf, invalid_proof);
    assert!(!result, "verify invalid proof failed");

    // [Assert] Verify a valid proof with an invalid leaf.
    let invalid_leaf = 0x1 + 1;
    let result = MerkleTreeImpl::<
        _, PedersenHasherImpl
    >::verify(ref merkle_tree, root, invalid_leaf, valid_proof);
    assert!(!result, "wrong result");
}

#[test]
#[available_gas(50000000000)]
fn merkle_tree_poseidon_test() {
    // [Setup] Merkle tree.
    let mut merkle_tree: MerkleTree<Hasher> = MerkleTreeImpl::<_, PoseidonHasherImpl>::new();
    let root = 0x48924a3b2a7a7b7cc1c9371357e95e322899880a6534bdfe24e96a828b9d780;
    let leaf = 0x1;
    let valid_proof = array![0x2, 0x338eb608d7e48306d01f5a8d4275dd85a52ba79aaf7a1a7b35808ba573c3669]
        .span();
    let leaves = array![0x1, 0x2, 0x3];

    // [Assert] Compute merkle root.
    let computed_root = MerkleTreeImpl::<
        _, PoseidonHasherImpl
    >::compute_root(ref merkle_tree, leaf, valid_proof);
    assert_eq!(computed_root, root, "compute valid root failed");

    // [Assert] Compute merkle proof.
    let mut input_leaves = leaves;
    let index = 0;
    let computed_proof = MerkleTreeImpl::<
        _, PoseidonHasherImpl
    >::compute_proof(ref merkle_tree, input_leaves, index);
    assert_eq!(computed_proof, valid_proof, "compute valid proof failed");

    // [Assert] Verify a valid proof.
    let result = MerkleTreeImpl::<
        _, PoseidonHasherImpl
    >::verify(ref merkle_tree, root, leaf, valid_proof);
    assert!(result, "verify valid proof failed");

    // [Assert] Verify an invalid proof.
    let invalid_proof = array![
        0x2 + 1, 0x68ba2a188dd231112c1cb5aaa5d18be6d84f6c8683e5c3a6638dee83e727acc
    ]
        .span();
    let result = MerkleTreeImpl::<
        _, PoseidonHasherImpl
    >::verify(ref merkle_tree, root, leaf, invalid_proof);
    assert!(!result, "verify invalid proof failed");

    // [Assert] Verify a valid proof with an invalid leaf.
    let invalid_leaf = 0x1 + 1;
    let result = MerkleTreeImpl::<
        _, PoseidonHasherImpl
    >::verify(ref merkle_tree, root, invalid_leaf, valid_proof);
    assert!(!result, "wrong result");
}
