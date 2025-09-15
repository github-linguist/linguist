use dep::std;

fn main(root : pub Field, index : Field, hash_path : [Field; 2], secret: Field, priv_key: Field, proposalId: pub Field, vote: pub u8) -> pub Field {
    let note_commitment = std::hash::pedersen([priv_key, secret]);
    let nullifier = std::hash::pedersen([root, priv_key, proposalId]);

    let is_member = std::merkle::check_membership(root, note_commitment[0], index, hash_path);
    assert(is_member == 1);
    assert(vote <= 1);

    nullifier[0]
}