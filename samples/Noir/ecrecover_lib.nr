use dep::std;
 
mod secp256k1;

fn ecrecover(
    pub_key_x: [u8; 32],
    pub_key_y: [u8; 32],
    signature: [u8; 64], // clip v value
    hashed_message: [u8; 32]
) -> pub Field {
    let key = secp256k1::PubKey::from_xy(pub_key_x, pub_key_y);

    assert(key.verify_sig(signature, hashed_message) == true);
    let addr = key.to_eth_address();

    addr
}

#[test]
fn test_ecrecover() {
    let pub_key_x = [131, 24, 83, 91, 84, 16, 93, 74, 122, 174, 96, 192, 143, 196, 95, 150, 135, 24, 27, 79, 223, 198, 37, 189, 26, 117, 63, 167, 57, 127, 237, 117];
    let pub_key_y = [53, 71, 241, 28, 168, 105, 102, 70, 242, 243, 172, 176, 142, 49, 1, 106, 250, 194, 62, 99, 12, 93, 17, 245, 159, 97, 254, 245, 123, 13, 42, 165];
    let signature = [57, 17, 112, 239, 241, 30, 64, 157, 170, 50, 85, 145, 156, 69, 226, 85, 147, 164, 10, 82, 71, 93, 42, 132, 200, 220, 161, 255, 95, 241, 211, 141, 81, 7, 150, 25, 25, 27, 162, 213, 80, 61, 12, 170, 50, 4, 154, 203, 252, 229, 119, 29, 202, 153, 50, 25, 126, 145, 245, 23, 136, 75, 29, 177];
    let hashed_message = [13, 82, 120, 60, 76, 186, 215, 235, 175, 126, 185, 67, 252, 100, 143, 82, 130, 165, 32, 112, 68, 47, 193, 141, 141, 209, 109, 219, 47, 203, 175, 102];

    let addr = ecrecover(pub_key_x, pub_key_y, signature, hashed_message);
    assert(addr == 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266);
}