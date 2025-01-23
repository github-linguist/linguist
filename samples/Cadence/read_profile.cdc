import Profile from 0xProfile

pub fun main(address: Address): Profile.ReadOnly? {
    return Profile.read(address)
}