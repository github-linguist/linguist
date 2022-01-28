%lang starknet

@contract_interface
namespace IAccount:
  #
  # Getters
  #

  func get_nonce() -> (res : felt):
  end

  func is_account() -> (res: felt):
  end

  #
  # Business logic
  #

  func is_valid_signature(
      hash: felt,
      signature_len: felt,
      signature: felt*
    ):
  end

  func execute(
      to: felt,
      selector: felt,
      calldata_len: felt,
      calldata: felt*,
      nonce: felt
    ) -> (response_len: felt, response: felt*):
  end
end
