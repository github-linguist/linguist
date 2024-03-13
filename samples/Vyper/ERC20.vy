# @dev Implementation of ERC-20 token standard.
# @author Takayuki Jimba (@yudetamago)
# https://github.com/ethereum/EIPs/blob/master/EIPS/eip-20.md

Transfer: event({_from: indexed(address), _to: indexed(address), _value: uint256})
Approval: event({_owner: indexed(address), _spender: indexed(address), _value: uint256})

name: public(string[64])
symbol: public(string[32])
decimals: public(uint256)

# NOTE: By declaring `balanceOf` as public, vyper automatically generates a 'balanceOf()' getter
#       method to allow access to account balances.
#       The _KeyType will become a required parameter for the getter and it will return _ValueType.
#       See: https://vyper.readthedocs.io/en/v0.1.0-beta.8/types.html?highlight=getter#mappings
balanceOf: public(map(address, uint256))
allowances: map(address, map(address, uint256))
total_supply: uint256
minter: address


@public
def __init__(_name: string[64], _symbol: string[32], _decimals: uint256, _supply: uint256):
    init_supply: uint256 = _supply * 10 ** _decimals
    self.name = _name
    self.symbol = _symbol
    self.decimals = _decimals
    self.balanceOf[msg.sender] = init_supply
    self.total_supply = init_supply
    self.minter = msg.sender
    log.Transfer(ZERO_ADDRESS, msg.sender, init_supply)


@public
@constant
def totalSupply() -> uint256:
    """
    @dev Total number of tokens in existence.
    """
    return self.total_supply


@public
@constant
def allowance(_owner : address, _spender : address) -> uint256:
    """
    @dev Function to check the amount of tokens that an owner allowed to a spender.
    @param _owner The address which owns the funds.
    @param _spender The address which will spend the funds.
    @return An uint256 specifying the amount of tokens still available for the spender.
    """
    return self.allowances[_owner][_spender]


@public
def transfer(_to : address, _value : uint256) -> bool:
    """
    @dev Transfer token for a specified address
    @param _to The address to transfer to.
    @param _value The amount to be transferred.
    """
    # NOTE: vyper does not allow unterflows
    #       so the following subtraction would revert on insufficient balance
    self.balanceOf[msg.sender] -= _value
    self.balanceOf[_to] += _value
    log.Transfer(msg.sender, _to, _value)
    return True


@public
def transferFrom(_from : address, _to : address, _value : uint256) -> bool:
    """
     @dev Transfer tokens from one address to another.
          Note that while this function emits a Transfer event, this is not required as per the specification,
          and other compliant implementations may not emit the event.
     @param _from address The address which you want to send tokens from
     @param _to address The address which you want to transfer to
     @param _value uint256 the amount of tokens to be transferred
    """
    # NOTE: vyper does not allow unterflows
    #       so the following subtraction would revert on insufficient balance
    self.balanceOf[_from] -= _value
    self.balanceOf[_to] += _value
    # NOTE: vyper does not allow underflows
    #      so the following subtraction would revert on insufficient allowance
    self.allowances[_from][msg.sender] -= _value
    log.Transfer(_from, _to, _value)
    return True


@public
def approve(_spender : address, _value : uint256) -> bool:
    """
    @dev Approve the passed address to spend the specified amount of tokens on behalf of msg.sender.
         Beware that changing an allowance with this method brings the risk that someone may use both the old
         and the new allowance by unfortunate transaction ordering. One possible solution to mitigate this
         race condition is to first reduce the spender's allowance to 0 and set the desired value afterwards:
         https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729
    @param _spender The address which will spend the funds.
    @param _value The amount of tokens to be spent.
    """
    self.allowances[msg.sender][_spender] = _value
    log.Approval(msg.sender, _spender, _value)
    return True


@public
def mint(_to: address, _value: uint256):
    """
    @dev Mint an amount of the token and assigns it to an account. 
         This encapsulates the modification of balances such that the
         proper events are emitted.
    @param _to The account that will receive the created tokens.
    @param _value The amount that will be created.
    """
    assert msg.sender == self.minter
    assert _to != ZERO_ADDRESS
    self.total_supply += _value
    self.balanceOf[_to] += _value
    log.Transfer(ZERO_ADDRESS, _to, _value)


@private
def _burn(_to: address, _value: uint256):
    """
    @dev Internal function that burns an amount of the token of a given
         account.
    @param _to The account whose tokens will be burned.
    @param _value The amount that will be burned.
    """
    assert _to != ZERO_ADDRESS
    self.total_supply -= _value
    self.balanceOf[_to] -= _value
    log.Transfer(_to, ZERO_ADDRESS, _value)


@public
def burn(_value: uint256):
    """
    @dev Burn an amount of the token of msg.sender.
    @param _value The amount that will be burned.
    """
    self._burn(msg.sender, _value)


@public
def burnFrom(_to: address, _value: uint256):
    """
    @dev Burn an amount of the token from a given account.
    @param _to The account whose tokens will be burned.
    @param _value The amount that will be burned.
    """
    self.allowances[_to][msg.sender] -= _value
    self._burn(_to, _value)
