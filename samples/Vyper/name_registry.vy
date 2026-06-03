
registry: HashMap[Bytes[100], address]

@external
def register(name: Bytes[100], owner: address):
    assert self.registry[name] == ZERO_ADDRESS  # check name has not been set yet.
    self.registry[name] = owner


@view
@external
def lookup(name: Bytes[100]) -> address:
    return self.registry[name]
