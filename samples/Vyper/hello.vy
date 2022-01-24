hello: public(String[100])

@external
def __init__():
    self.hello = "Hello World!"

@external
def hello() -> String[100]:
    return self.hello