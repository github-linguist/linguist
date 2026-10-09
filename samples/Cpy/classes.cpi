# classes, inheritance, special methods and exceptions
class InsufficientFunds(Exception):
    pass

class Account:
    interest = 0.02

    def __init__(self, owner, balance=0):
        self.owner = owner
        self.balance = balance

    def deposit(self, amount):
        if amount <= 0:
            raise ValueError("amount must be positive")
        self.balance += amount
        return self

    def withdraw(self, amount):
        if amount > self.balance:
            raise InsufficientFunds(f"{self.owner} has only {self.balance}")
        self.balance -= amount
        return self

    def __str__(self):
        return f"{self.owner}: {self.balance:.2f}"

    def __lt__(self, other):
        return self.balance < other.balance

class Savings(Account):
    def add_interest(self):
        self.balance += self.balance * Account.interest
        return self

accounts = [Account("Ann", 100), Savings("Bob", 250), Account("Cid")]
accounts[0].deposit(50).withdraw(20)
accounts[1].add_interest()

for acc in sorted(accounts, reverse=True):
    print(acc)

for action in [lambda: accounts[2].withdraw(10), lambda: accounts[0].deposit(-1)]:
    try:
        action()
    except InsufficientFunds as e:
        print("insufficient:", e)
    except ValueError as e:
        print("bad value:", e)
