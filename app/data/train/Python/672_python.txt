import os
import sys
import string
import random
import math


#################################################
# State

balance = 0


def deposit(amount):
    global balance
    balance += amount
    return balance


def withdraw(amount):
    global balance
    balance -= amount
    return balance

#################################################
# Dict like


def make_account():
    return {'balance': 0}


def deposit(account, amount):
    account['balance'] += amount
    return account['balance']


def withdraw(account, amount):
    account['balance'] -= amount
    return account['balance']


# >>> a = make_account()
# >>> b = make_account()
# >>> deposit(a, 100)
# 100
# >>> deposit(b, 50)
# 50
# >>> withdraw(b, 10)
# 40
# >>> withdraw(a, 10)
# 90

#################################################
# Class


class BankAccount:
    def __init__(self, balance=0):
        self.balance = balance

    def withdraw(self, amount):
        self.balance -= amount
        return self.balance

    def deposit(self, amount):
        self.balance += amount
        return self.balance


# >>> a = BankAccount()
# >>> b = BankAccount()
# >>> a.deposit(100)
# 100
# >>> b.deposit(50)
# 50
# >>> b.withdraw(10)
# 40
# >>> a.withdraw(10)
# 90

#################################################
# Inheritance


class MinimumBalanceAccount(BankAccount):
    def __init__(self, minimum_balance):
        BankAccount.__init__(self)
        self.minimum_balance = minimum_balance

    def withdraw(self, amount):
        if self.balance - amount < self.minimum_balance:
            print('Sorry, minimum balance must be maintained.')
        else:
            BankAccount.withdraw(self, amount)

# >>> a = MinimumBalanceAccount(0)
# >>> a.deposit(100)
# 100
# >>> b.withdraw(101)
# 'Sorry, minimum balance must be maintained.'


########################################
# Mangling, Exceptions


def generate_id(n=16):
    alphabet = string.ascii_letters + string.digits
    return ''.join(random.choice(alphabet) for _ in range(n))


class WithdrawError(Exception):
    """Not enough money"""
    def __init__(self, amount):
        super().__init__()
        self.amount = amount


class AdvancedBankAccount:
    MAX_BALANCE = 2 ** 64

    def __init__(self):
        self._balance = 0
        self.__id = generate_id()

    def withdraw(self, amount):
        if not isinstance(amount, int):
            raise ValueError
        if self._balance < amount:
            raise WithdrawError(amount)
        self._balance -= amount
        return self._balance

    def deposit(self, amount):
        self._balance += amount
        return self._balance

    def get_max_balance():
        return AdvancedBankAccount.MAX_BALANCE

if __name__ == '__main__':
    a = AdvancedBankAccount()
    b = a
    c = AdvancedBankAccount()
    a.deposit(10)
    # AdvancedBankAccount.deposit(a, 10) # the same
    print('UNACCEPTABLE! b balance:', b._balance)
    # print(b.__id) # error, name mangling
    a.get_id = lambda self: self.__id
    # print(a.get_id())  # TypeError
    # print(a.get_id(a)) # AttributeError

    ################################################
    # UNACCEPTABLE!
    print("UNACCEPTABLE! b id:", b._AdvancedBankAccount__id)  # name unmangling

    # static
    AdvancedBankAccount.MAX_BALANCE = 2 ** 32
    print('max balance:', AdvancedBankAccount.get_max_balance())
    a.MAX_BALANCE = 2 ** 64
    print('a max: {}, c max: {}'.format(a.MAX_BALANCE,
                                        c.MAX_BALANCE))

    ################################################
    # Exceptions

    # in module import
    try:
        a.withdraw("100")
    except:
        pass
        # UNACCEPTIBLE!
    try:
        a.withdraw(100)
    except WithdrawError as e:
        pass

    try:
        a.withdraw(100)
    except (ValueError, WithdrawError) as e:
        print('exception raised')
    else:
        print('no exception')
    finally:
        print('Finally')

    def tricky():
        try:
            print('Tricky called')
            return 1
        finally:
            print('Tricky finally called')
            return 42
        return 0

    print(tricky())
    # how about with statement?
    # module is object -> import

class Shape:
    def area(self):
        raise NotImplementedError

class Circle(Shape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return math.pi * self.radius ** 2

class Square(Shape):
    def __init__(self, side):
        self.side = side

    def area(self):
        return self.side ** 2


if __name__ == "__main__":
    a = [Square(10), Circle(2)]
    s = sum(s.area() for s in a)
    print(s)
