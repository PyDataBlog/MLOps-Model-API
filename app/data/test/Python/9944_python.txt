import math

def is_palindrome(n):
    s = str(n)

    return s == s[::-1]

def is_prime(n):
    if n <= 1:
        return False

    if n % 2 == 0 and n != 2:
        return False

    if n == 2:
        return True

    root = math.sqrt(n)
    i    = 3
    while i <= root:
        if n % i == 0:
            return False

        i += 2

    return True

i = 999
while i > 0:
    if not is_palindrome(i):
        i -= 1
        continue

    if not is_prime(i):
        i -= 1
        continue

    print i
    break

