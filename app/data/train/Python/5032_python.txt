low_primes = {1,3,5,7,11,13}

low_primes.add(17)                    # It will be {1,3,5,7,11,13,17}

low_primes.update({19,23},{2,29})     # It will be {1,2,3,5,7,11,13,17,19,23,29}, sorted order

while low_primes:
    print(low_primes.pop()/3)         #It will pop the first one (1) out, but because it is within a while loop, it will eventually pop everything out
