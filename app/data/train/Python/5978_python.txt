num = input("What is the numerator")
dem = input("What is the denominator")

counta = 2
countb = 2


def math (num,dem):
    remainsa = 1
    remainsb = 1
    remains = remainsa - remainsb
    while remains > 0:
        a = num / counta
        b = dem / countb
        remainsa = num % counta
        remainsb = num % countb
        remains = remainsa - remainsb
        if remains = 
