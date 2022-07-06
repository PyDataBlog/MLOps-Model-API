# -*- coding: utf-8 -*-


def calc_note(count, value):
    qnt = 0
    if count >= value:
        qnt = int(count) / value
    print '%d nota(s) de R$ %d.00' % (qnt, value)
    return count - qnt * value


n = float(raw_input())

print 'NOTAS:'
n = calc_note(n, 100)
n = calc_note(n, 50)
n = calc_note(n, 20)
n = calc_note(n, 10)
n = calc_note(n, 5)
n = calc_note(n, 2)
print 'MOEDAS:'
print '%d moeda(s) de R$ 1.00' % int(n)
n -= int(n)
m50 = n / 0.50
print '%d moeda(s) de R$ 0.50' % m50
n -= int(m50) * 0.50
m25 = n / 0.25
print '%d moeda(s) de R$ 0.25' % m25
n -= int(m25) * 0.25
m10 = n / 0.10
print '%d moeda(s) de R$ 0.10' % m10
n -= int(m10) * 0.10
if round(n, 2) >= 0.05:
    print '1 moeda(s) de R$ 0.05'
    m1 = (n - 0.05) * 100
else:
    print '0 moeda(s) de R$ 0.05'
    m1 = round(n, 2) * 100
if round(m1, 0):
    print '%.0f moeda(s) de R$ 0.01' % m1
else:
    print '0 moeda(s) de R$ 0.01'
