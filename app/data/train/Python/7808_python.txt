#http://informatics.mccme.ru/mod/statements/view3.php?id=22783&chapterid=113362#1
n = int(input())

def sum_kv_cifr(x):
    su = 0
    for i in str(x):
        su += int(i)*int(i)
    return su


maxi_power = 0

for i in range(1, n//2+1):
    print('______',i)
    for k in range(n//i, 0, -1):
        power = sum_kv_cifr(i * k)
        print('_', k, power)

        if power > maxi_power:
            maxi_power = power

print(maxi_power)


