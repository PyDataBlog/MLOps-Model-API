#Kunal Gautam
#Codewars : @Kunalpod
#Problem name: Mexican Wave
#Problem level: 6 kyu

def wave(str):
    li=[]
    for i in range(len(str)):
        x=list(str)
        x[i]=x[i].upper()
        li.append(''.join(x))
    return [x for x in li if x!=str]
