from collections import OrderedDict

n = int(input())
occurrences = OrderedDict()
for _ in range(0, n):
    word = input().strip()
    occurrences[word] = occurrences.get(word, 0) + 1
print(len(occurrences))
print(sep=' ', *[count for _, count in occurrences.items()])
