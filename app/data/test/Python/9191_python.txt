

f = open('test_content.txt', 'r')
print(f.read())
f.close()

# using context manager
with open('test_content.txt', 'r') as f:
    print(f.read())
