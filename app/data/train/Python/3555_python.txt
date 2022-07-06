n = int(input())
st = [(-1, -2)]
s = 0
for i, h in enumerate(map(int, input().split() + [' -1'])):
    if h > st[-1][1]:
        st.append((i, h))
    else:
        while st[-1][1] >= h:
            r = st.pop()
            s = max(s, (i - r[0]) * r[1])
        st.append((r[0], h))
print(s)
