def hamming(s,t):
	dist = 0
	for x in range(len(s)):
		if s[x]!=t[x]:
			dist+=1
	return dist
