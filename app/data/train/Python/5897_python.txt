#http://codeforces.com/problemset/problem/71/A

T = int(raw_input())

while(not T == 0):
	word = str(raw_input())
	
	if len(word)>10:
		print word[0]+str(len(word[1:len(word)-1]))+word[len(word)-1]
	else:
		print word
		
	T-=1
