class Solution(object):
    def multiply(self, A, B):
        """
        :type A: List[List[int]]
        :type B: List[List[int]]
        :rtype: List[List[int]]
        """
        p = len(B[0])
        C = [[0 for _ in xrange(p)] for _ in xrange(len(A))]
        for i in xrange(len(A)):
            for j in xrange(len(B)):
                if A[i][j] != 0:
                    for k in xrange(p):
                        C[i][k] += A[i][j] * B[j][k]
        return C
                    
                    
                    
