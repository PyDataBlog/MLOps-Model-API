class Solution(object):
    def __init__(self):
        self.l=[]
    def helper(self,root,level):
        if not root:
            return None
        else:
            if level<len(self.l):
                self.l[level].append(root.val)
            else:
                self.l.append([root.val])
            self.helper(root.left,level+1)
            self.helper(root.right,level+1)
        return self.l
    def levelOrder(self, root):
        """
        :type root: TreeNode
        :rtype: List[List[int]]
        """
        if not root:
            return []
        return self.helper(root,0)
