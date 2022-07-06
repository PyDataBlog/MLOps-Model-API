# /**
#  * Definition for a binary tree node.
#  * public class TreeNode {
#  *     int val;
#  *     TreeNode left;
#  *     TreeNode right;
#  *     TreeNode(int x) { val = x; }
#  * }
#  */
# public class Solution {
#     public List<Integer> inorderTraversal(TreeNode root) {
#         List<Integer> list = new ArrayList<Integer>();
#
#         Stack<TreeNode> stack = new Stack<TreeNode>();
#         TreeNode cur = root;
#
#         while(cur!=null || !stack.empty()){
#             while(cur!=null){
#                 stack.add(cur);
#                 cur = cur.left;
#             }
#             cur = stack.pop();
#             list.add(cur.val);
#             cur = cur.right;
#         }
#
#         return list;
#     }
# }
# Definition for a binary tree node.
# class TreeNode(object):
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

class Solution(object):
    def inorderTraversal(self, root):
        """
        :type root: TreeNode
        :rtype: List[int]
        """
        res, stack = [], []
        while True:
            while root:
                stack.append(root)
                root = root.left
            if not stack:
                return res
            node = stack.pop()
            res.append(node.val)
            root = node.right
