// DFS
// recursive
#include <iostream>
using namespace std;

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};

int minDepth(TreeNode *root)
{ 
    if(NULL == root)
        return 0;
    if(NULL == root->left && NULL == root->right)
        return 1;
    int left = minDepth(root->left);
    int right = minDepth(root->right);
    
    if(0 == left)
        return right + 1;
    if(0 == right)
        return left + 1;
    return left < right ? left + 1 : right + 1;
}

int main()
{

    return 0;
}
