bool isSameTree(struct TreeNode* p, struct TreeNode* q) {
	if (p == NULL && q == NULL) {
		return true;
	} else if ((p && !q) || (q && !p)) {
		return false;
	} else if ((p->left == NULL && q->left != NULL) || (q->left == NULL && p->left != NULL)) {
		return false;
	} else if ((p->right == NULL && q->right != NULL) || (q->right == NULL && p->right != NULL)) {
		return false;
	} else if (p->val != q->val) {
		return false;
	} else {
		if (!isSameTree(p->left, q->left)) {
			return false;
		} else if (!isSameTree(p->right, q->right)) {
			return false;
		}
		return true;
	}
}