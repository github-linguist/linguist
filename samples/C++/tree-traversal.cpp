#include <boost/scoped_ptr.hpp>
#include <iostream>
#include <queue>

template<typename T>
class TreeNode {
public:
  TreeNode(const T& n, TreeNode* left = NULL, TreeNode* right = NULL)
    : mValue(n),
      mLeft(left),
      mRight(right) {}

  T getValue() const {
    return mValue;
  }

  TreeNode* left() const {
    return mLeft.get();
  }

  TreeNode* right() const {
    return mRight.get();
  }

  void preorderTraverse() const {
    std::cout << " " << getValue();
    if(mLeft)  { mLeft->preorderTraverse();  }
    if(mRight) { mRight->preorderTraverse(); }
  }

  void inorderTraverse() const {
    if(mLeft)  { mLeft->inorderTraverse();  }
    std::cout << " " << getValue();
    if(mRight) { mRight->inorderTraverse(); }
  }

  void postorderTraverse() const {
    if(mLeft)  { mLeft->postorderTraverse();  }
    if(mRight) { mRight->postorderTraverse(); }
    std::cout << " " << getValue();
  }

  void levelorderTraverse() const {
    std::queue<const TreeNode*> q;
    q.push(this);

    while(!q.empty()) {
      const TreeNode* n = q.front();
      q.pop();
      std::cout << " " << n->getValue();

      if(n->left())  { q.push(n->left());  }
      if(n->right()) { q.push(n->right()); }
    }
  }

protected:
  T mValue;
  boost::scoped_ptr<TreeNode> mLeft;
  boost::scoped_ptr<TreeNode> mRight;

private:
  TreeNode();
};

int main() {
  TreeNode<int> root(1,
    new TreeNode<int>(2,
      new TreeNode<int>(4,
        new TreeNode<int>(7)),
      new TreeNode<int>(5)),
    new TreeNode<int>(3,
      new TreeNode<int>(6,
        new TreeNode<int>(8),
        new TreeNode<int>(9))));

  std::cout << "preorder:   ";
  root.preorderTraverse();
  std::cout << std::endl;

  std::cout << "inorder:    ";
  root.inorderTraverse();
  std::cout << std::endl;

  std::cout << "postorder:  ";
  root.postorderTraverse();
  std::cout << std::endl;

  std::cout << "level-order:";
  root.levelorderTraverse();
  std::cout << std::endl;

  return 0;
}
