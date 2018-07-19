from collections import namedtuple
from sys import stdout

Node = namedtuple('Node', 'data, left, right')
tree = Node(1,
            Node(2,
                 Node(4,
                      Node(7, None, None),
                      None),
                 Node(5, None, None)),
            Node(3,
                 Node(6,
                      Node(8, None, None),
                      Node(9, None, None)),
                 None))

def printwithspace(i):
    stdout.write("%i " % i)

def preorder(node, visitor = printwithspace):
    if node is not None:
        visitor(node.data)
        preorder(node.left, visitor)
        preorder(node.right, visitor)

def inorder(node, visitor = printwithspace):
    if node is not None:
        inorder(node.left, visitor)
        visitor(node.data)
        inorder(node.right, visitor)

def postorder(node, visitor = printwithspace):
    if node is not None:
        postorder(node.left, visitor)
        postorder(node.right, visitor)
        visitor(node.data)

def levelorder(node, more=None, visitor = printwithspace):
    if node is not None:
        if more is None:
            more = []
        more += [node.left, node.right]
        visitor(node.data)
    if more:
        levelorder(more[0], more[1:], visitor)

stdout.write('  preorder: ')
preorder(tree)
stdout.write('\n   inorder: ')
inorder(tree)
stdout.write('\n postorder: ')
postorder(tree)
stdout.write('\nlevelorder: ')
levelorder(tree)
stdout.write('\n')
