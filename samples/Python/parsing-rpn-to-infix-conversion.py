from math import log as ln
from math import sqrt, log, exp, sin, cos, tan, pi, e
from ast import literal_eval

"""
>>> # EXAMPLE USAGE
>>> result = rpn_to_infix('3 4 2 * 1 -5 - ln 2 3 ** ** / +', VERBOSE=True)
TOKEN  STACK
3      ['3']
4      ['3', '4']
2      ['3', '4', '2']
*      ['3', Node('2','*','4')]
1      ['3', Node('2','*','4'), '1']
-5     ['3', Node('2','*','4'), '1', '-5']
-      ['3', Node('2','*','4'), Node('-5','-','1')]
ln     ['3', Node('2','*','4'), Node(Node('-5','-','1'),'ln',None)]
2      ['3', Node('2','*','4'), Node(Node('-5','-','1'),'ln',None), '2']
3      ['3', Node('2','*','4'), Node(Node('-5','-','1'),'ln',None), '2', '3']
**     ['3', Node('2','*','4'), Node(Node('-5','-','1'),'ln',None), Node('3','**','2')]
**     ['3', Node('2','*','4'), Node(Node('3','**','2'),'**',Node(Node('-5','-','1'),'ln',None))]
/      ['3', Node(Node(Node('3','**','2'),'**',Node(Node('-5','-','1'),'ln',None)),'/',Node('2','*','4'))]
+      [Node(Node(Node(Node('3','**','2'),'**',Node(Node('-5','-','1'),'ln',None)),'/',Node('2','*','4')),'+','3')]
>>> result
"""

prec_dict =  {'abs':5, 'sqrt':5, 'exp':5,  'log':5,  'ln':5,
              'sin':5, 'cos':5,  'tan':5,
              '**':4, '*':3, '/':3, '+':2, '-':2}
assoc_dict = {'abs':0, 'sqrt':0, 'exp':0,  'log':0,  'ln':0,
              'sin':0, 'cos':0,  'tan':0,
              '**':1, '*':0, '/':0, '+':0, '-':0}
arity_dict = {'abs':1, 'sqrt':1, 'exp':1,  'log':1,  'ln':1,
              'sin':1, 'cos':1,  'tan':1,
              '**':2, '*':2, '/':2, '+':2, '-':2}

class Node:
    def __init__(self,x,op,y=None):
        self.precedence = prec_dict[op]
        self.assocright = assoc_dict[op]
        self.arity = arity_dict[op]
        self.op = op

        if not self.assocright and self > x and \
           isinstance(x, Node) and self.arity == 2:
                self.x = x.x
                self.y = Node(x.y, x.op, y)
        else:
            self.x,self.y = x,y

    def __str__(self):
        """
        Building an infix string that evaluates correctly is easy.
        Building an infix string that looks pretty and evaluates
        correctly requires more effort.
        """

        # easy case, Node is unary
        if self.y == None:
            return '%s(%s)'%(self.op,str(self.x))

        # determine left side string
        str_y = str(self.y)
        if  self.y < self or \
            (self.y == self and self.assocright) or \
            (str_y[0] is '-' and self.assocright):

            str_y = '(%s)'%str_y

        # determine right side string and operator
        str_x = str(self.x)
        str_op = self.op
        if self.op is '+' and not isinstance(self.x, Node) and str_x[0] is '-':
            str_x = str_x[1:]
            str_op = '-'
        elif self.op is '-' and not isinstance(self.x, Node) and str_x[0] is '-':
            str_x = str_x[1:]
            str_op = '+'
        elif self.x < self or \
             (self.x == self and not self.assocright and \
              getattr(self.x, 'op', 1) != getattr(self, 'op', 2)):

            str_x = '(%s)'%str_x

        return ' '.join([str_y, str_op, str_x])

    def __repr__(self):
        """
        >>> repr(Node('3','+','4')) == repr(eval(repr(Node('3','+','4'))))
        True
        """
        return 'Node(%s,%s,%s)'%(repr(self.x), repr(self.op), repr(self.y))

    def __lt__(self, other):
        if isinstance(other, Node):
            return self.precedence < other.precedence
        return self.precedence < prec_dict.get(other,9)

    def __gt__(self, other):
        if isinstance(other, Node):
            return self.precedence > other.precedence
        return self.precedence > prec_dict.get(other,9)



def rpn_to_infix(s, VERBOSE=False):
    """
    converts rpn notation to infix notation for string s

        "^" are replaced with "**"

    >>> rpn_to_infix('5 4 +')
    '5 + 4'
    >>> rpn_to_infix('5 -4 +')
    '5 - 4'
    >>> rpn_to_infix('5 -4 -')
    '5 + 4'
    >>> rpn_to_infix('5 -4.2 +')
    '5 - 4.2'
    >>> rpn_to_infix('-4 2 **')
    '(-4) ** 2'
    >>> rpn_to_infix('x y +')
    'x + y'
    >>> rpn_to_infix('x -y +')
    'x - y'
    >>> rpn_to_infix('5 4 3 2 ** ** **')
    '5 ** 4 ** 3 ** 2'
    >>> rpn_to_infix('5 6 ** 7 **')
    '(5 ** 6) ** 7'
    >>> rpn_to_infix('1 2 3 + -')
    '1 - (2 + 3)'
    >>> rpn_to_infix('4 3 2 + +')
    '4 + 3 + 2'
    >>> rpn_to_infix('5 4 3 2 + + +')
    '5 + 4 + 3 + 2'
    >>> rpn_to_infix('5 4 3 2 * * *')
    '5 * 4 * 3 * 2'
    >>> rpn_to_infix('5 4 3 2 + - +')
    '5 + (4 - (3 + 2))'
    >>> rpn_to_infix('3 4 5 * -')
    '3 - 4 * 5'
    >>> rpn_to_infix('3 4 5 - *')
    '(3 - 4) * 5'
    >>> rpn_to_infix('4 2 * 1 5 - +')
    '4 * 2 + (1 - 5)'
    >>> rpn_to_infix('4 2 * 1 5 - 2 ** /')
    '4 * 2 / (1 - 5) ** 2'
    >>> rpn_to_infix('3 4 2 * 1 5 - 2 3 ** ** / +')
    '3 + 4 * 2 / (1 - 5) ** 2 ** 3'
    >>> rpn_to_infix('1 2 + 3 4 + ** 5 6 + **')
    '((1 + 2) ** (3 + 4)) ** (5 + 6)'
    >>> rpn_to_infix('x sin')
    'sin(x)'
    >>> rpn_to_infix('5 4 3 2 + sqrt - +')
    '5 + (4 - sqrt(3 + 2))'
    >>> rpn_to_infix('5 4 3 2 + sqrt ln - +')
    '5 + (4 - ln(sqrt(3 + 2)))'
    >>> rpn_to_infix('5 sin 4 cos *')
    'sin(5) * cos(4)'
    >>> rpn_to_infix('5 4 cos * sin')
    'sin(5 * cos(4))'
    >>> rpn_to_infix('3 4 2 * 1 -5 - ln 2 3 ** ** / +')
    '3 + 4 * 2 / ln(1 + 5) ** 2 ** 3'
    """
    if VERBOSE : print('TOKEN  STACK')

    stack=[]
    for token in s.replace('^','**').split():
        if token in prec_dict:
            if arity_dict[token] == 1:
                stack.append(Node(stack.pop(),token))
            elif arity_dict[token] == 2:
                stack.append(Node(stack.pop(),token,stack.pop()))
        else:
            stack.append(token)

        # can't use \t in order to make global docstring pass doctest
        if VERBOSE : print(token+' '*(7-len(token))+repr(stack))

    return str(stack[0])

def rpn_eval(s):
    """
    computes the value of an rpn string

    >>> rpn_eval('5 4 +') == eval(rpn_to_infix('5 4 +'))
    True
    >>> rpn_eval('-4 2 **') == eval(rpn_to_infix('-4 2 **'))
    True
    >>> round(rpn_eval('3 4 2 * 1 -5 - ln 2 3 ** ** / +'),7) == \
        round(eval(rpn_to_infix('3 4 2 * 1 -5 - ln 2 3 ** ** / +')),7)
    True
    >>> round(rpn_eval('5 4 3 2 + sqrt ln - +'),7) == \
        round(eval(rpn_to_infix('5 4 3 2 + sqrt ln - +')),7)
    True
    """

    stack=[]
    for token in s.replace('^','**').split():
        if token in prec_dict:
            if arity_dict[token] == 1:
                stack.append(literal_eval('%s(%s)'%(token,stack.pop())))
            elif arity_dict[token] == 2:
                x,y=stack.pop(),stack.pop()
                stack.append(literal_eval('(%s) %s %s'%(y,token,x)))
        else:
            stack.append(token)

    return stack[0]

strTest = "3 4 2 * 1 -5 - ln 2 3 ** ** / +"
strResult = rpn_to_infix(strTest)
print ("Input: ",strTest)
print ("Output:",strResult)
