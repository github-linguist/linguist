def op_pow(stack):
    b = stack.pop(); a = stack.pop()
    stack.append( a ** b )
def op_mul(stack):
    b = stack.pop(); a = stack.pop()
    stack.append( a * b )
def op_div(stack):
    b = stack.pop(); a = stack.pop()
    stack.append( a / b )
def op_add(stack):
    b = stack.pop(); a = stack.pop()
    stack.append( a + b )
def op_sub(stack):
    b = stack.pop(); a = stack.pop()
    stack.append( a - b )
def op_num(stack, num):
    stack.append( num )

ops = {
 '^': op_pow,
 '*': op_mul,
 '/': op_div,
 '+': op_add,
 '-': op_sub,
 }

def get_input(inp = None):
    'Inputs an expression and returns list of tokens'

    if inp is None:
        inp = input('expression: ')
    tokens = inp.strip().split()
    return tokens

def rpn_calc(tokens):
    stack = []
    table = ['TOKEN,ACTION,STACK'.split(',')]
    for token in tokens:
        if token in ops:
            action = 'Apply op to top of stack'
            ops[token](stack)
            table.append( (token, action, ' '.join(str(s) for s in stack)) )
        else:
            action = 'Push num onto top of stack'
            op_num(stack, eval(token))
            table.append( (token, action, ' '.join(str(s) for s in stack)) )
    return table

if __name__ == '__main__':
    rpn = '3 4 2 * 1 5 - 2 3 ^ ^ / +'
    print( 'For RPN expression: %r\n' % rpn )
    rp = rpn_calc(get_input(rpn))
    maxcolwidths = [max(len(y) for y in x) for x in zip(*rp)]
    row = rp[0]
    print( ' '.join('{cell:^{width}}'.format(width=width, cell=cell) for (width, cell) in zip(maxcolwidths, row)))
    for row in rp[1:]:
        print( ' '.join('{cell:<{width}}'.format(width=width, cell=cell) for (width, cell) in zip(maxcolwidths, row)))

    print('\n The final output value is: %r' % rp[-1][2])
