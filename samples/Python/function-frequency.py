import ast

class CallCountingVisitor(ast.NodeVisitor):

    def __init__(self):
        self.calls = {}

    def visit_Call(self, node):
        if isinstance(node.func, ast.Name):
            fun_name = node.func.id
            call_count = self.calls.get(fun_name, 0)
            self.calls[fun_name] = call_count + 1
        self.generic_visit(node)

filename = input('Enter a filename to parse: ')
with open(filename, encoding='utf-8') as f:
    contents = f.read()
root = ast.parse(contents, filename=filename) #NOTE: this will throw a SyntaxError if the file isn't valid Python code
visitor = CallCountingVisitor()
visitor.visit(root)
top10 = sorted(visitor.calls.items(), key=lambda x: x[1], reverse=True)[:10]
for name, count in top10:
    print(name,'called',count,'times')
