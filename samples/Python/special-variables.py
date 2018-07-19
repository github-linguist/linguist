names = sorted((set(globals().keys()) | set(__builtins__.__dict__.keys())) - set('_ names i'.split()))
print( '\n'.join(' '.join(names[i:i+8]) for i in range(0, len(names), 8)) )
