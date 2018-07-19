environments = [{'cnt':0, 'seq':i+1} for i in range(12)]

code = '''
print('% 4d' % seq, end='')
if seq != 1:
    cnt += 1
    seq = 3 * seq + 1 if seq & 1 else seq // 2
'''

while any(env['seq'] > 1 for env in environments):
    for env in environments:
        exec(code, globals(), env)
    print()

print('Counts')
for env in environments:
    print('% 4d' % env['cnt'], end='')
print()
