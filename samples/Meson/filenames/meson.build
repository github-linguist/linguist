project('test', ['c'],
  version: '0.1.0'
)

# This is a comment test('foo')

add_global_arguments(['-foo'])
add_global_link_arguments(['-foo'])

gnome = import('gnome') # As is this

gnome.do_something('test')

meson.source_root()

foreach foo: bar
  foreach baz : foo
    message(baz)
  endforeach
endforeach

blah = '''
afjoakjflajf  # Test
lflkasjf
test\'test
test\\\\test
test\ntest
'''

foo = ''
foo = ''''''
foo = 'string'
foo = '''string2'''
foo = 12314
foo = 1231.1231
foo = true
foo = false
foo = ['te\'st', 1, 3.3, '''test''']
foo += 1231
foo = '@0@'.format('test')
foo = include_directories('foo', kwarg: 'bar', include_directories: 'foo')
foo = true ? 'true' : 'false'
foo = 2 - 1 + 3 % 8 / 4 * 3

if true and false
elif false or true
elif true not false
elif foo == 12
elif (foo != 124) and (foo <= 200)
else
endif
