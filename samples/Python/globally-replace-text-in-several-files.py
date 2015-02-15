import fileinput

for line in fileinput.input(inplace=True):
    print(line.replace('Goodbye London!', 'Hello New York!'), end='')
