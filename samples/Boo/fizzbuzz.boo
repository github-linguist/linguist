def fizzbuzz(size):
    for i in range(1, size):
        if i%15 == 0:
            print 'FizzBuzz'
        elif i%5 == 0:
            print 'Buzz'
        elif i%3 == 0:
            print 'Fizz'
        else:
            print i

fizzbuzz(101)
