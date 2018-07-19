from concurrent import futures
from math import floor, sqrt

NUMBERS = [
    112272537195293,
    112582718962171,
    112272537095293,
    115280098190773,
    115797840077099,
    1099726829285419]
# NUMBERS = [33, 44, 55, 275]

def lowest_factor(n, _start=3):
    if n % 2 == 0:
        return 2
    search_max = int(floor(sqrt(n))) + 1
    for i in range(_start, search_max, 2):
        if n % i == 0:
            return i
    return n

def prime_factors(n, lowest):
    pf = []
    while n > 1:
        pf.append(lowest)
        n //= lowest
        lowest = lowest_factor(n, max(lowest, 3))
    return pf

def prime_factors_of_number_with_lowest_prime_factor(NUMBERS):
    with futures.ProcessPoolExecutor() as executor:
        low_factor, number = max( (l, f) for l, f in zip(executor.map(lowest_factor, NUMBERS), NUMBERS) )
        all_factors = prime_factors(number, low_factor)
        return number, all_factors


def main():
    print( 'For these numbers:\n  ' + '\n  '.join(str(p) for p in NUMBERS) )
    number, all_factors = prime_factors_of_number_with_lowest_prime_factor(NUMBERS)
    print('    The one with the largest minimum prime factor is %i:' % number)
    print('      All its prime factors in order are: %s' % all_factors)


if __name__ == '__main__':
    main()
