def is_palindrome(s):
    '''
        >>> is_palindrome('')
        True
        >>> is_palindrome('a')
        True
        >>> is_palindrome('aa')
        True
        >>> is_palindrome('baa')
        False
        >>> is_palindrome('baab')
        True
        >>> is_palindrome('ba_ab')
        True
        >>> is_palindrome('ba_ ab')
        False
        >>> is_palindrome('ba _ ab')
        True
        >>> is_palindrome('ab'*2)
        False
        >>> x = 'ab' *2**15
        >>> len(x)
        65536
        >>> xreversed = x[::-1]
        >>> is_palindrome(x+xreversed)
        True
        >>> len(x+xreversed)
        131072
        >>>
    '''
    return s == s[::-1]

def _test():
    import doctest
    doctest.testmod()
    #doctest.testmod(verbose=True)

if __name__ == "__main__":
    _test()
