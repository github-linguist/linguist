def dotp(a,b):
    assert len(a) == len(b), 'Vector sizes must match'
    return sum(aterm * bterm for aterm,bterm in zip(a, b))

if __name__ == '__main__':
    a, b = [1, 3, -5], [4, -2, -1]
    assert dotp(a,b) == 3
