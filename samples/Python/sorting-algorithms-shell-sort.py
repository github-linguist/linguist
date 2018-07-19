def shell(seq):
    inc = len(seq) // 2
    while inc:
        for i, el in enumerate(seq):
            while i >= inc and seq[i - inc] > el:
                seq[i] = seq[i - inc]
                i -= inc
            seq[i] = el
        inc = 1 if inc == 2 else int(inc * 5.0 / 11)

data = [22, 7, 2, -5, 8, 4]
shell(data)
print data # [-5, 2, 4, 7, 8, 22]
