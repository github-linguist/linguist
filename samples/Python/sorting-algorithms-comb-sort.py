>>> def combsort(input):
    gap = len(input)
    swaps = True
    while gap > 1 or swaps:
        gap = max(1, int(gap / 1.25))  # minimum gap is 1
        swaps = False
        for i in range(len(input) - gap):
            j = i+gap
            if input[i] > input[j]:
                input[i], input[j] = input[j], input[i]
                swaps = True


>>> y = [88, 18, 31, 44, 4, 0, 8, 81, 14, 78, 20, 76, 84, 33, 73, 75, 82, 5, 62, 70]
>>> combsort(y)
>>> assert y == sorted(y)
>>> y
[0, 4, 5, 8, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
>>>
