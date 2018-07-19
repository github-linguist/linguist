def selectionSort(lst):
    for i in range(0,len(lst)-1):
        mn = min(range(i,len(lst)), key=lst.__getitem__)
        lst[i],lst[mn] = lst[mn],lst[i]
    return lst
