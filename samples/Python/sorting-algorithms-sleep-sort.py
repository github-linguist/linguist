from time import sleep
from threading import Timer

def sleepsort(values):
    sleepsort.result = []
    def add1(x):
        sleepsort.result.append(x)
    mx = values[0]
    for v in values:
        if mx < v: mx = v
        Timer(v, add1, [v]).start()
    sleep(mx+1)
    return sleepsort.result

if __name__ == '__main__':
    x = [3,2,4,7,3,6,9,1]
    if sleepsort(x) == sorted(x):
        print('sleep sort worked for:',x)
    else:
        print('sleep sort FAILED for:',x)
