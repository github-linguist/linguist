import subprocess
import time

class Tlogger(object):
    def __init__(self):
        self.counts = 0
        self.tottime = 0.0
        self.laststart = 0.0
        self.lastreport = time.time()

    def logstart(self):
        self.laststart = time.time()

    def logend(self):
        self.counts +=1
        self.tottime += (time.time()-self.laststart)
        if (time.time()-self.lastreport)>5.0:   # report once every 5 seconds
           self.report()

    def report(self):
        if ( self.counts > 4*self.tottime):
            print "Subtask execution rate: %f times/second"% (self.counts/self.tottime);
        else:
            print "Average execution time: %f seconds"%(self.tottime/self.counts);
        self.lastreport = time.time()


def taskTimer( n, subproc_args ):
    logger = Tlogger()

    for x in range(n):
        logger.logstart()
        p = subprocess.Popen(subproc_args)
        p.wait()
        logger.logend()
    logger.report()


import timeit
import sys

def main( ):

    # for accurate timing of code segments
    s = """j = [4*n for n in range(50)]"""
    timer = timeit.Timer(s)
    rzlts = timer.repeat(5, 5000)
    for t in rzlts:
        print "Time for 5000 executions of statement = ",t

    # subprocess execution timing
    print "#times:",sys.argv[1]
    print "Command:",sys.argv[2:]
    print ""
    for k in range(3):
       taskTimer( int(sys.argv[1]), sys.argv[2:])

main()
