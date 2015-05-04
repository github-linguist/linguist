implement Lock;

include "sys.m";
	sys:	Sys;
include "lock.m";

Semaphore.obtain(l: self ref Semaphore)
{
	l.c <-= 0;
}

Semaphore.release(l: self ref Semaphore)
{
	<-l.c;
}

Semaphore.new(): ref Semaphore
{
	l := ref Semaphore;
	l.c = chan[1] of int;
	return l;
}

init()
{
}
