dnl $OpenBSD: fibo.m4,v 1.2 2000/07/01 14:18:39 espie Exp $
define(`copy', `$1')dnl
define(`fibo',dnl
`ifelse($1,0,`a',dnl
$1,1,`b',dnl
`copy(fibo(decr($1)))`'copy(fibo(decr(decr($1))))')')dnl
fibo(N)
