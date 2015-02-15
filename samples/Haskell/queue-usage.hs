Prelude> :l fifo.hs
[1 of 1] Compiling Main             ( fifo.hs, interpreted )
Ok, modules loaded: Main.
*Main> let q = emptyFifo
*Main> isEmpty q
True
*Main> let q' = push q 1
*Main> isEmpty q'
False
*Main> let q'' = foldl push q' [2..4]
*Main> let (v,q''') = pop q''
*Main> v
Just 1
*Main> let (v',q'''') = pop q'''
*Main> v'
Just 2
*Main> let (v'',q''''') = pop q''''
*Main> v''
Just 3
*Main> let (v''',q'''''') = pop q'''''
*Main> v'''
Just 4
*Main> let (v'''',q''''''') = pop q''''''
*Main> v''''
Nothing
