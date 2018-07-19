select i, k, sum(A.N*B.N) as N
        from A inner join B on A.j=B.j
        group by i, k
