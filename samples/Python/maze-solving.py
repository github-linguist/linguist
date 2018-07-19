# python 3

def Dijkstra(Graph, source):
    '''
        +   +---+---+
        | 0   1   2 |
        +---+   +   +
        | 3   4 | 5
        +---+---+---+

        >>> graph = (        # or ones on the diagonal
        ...     (0,1,0,0,0,0,),
        ...     (1,0,1,0,1,0,),
        ...     (0,1,0,0,0,1,),
        ...     (0,0,0,0,1,0,),
        ...     (0,1,0,1,0,0,),
        ...     (0,0,1,0,0,0,),
        ... )
        ...
        >>> Dijkstra(graph, 0)
        ([0, 1, 2, 3, 2, 3], [1e+140, 0, 1, 4, 1, 2])
        >>> display_solution([1e+140, 0, 1, 4, 1, 2])
        5<2<1<0
    '''
    # Graph[u][v] is the weight from u to v (however 0 means infinity)
    infinity = float('infinity')
    n = len(graph)
    dist = [infinity]*n   # Unknown distance function from source to v
    previous = [infinity]*n # Previous node in optimal path from source
    dist[source] = 0        # Distance from source to source
    Q = list(range(n)) # All nodes in the graph are unoptimized - thus are in Q
    while Q:           # The main loop
        u = min(Q, key=lambda n:dist[n])                 # vertex in Q with smallest dist[]
        Q.remove(u)
        if dist[u] == infinity:
            break # all remaining vertices are inaccessible from source
        for v in range(n):               # each neighbor v of u
            if Graph[u][v] and (v in Q): # where v has not yet been visited
                alt = dist[u] + Graph[u][v]
                if alt < dist[v]:       # Relax (u,v,a)
                    dist[v] = alt
                    previous[v] = u
    return dist,previous

def display_solution(predecessor):
    cell = len(predecessor)-1
    while cell:
        print(cell,end='<')
        cell = predecessor[cell]
    print(0)
