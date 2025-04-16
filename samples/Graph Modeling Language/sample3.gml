graph 
[
    comment "Example graph with more newline formatting"
    directed 1
    id 42
    label "Graph label"
    node 
    [
        id 1
        label "Node 1"
        extraAttribute 42
    ]
    node 
    [
        id 2
        label "node 2"
        extraAttribute 43
    ]
    node 
    [
        id 3
        label "node 3"
        extraAttribute 44
    ]
    edge 
    [
        source 1
        target 2
        label "Edge from 1 to 2"
    ]
    edge 
    [
        source 2
        target 3
        label "Edge from 2 to 3"
    ]
    edge 
    [
        source 3
        target 1
        label "Edge from 3 to 1"
    ]
]
