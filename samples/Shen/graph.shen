\* graph.shen --- a library for graph definition and manipulation

Copyright (C) 2011,  Eric Schulte

*** License:

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

 - Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

 - Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*** Commentary:

Graphs are represented as two dictionaries one for vertices and one
for edges.  It is important to note that the dictionary implementation
used is able to accept arbitrary data structures as keys.  This
structure technically encodes hypergraphs (a generalization of graphs
in which each edge may contain any number of vertices).  Examples of a
regular graph G and a hypergraph H with the corresponding data
structure are given below.


--G=<graph Vertices Edges>------------------------------------------------
                            Vertices                  Edges
                           ----------                -------
 +----Graph G-----+      hash | key -> value      hash |  key  -> value
 |                |      -----+------>--------    -----+-------->---------
 | a---b---c   g  |         1 |  a  -> [1]           1 | [a b] -> [1 2]
 |     |   |      |         2 |  b  -> [1 2 3]       2 | [b c] -> [2 3]
 |     d---e---f  |         3 |  c  -> [2 4]         3 | [b d] -> [2 4]
 |                |         4 |  d  -> [3 5]         4 | [c e] -> [3 5]
 +----------------+         5 |  e  -> [4 5 6]       5 | [d e] -> [4 5]
                            6 |  f  -> [6]           6 | [e f] -> [5 6]
                            7 |  g  -> []


--H=<graph Vertices Edges>------------------------------------------------
                            Vertices                  Edges
                           ----------                -------
                         hash | key -> value      hash |  key  -> value
 +-- Hypergraph H----+   -----+------>--------    -----+-------->---------
 |                   |      1 |  a  -> [1]           1 | [a b     [1 2
 |        +------+   |      2 |  b  -> [1]             |  c d  ->  3 4
 | +------+------+   |      3 |  c  -> [1]             |  e f]     5 6]
 | |a b c |d e f |   |      4 |  d  -> [1 2]           |
 | +------+------+   |      5 |  e  -> [1 2]         2 | [d e     [4 5
 |        |g h i | j |      6 |  f  -> [1 2]           |  f g  ->  6 7
 |        +------+   |      7 |  g  -> [2]             |  h i]     8 9]
 |                   |      8 |  h  -> [2]
 +-------------------+      9 |  i  -> [2]
                           10 |  j  -> []


--G=<graph Vertices Edges>-------Graph with associated edge/vertex data---------
                            Vertices                    Edges
                           ----------                  -------
 +----Graph G-----+      hash | key -> value        hash |  key  -> value
 |   4   6     7  |      -----+------>--------      -----+-------->---------
 |0a---b---c   g  |         1 |  a  -> (@p 0 [1])      1 | [a b] -> (@p 4 [1 2])
 |    1|  3|      |         2 |  b  -> [1 2 3]         2 | [b c] -> (@p 6 [2 3])
 |     d---e---f  |         3 |  c  -> [2 4]           3 | [b d] -> (@p 1 [2 4])
 |       2   5    |         4 |  d  -> [3 5]           4 | [c e] -> (@p 3 [3 5])
 +----------------+         5 |  e  -> [4 5 6]         5 | [d e] -> (@p 2 [4 5])
                            6 |  f  -> [6]             6 | [e f] -> (@p 5 [5 6])
                            7 |  g  -> (@p 7 [])

V = # of vertices
E = # of edges
M = # of vertex edge associations

size = size of all vertices +            all vertices stored in Vertices dict
       M * sizeof(int) * 4 +             indices into Vertices & Edge dicts
       V * sizeof(dict entry) +          storage in the Vertex dict
       E * sizeof(dict entry) +          storage in the Edge dict
       2 * sizeof(dict)                  the Vertices and Edge dicts

*** Code: *\
(require dict)
(require sequence)

(datatype graph
  Vertices : dictionary;
     Edges : dictoinary;
  ===================
  (vector symbol Vertices Edges);)

(package graph- [graph graph? vertices edges add-vertex
                 add-edge has-edge? has-vertex? edges-for
                 neighbors connected-to connected? connected-components
                 vertex-partition bipartite?
                 \* included from the sequence library\ *\
                 take drop take-while drop-while range flatten
                 filter complement seperate zip indexed reduce
                 mapcon partition partition-with unique frequencies
                 shuffle pick remove-first interpose subset?
                 cartesian-product
                 \* included from the dict library\ *\
                 dict? dict dict-> <-dict contents key? keys vals
                 dictionary make-dict]

(define graph?
  X -> (= graph (<-address X 0)))

(define make-graph
  \* create a graph with specified sizes for the vertex dict and edge dict *\
  {number --> number --> graph}
  Vertsize Edgesize ->
    (let Graph (absvector 3)
      (do (address-> Graph 0 graph)
          (address-> Graph 1 (make-dict Vertsize))
          (address-> Graph 2 (make-dict Edgesize))
          Graph)))

(defmacro graph-macro
  \* return a graph taking optional sizes for the vertex and edge dicts *\
  [graph] -> [make-graph 1024 1024]
  [graph N] -> [make-graph N 1024]
  [graph N M] -> [make-graph N M])

(define vert-dict Graph -> (<-address Graph 1))

(define edge-dict Graph -> (<-address Graph 2))

(define vertices
  {graph --> (list A)}
  Graph -> (keys (vert-dict Graph)))

(define edges
  {graph --> (list (list A))}
  Graph -> (keys (edge-dict Graph)))

(define get-data
  Value V -> (if (tuple? Value)
                 (fst Value)
                 (error (make-string "no data for ~S~%" V))))

(define vertex-data
  Graph V -> (get-data (<-dict (vert-dict Graph) V) V))

(define edge-data
  Graph V -> (get-data (<-dict (edge-dict Graph) V) V))

(define resolve
  {(vector (list A)) --> (@p number number) --> A}
  Vector (@p Index Place) -> (nth (+ 1 Place) (<-vector Vector Index)))

(define resolve-vert
  {graph --> (@p number number) --> A}
  Graph Place -> (resolve (<-address (vert-dict Graph) 2) Place))

(define resolve-edge
  {graph --> (@p number number) --> A}
  Graph Place -> (resolve (<-address (edge-dict Graph) 2) Place))

(define edges-for
  {graph --> A --> (list (list A))}
  Graph Vert -> (let Val (trap-error (<-dict (vert-dict Graph) Vert) (/. E []))
                     Edges (if (tuple? Val) (snd Val) Val)
                  (map (lambda X (fst (resolve-edge Graph X))) Val)))

(define add-vertex-w-data
  \* add a vertex to a graph *\
  {graph --> A --> B --> A}
  G V Data -> (do (dict-> (vert-dict G) V (@p Data (edges-for G V))) V))

(define add-vertex-w/o-data
  \* add a vertex to a graph *\
  {graph --> A --> B --> A}
  G V -> (do (dict-> (vert-dict G) V (edges-for G V)) V))

(defmacro add-vertex-macro
  [add-vertex G V]   -> [add-vertex-w/o-data G V]
  [add-vertex G V D] -> [add-vertex-w-data G V D])

(define update-vert
  \* in a dict, add an edge to a vertex's edge list *\
  {vector --> (@p number number) --> A --> number}
  Vs Edge V -> (let Store (<-address Vs 2)
                    N (hash V (limit Store))
                    VertLst (trap-error (<-vector Store N) (/. E []))
                    Contents (trap-error (<-dict Vs V) (/. E []))
                 (do (dict-> Vs V (if (tuple? Contents)
                                      (@p (fst Contents)
                                          (adjoin Edge (snd Contents)))
                                      (adjoin Edge Contents)))
                     (@p N (length VertLst)))))

(define update-edges-vertices
  \* add an edge to a graph *\
  {graph --> (list A) --> (list A)}
  Graph Edge ->
  (let Store (<-address (edge-dict Graph) 2)
       EdgeID (hash Edge (limit Store))
       EdgeLst (trap-error (<-vector Store EdgeID) (/. E []))
    (map (update-vert (vert-dict Graph) (@p EdgeID (length EdgeLst))) Edge)))

(define add-edge-w-data
  G E D -> (do (dict-> (edge-dict G) E (@p D (update-edges-vertices G E))) E))

(define add-edge-w/o-data
  G E -> (do (dict-> (edge-dict G) E (update-edges-vertices G E)) E))

(defmacro add-edge-macro
  [add-edge G E]   -> [add-edge-w/o-data G E]
  [add-edge G E V] -> [add-edge-w-data G E V])

(define has-edge?
  {graph --> (list A) --> boolean}
  Graph Edge -> (key? (edge-dict Graph) Edge))

(define has-vertex?
  {graph --> A --> boolean}
  Graph Vertex -> (key? (vert-dict Graph) Vertex))

(define neighbors
  \* Return the neighbors of a vertex *\
  {graph --> A --> (list A)}
  Graph Vert -> (unique (mapcon (remove-first Vert) (edges-for Graph Vert))))

(define connected-to-
  {graph --> (list A) --> (list A) --> (list A)}
  Graph [] Already -> Already
  Graph New Already ->
  (let Reachable (unique (mapcon (neighbors Graph) New))
       New (difference Reachable Already)
    (connected-to- Graph New (append New Already))))

(define connected-to
  \* return all vertices connected to the given vertex, including itself *\
  {graph --> A --> (list A)}
  Graph V -> (connected-to- Graph [V] [V]))

(define connected?
  \* return if a graph is fully connected *\
  {graph --> boolean}
  Graph -> (reduce (/. V Acc
                       (and Acc
                            (subset? (vertices Graph) (connected-to Graph V))))
                   true (vertices Graph)))

(define connected-components-
  \* given a graph return a list of connected components *\
  {graph --> (list A) --> (list (list A)) --> (list graph)}
  Graph [] _ -> []
  Graph VS [] -> (map (/. V (let Component (graph 1 0)
                              (do (add-vertex Component V) Component)))
                      VS)
  Graph [V|VS] ES ->
    (let Con-verts (connected-to Graph V)
         Con-edges (filter (/. E (subset? E Con-verts)) ES)
         Component (graph (length Con-verts) (length Con-edges))
      (do (map (add-edge-w/o-data Component) Con-edges)
          (cons Component (connected-components- Graph
                                                 (difference VS Con-verts)
                                                 (difference ES Con-edges))))))

(define connected-components
  {graph --> (list graph)}
  Graph -> (connected-components- Graph (vertices Graph) (edges Graph)))

(define place-vertex
  \* given a graph, vertex and list of partitions, partition the vertex *\
  {graph --> A --> (list (list A)) --> (list (list A))}
  Graph V [] -> (if (element? V (neighbors Graph V))
                    (simple-error
                     (make-string "self-loop ~S, no vertex partition" V))
                    [[V]])
  Graph V [C|CS] -> (let Neighbors (neighbors Graph V)
                      (if (element? V Neighbors)
                          (simple-error
                           (make-string "self-loop ~S, no vertex partition" V))
                          (if (empty? (intersection C Neighbors))
                              [[V|C]|CS]
                              [C|(place-vertex Graph V CS)]))))

(define vertex-partition
  \* partition the vertices of a graph *\
  {graph --> (list (list A))}
  Graph -> (reduce (place-vertex Graph) [] (vertices Graph)))

(define bipartite?
  \* check if a graph is bipartite *\
  {graph --> boolean}
  Graph -> (= 2 (length (vertex-partition Graph))))

)

\* simple tests

(set g (graph))
(add-edge (value g) [chris patton])
(add-edge (value g) [eric chris])
(add-vertex (value g) nobody)
(has-edge? (value g) [patton chris])
(edges-for (value g) chris)
(neighbors (value g) chris)
(neighbors (value g) nobody)
(connected-to (value g) chris)
(connected? (value g))
(connected-components (value g)) <- fail when package wrapper is used
(map (function vertices) (connected-components (value g)))

*\