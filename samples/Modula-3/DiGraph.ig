(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep  5 12:17:52 PDT 1996 by detlefs                  *)

GENERIC INTERFACE DiGraph(NodeVal, EdgeVal);

(* A parameterized directed graph abstraction.

   Requires:
     NodeVal.T: TYPE
     NodeVal.Hash: PROCEDURE(nv: NodeVal.T; lessThan: CARDINAL): CARDINAL;
     NodeVal.Equal: PROCEDURE(nv1, nv2: NodeVal.T): BOOLEAN;
     NodeVal.Compare: PROCEDURE(nv1, nv2: NodeVal.T): [-1..1];

     NodeVal.Equal(n1, n2) => NodeVal.Hash(n1, m) = NodeVal.Hash(n2, m)
     NodeVal.Equal(n1, n2) <=> NodeVal.Compare(n1, n2) = 0


   A DiGraph.T represents a directed graph whose nodes and edges are 
   labelled with values of types NodeVal and EdgeVal, respectively. 
   A DiGraph.T is initially empty; clients are allowed to add and 
   delete nodes and edges.  Observer functions allow clients to obtain 
   the nth predecessor or successor of a node, or to iterate over 
   all the predecessors or successors.  The most interesting function 
   provided by DiGraph is TransitiveClose; this is a generalized 
   transitive closure algorithm that can, with appropriate arguments, 
   add sufficient edges to transitively close a graph or, 
   alternatively, compute the shortest paths in a graph whose edges 
   are labelled with integers.

   The representation of DiGraph.T requires that the NodeVal type 
   provides hash and comparison operations.  A hash table maps NodeVal
   values into nodes in the graph.  Nodes contain lists of edges to 
   successors and predecessors.  Because of this edge-list 
   implementation, this implementation of DiGraph is biased towards 
   sparse graphs.

   I hope eventually to have this interface support a number of 
   interesting graph algorithms, and to modify it to use the new 
   Vesta/Vulcan method of parameterization.  Any contributions of 
   work on this code is strongly welcomed.

   Index: graph; directed graph; network; transitive closure; relation
*)


IMPORT Wr, RefList;

EXCEPTION
  NoSuchNode; NoSuchEdge; DupNode; DupEdge; RangeFault; BadSemiGroup;

TYPE
  (* Used in transitive closure. *)
  ClosedSemiRing = OBJECT
    plusIdent, bottom: EdgeVal.T;
   METHODS
    init(): ClosedSemiRing;
    plus(ev1, ev2: EdgeVal.T): EdgeVal.T;
    times(ev1, ev2: EdgeVal.T): EdgeVal.T;
    closure(ev: EdgeVal.T): EdgeVal.T;
  END (* OBJECT *);

  (* These are used in printing. *)
  (* A PrintProc should take a node or edge and print it on 'wr', and print
     exactly 'width' characters on 'wr', truncating or padding with blanks as
     necessary. *)
  NodePrintProc = PROCEDURE(wr: Wr.T; nv: NodeVal.T; width: CARDINAL);
  EdgePrintProc = PROCEDURE(wr: Wr.T; exists: BOOLEAN; ev: EdgeVal.T;
                            width: CARDINAL);
  (* The 'exists' argument indicates whether an edge exists. *)

TYPE
  EdgeMapProc = PROCEDURE(n1: NodeVal.T; e: EdgeVal.T; n2: NodeVal.T);
  NodeMapProc = PROCEDURE(n: NodeVal.T);


TYPE
  EdgePublic = OBJECT
    from, to: Node;
    value: EdgeVal.T;
  END (* OBJECT *);
  Edge <: EdgePublic;

  NodePublic = OBJECT
    value: NodeVal.T;
  END (* OBJECT *);
  Node <: NodePublic;

  TPublic = OBJECT
   METHODS
    init(csr: ClosedSemiRing := NIL;
         undoable := FALSE): T;
    (* Creates a new, empty graph (one with no nodes or edges.)
       "csr" is the closed semi-ring to use for transitive closure
       operations, and "undoable" indicates that operations should be
       undoable.
    *)

    nodeSize(): CARDINAL;
    (* Returns the number of nodes in 'self'. *)

    edgeSize(): CARDINAL;
    (* Returns the number of edges in 'self'. *)

    nodeExists(nv: NodeVal.T): BOOLEAN;
    (* Returns TRUE iff there exists a node 'n' in the 'g' such that
       NodeVal.Equal(n, nv) = TRUE. *)

    addNode(nv: NodeVal.T) RAISES { DupNode };
    (* If self.nodeExists(nv), raises NodeExists and does not modify 'self'.
       Otherwise, adds a node with value 'nv' (and no successors or
       predecessors) to 'self'.
    *)
    
    deleteNode(nv: NodeVal.T) RAISES { NoSuchNode};
    (* If self.nodeExists(nv), deletes the node associated with 'nv'
       and all incoming edges to and outgoing edges from that node from
       'self'.  Otherwise, raises NoSuchNode and does not modify 'self'.
    *)

    addEdge(node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T;
            addNodes := FALSE)
        RAISES { NoSuchNode, DupEdge };
    (* If 'addNodes' is FALSE, and either of self.nodeExists(node1) or
       self.nodeExists(node2) is FALSE, then raises NoSuchNode.  If 'addNodes'
       is TRUE, adds 'node1' and/or 'node2' to 'self' if necessary to
       ensure that self.nodeExists(node1) and self.nodeExists(node2).  Next, if
       self.edgeExists(node1, node2), raises EdgeExists ('self' is not
       modified in this case, since if a node was added in the first
       step then there could not have been an edge between the input
       nodes.).  Otherwise, creates such an edge in 'self' and gives
       it the value 'edgeVal.'
    *)

    edgeExists(node1, node2: NodeVal.T): BOOLEAN;
    (* If self.nodeExists(node1) and self.nodeExists(node2) and an edge is
       presently defined between the nodes associated with those values in
       'self', returns TRUE; otherwise, returns FALSE.
    *)

    getEdge(node1, node2: NodeVal.T; VAR (*OUT*) ev: EdgeVal.T): BOOLEAN;
    (* If self.nodeExists(node1) and self.nodeExists(node2) and an edge is
       presently defined between the nodes associated with those values in
       'self', returns TRUE and sets 'ev' to the value of that edge; otherwise,
       returns FALSE.
    *)

    edgeValue(node1, node2: NodeVal.T): EdgeVal.T
        RAISES { NoSuchNode, NoSuchEdge };
    (* If NOT self.nodeExists(node1) or self.nodeExists(node2), raises
       NoSuchNode and does not modify 'self'.  If the nodes exist but
       NOT self.edgeExists(node1, node2), raises NoSuchEdge and does not
       modify 'self'.  Otherwise, if the nodes and an edge exist, returns the
       value associated with that edge.
    *)


    deleteEdge(node1, node2: NodeVal.T) RAISES { NoSuchNode, NoSuchEdge };
    (* If NOT self.nodeExists(node1) or NOT self.nodeExists(node2), raises
       NoSuchNode and does not modify 'self'.  If the nodes exist but
       NOT self.edgeExists(node1, node2), raises NoSuchEdge and does not
       modify 'self'.  Finally, if the nodes and an edge exist, deletes the
       edge between the nodes.
    *)


    setEdge(node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T)
        RAISES { NoSuchNode };
    (* If NOT self.nodeExists(node1) or NOT self.nodeExists(node2), raises
       NoSuchNode and does not modify 'self'.  If the nodes exist but
       NOT self.edgeExists(node1, node2), creates a new edge between 'node1'
       and 'node2' with value 'edgeVal.'  If an edge already exists between the
       nodes, sets its value to 'edgeVal.'
    *)

    changeEdge(node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T)
        RAISES { NoSuchNode, NoSuchEdge };
    (* If NOT self.nodeExists(node1) or NOT self.nodeExists(node2), raises
       NoSuchNode and does not modify 'self'.  If the nodes exist but
       NOT self.edgeExists(node1, node2), raises NoSuchEdge and does not
       modify 'self'.  Finally, if the nodes and an edge exist,
       changes the value of the edge to 'edgeVal.'
    *)

    nSucc(nodeVal: NodeVal.T): CARDINAL RAISES { NoSuchNode };
    (* If self.nodeExists(nodeVal), returns the number of successors
       of that node; otherwise, signals NoSuchNode.
    *)

    getSuccN(nodeVal: NodeVal.T; n: CARDINAL): NodeVal.T
        RAISES { NoSuchNode, RangeFault };
    (* If self.nodeExists(nodeVal) and that node has 'n'-1 or more successors,
       returns the NodeVal associated with the 'n'-th (0-based) successor.
       If NOT self.nodeExists(nodeVal), raises NoSuchNode; if n < 0 or n >= the
       number of successors of the node associated with 'nodeVal', raises
       RangeFault.
    *)

    getSuccIter(nodeVal: NodeVal.T): NodeIter RAISES { NoSuchNode };
    (* If self.nodeExists(nodeVal), returns a NodeIter that will yield all the
       successors of that node (assuming that the graph is not modified during
       the iteration.)  If NOT self.nodeExists(nodeVal), raises NoSuchNode.
    *)

    getSuccList(nodeVal: NodeVal.T): RefList.T (* OF Edge *)
        RAISES { NoSuchNode };
    (* Returns the list of edges emanating from "nodeVal". *)

    nPred(nodeVal: NodeVal.T): CARDINAL RAISES { NoSuchNode };
    (* If self.nodeExists(nodeVal), returns the number of successors
       of that node; otherwise, signals NoSuchNode.
    *)

    getPredN(nodeVal: NodeVal.T; n: CARDINAL): NodeVal.T
        RAISES { NoSuchNode, RangeFault };
    (* If self.nodeExists(nodeVal) and that node has 'n'-1 or more
       predecessors, returns the NodeVal associated with the 'n'-th
       (0-based) predecessor.  If NOT self.nodeExists(nodeVal), raises
       NoSuchNode; if n < 0 or n >= the number of predecessors of the
       node associated with 'nodeVal', raises RangeFault.
    *)


    getPredIter(nodeVal: NodeVal.T): NodeIter RAISES { NoSuchNode };
    (* If self.nodeExists(nodeVal), returns a NodeIter that will yield all the
       predecessors of that node (assuming that the graph is not
       modified during the iteration.)  If NOT
       self.nodeExists(nodeVal), raises NoSuchNode.
    *)

    getPredList(nodeVal: NodeVal.T): RefList.T (* OF Edge *)
        RAISES { NoSuchNode };
    (* Returns the list of edges terminating at "nodeVal". *)

    mapOverNodes(nmp: NodeMapProc) RAISES ANY;
    (* Applies 'nmp' to every Node in 'self.' *)

    mapOverEdges(emp: EdgeMapProc) RAISES ANY;
    (* Applies 'emp' to every edge in 'self.' *)

    transitiveClose(edgeChange: EdgeMapProc := NIL): BOOLEAN;

    (* Modifies 'self' so that the final value of 'self' is the
       transitive closure of the initial value.  If "csr" is
       non-"NIL", then it must specify a valid "closed semi-ring" on
       the edge type.  See Section 5.5, p. 198, "The Design and
       Analysis of Computer Algorithms", by Aho, Hopcroft, and Ullman,
       Addison-Wesley, 1974 for a complete explanation.  If "csr" is
       "NIL", then it is a checked runtime if "EdgeVal" is any type
       other than "NULL"; if "EdgeVal" is "NULL", then
       "transitiveClose" closes the graph by inserting "NIL"-valued
       edges as necessary.

       Actually, one addition has been made to the algorithm in AHU: a
       closed semi-ring may specify a distinguished element "etBottom"
       of the "EdgeVal" type that is 'contagious' in the plus and
       times operations; if transitive closure gives any edge the
       value "etBottom", then "transitiveClose" stops and returns
       "FALSE".  OTherwise, completes the closure and returns "TRUE".
       Typically "etBottom" is returned by the closure operation.

       To give a concrete example, assume that the edge values are
       non-negative real numbers, and we want to compute the shortest
       paths between nodes.  Here we use MIN as the 'Plus' operation
       of the semi-ring, addition as the 'Times' operation, and
       +infinity as the identity of the Plus operation.  More specifically:

       'etPlusIdent': +infinity

       'etPlus': Returns the minimum of 'e1' and 'e2', unless one of 'e1'
       or 'e2' is 'etPlusIdent', in which case returns the other.

       'etTimes': confusingly, here, addition.  Returns the sum of
       'e1' and 'e2', unless one is 'etPlusIdent' in which case it
       returns that.

       'etClosure': In general, should be
         etPlus(etTimesIdent, e, etTimes(e, e), etTimes(e, etTimes(e, e)), ...)
       where 'etTimesIdent' is the identity element for the times
       operator.  In our example, Times is addition, so the identity
       is 0, so the closure operation is
         MIN(0, e, e+e, e+e+e, ...)
       or 0.

       If we did the same problem over all real numbers, not just
       non-negative numbers, we could use 'etBottom' to detect
       negative-weight cycles.  'etBottom' is would represent -infinity,
       and we could make 'etClosure(e)' return 'etBottom' when 'e' is
       negative.
    *)

    addEdgeAndClose(n1: NodeVal.T; ev: EdgeVal.T; n2: NodeVal.T;
                    addNodes := FALSE;
                    edgeChange: EdgeMapProc := NIL): BOOLEAN;
    (* "Conceptually" adds an edge of value 'ev' between 'n1' and 'n2', and
       transitively closes the graph according to the closed semi-ring 'csr'.
       That is, uses the "Plus" operation of csr to compute the new value of
       the edge from the old one, if any, and added edge value, and propogates
       any changes made through the graph.
    *)

    topSort(VAR (*OUT*) nodes: REF ARRAY OF NodeVal.T): BOOLEAN;
    (* If the graph is acyclic, returns "TRUE" and sets "nodes" to be
       an array containing all the nodes in a topological order; that
       is, if there is an edge from node "a" to node "b" in the graph,
       then "a" precedes "b" in the array.  If the graph contains a
       cycle, returns "FALSE" and sets "nodes" to be an array
       containing a cycle. *)

    printAsMatrix(wr: Wr.T;
                  np: NodePrintProc; ep: EdgePrintProc;
                  between, colWidth: CARDINAL;
                  absentEV: EdgeVal.T);
    (* Prints "self" in adjacency matrix form.  That is, prints a
       matrix whose rows and columns are labelled with node values and
       whose cells are labelled with edge values.  printAsMatrix
       writes its output to "wr", and calls "np" and "ep" to do the
       printing.  "np" is called with "wr", a "NodeVal", and "colwidth"
       as arguments.  "ep" is called with "wr", a BOOLEAN that is TRUE
       IFF there is an edge between the nodes corresponding by the
       cell in the matrix, the EdgeVal for that cell if it exists
       (else "absentEV"), and "colWidth".  The order of the nodes in
       the rows and columns is determined by the NodeVal.Compare
       passed to New.
    *)

    push();
    (* Saves a state of the graph.  Requires the graph to be undoable. *)
    pop();
    (* Requires the graph to be undoable and for there to be a
       previously saved state; restores that state. *)

  END (* OBJECT *);
  T <: TPublic;

  (* Node Iterators. *)    
  NodeIter = OBJECT
   METHODS
    next(VAR nv: NodeVal.T): BOOLEAN;
    (* If there are Nodes in 'self' that have not yet been yielded, returns
       TRUE and sets 'next' to the next node to be yielded.  Otherwise, the
       iteration is complete and FALSE is returned.
    *)
  END (* OBJECT *);

END DiGraph.


