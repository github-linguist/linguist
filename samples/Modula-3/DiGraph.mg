(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep  5 12:16:01 PDT 1996 by detlefs                  *)

GENERIC MODULE DiGraph(NodeVal, EdgeVal);
(* The DiGraph type is parameterized over the types of the nodes and the
   edges. *) 

IMPORT RefList, Wr, Word, RefRefTbl, RefListSort, RefSeq;

IMPORT Thread;
<*FATAL Wr.Failure, Thread.Alerted*>

TYPE
  NodeValRef = REF NodeVal.T;

REVEAL
  Node = NodePublic BRANDED OBJECT
    succ, pred: RefList.T (* Of Edge *);
    misc: INTEGER;
  END;
TYPE
  NodeArr = REF ARRAY OF Node;
REVEAL
  Edge = EdgePublic BRANDED OBJECT
    nextValue : EdgeVal.T (* used in transitive closure *);
  END;

  T = TPublic BRANDED OBJECT
    nodeTbl: RefRefTbl.T; (* map from REF NodeVal's to nodes. *)
    edges: CARDINAL := 0;
    csr: ClosedSemiRing;
    undoable: BOOLEAN;
    undoSP: CARDINAL;
    undoStack: REF ARRAY OF UndoRec;

   METHODS
    nodeValToNode(nodeVal: NodeVal.T; addNodes: BOOLEAN): Node
        RAISES { NoSuchNode } := NodeValToNode;
    makeNodeArray(): NodeArr := MakeNodeArray;

   OVERRIDES
    init := TInit;
    nodeSize := NodeSize;
    edgeSize := EdgeSize;
    nodeExists := NodeExists;
    addNode := AddNode;
    deleteNode := DeleteNode;
    edgeExists := EdgeExists;
    getEdge := GetEdge;
    edgeValue := EdgeValue;
    addEdge := AddEdge;
    deleteEdge := DeleteEdge;
    setEdge := SetEdge;
    changeEdge := ChangeEdge;
    nSucc := NSucc;
    getSuccN := GetSuccN;
    getSuccIter := GetSuccIter;
    getSuccList := GetSuccList;
    nPred := NPred;
    getPredN := GetPredN;
    getPredIter := GetPredIter;
    getPredList := GetPredList;
    mapOverEdges := MapOverEdges;
    mapOverNodes := MapOverNodes;
    transitiveClose := TransitiveClose;
    addEdgeAndClose := AddEdgeAndClose;
    topSort := TopSort;
    printAsMatrix := PrintAsMatrix;
    push := Push;
    pop := Pop;
  END;


TYPE
  NodeIterImpl = NodeIter BRANDED OBJECT
    list: RefList.T;        (* Uniterated remainder of edge list. *)
    toNotFrom: BOOLEAN;  (* TRUE IF this is a 'succ' iter, FALSE if 'pred' *)
   OVERRIDES
    next := NodeIterNext;
  END (* OBJECT *);

  UndoType = { Mark, AddNode, DeleteNode, AddEdge, DeleteEdge, EdgeVal };
  UndoRec = RECORD
    type: UndoType;
    n: Node;
    e: Edge;
    ev: EdgeVal.T;
  END (* RECORD *);


PROCEDURE TInit(self: T; csr: ClosedSemiRing; undoable: BOOLEAN): T =
  BEGIN
    self.nodeTbl := NEW(RefRefTbl.Default,
                        keyHash := NodeValRefHash,
                        keyEqual := NodeValRefEqual).init();
    self.edges := 0;
    self.csr := csr;
    self.undoable := undoable;
    IF undoable THEN
      self.undoSP := 0;
      self.undoStack := NEW(REF ARRAY OF UndoRec, 100)
    END (* IF *);
    RETURN self;
  END TInit;

PROCEDURE NodeValRefHash(<*UNUSED*> t: RefRefTbl.T;
                         READONLY key: REFANY): Word.T =
  BEGIN
    RETURN NodeVal.Hash(NARROW(key, NodeValRef)^);
  END NodeValRefHash;
    
PROCEDURE NodeValRefEqual(<*UNUSED*> t: RefRefTbl.T;
                          READONLY key1, key2: REFANY): BOOLEAN =
  BEGIN
    RETURN NodeVal.Equal(NARROW(key1, NodeValRef)^,
                         NARROW(key2, NodeValRef)^);
  END NodeValRefEqual;
    
               
(* Should be INLINE *)
PROCEDURE NodeSize(self: T): CARDINAL =
  BEGIN
    RETURN self.nodeTbl.size()
  END NodeSize;


(* Should be INLINE *)
PROCEDURE EdgeSize(self: T): CARDINAL =
  BEGIN
    RETURN self.edges;
  END EdgeSize;


PROCEDURE NodeExists(self: T; nodeVal: NodeVal.T): BOOLEAN =
  VAR dummyVal: REFANY;
  BEGIN
    WITH nvr = NEW(NodeValRef) DO
      nvr^ := nodeVal;
      RETURN self.nodeTbl.get(nvr, dummyVal);
    END (* WITH *);
  END NodeExists;


PROCEDURE AddNode(self: T; nodeVal: NodeVal.T) RAISES { DupNode } =
  VAR n: Node;
      dummy: BOOLEAN;
  BEGIN
    IF self.nodeExists(nodeVal) THEN RAISE DupNode END;
    n := NEW(Node, value := nodeVal, succ := NIL, pred := NIL);
    WITH nvr = NEW(NodeValRef) DO
      nvr^ := nodeVal;
      dummy := self.nodeTbl.put(nvr, n);
      <*ASSERT NOT dummy*>
    END (* WITH *);
    IF self.undoable THEN PushUndo(self, UndoType.AddNode, n) END (* IF *)
  END AddNode;  


PROCEDURE DeleteNode(self: T; nodeVal: NodeVal.T) RAISES { NoSuchNode } =
  VAR node: Node;
      edge: Edge;
      preds, succs: RefList.T (* Of Edge *);
      dummy: BOOLEAN;
      resultRA: REFANY;
  BEGIN
    (* This raises an exception if the node doesn't exist. *)
    node := self.nodeValToNode(nodeVal, FALSE);

    VAR nvr := NEW(NodeValRef); BEGIN
      nvr^ := nodeVal;
      dummy := self.nodeTbl.delete(nvr, resultRA);
      (* If NodeValToNode said it was there, it ought to be there. *)
      <*ASSERT dummy*>
    END (* WITH *);
    IF self.undoable THEN
      PushUndo(self, UndoType.DeleteNode, node)
    END (* IF *);

    (* Delete node from the 'succs' list of each of its predecessors. *)
    preds := node.pred;
    WHILE preds # NIL DO
      edge := preds.head;
      dummy := DeleteFromEdgeList(edge.from.succ, FALSE, node);
      <*ASSERT dummy*>
      IF self.undoable THEN
        PushUndo(self, UndoType.DeleteEdge, NIL, edge)
      END (* IF *);
      DEC(self.edges);
      preds := preds.tail
    END;
    (* ...and also from the 'preds' list of each of its successors. *)
    succs := node.succ;
    WHILE succs # NIL DO
      edge := succs.head;
      dummy := DeleteFromEdgeList(edge.to.pred, TRUE, node);
      <*ASSERT dummy*>
      IF self.undoable THEN
        PushUndo(self, UndoType.DeleteEdge, NIL, edge)
      END (* IF *);
      DEC(self.edges);
      succs := succs.tail
    END;

  END DeleteNode;


(* INTERNAL *)

(* Returns a NodeArr (Array.T OF Node) of all the nodes.  If 'cp' is
   non-NIL, uses it to sort the array. *)

PROCEDURE MakeNodeArray(self: T): NodeArr =
  VAR newArr := NEW(NodeArr, self.nodeTbl.size());
      iter := self.nodeTbl.iterate();
      nodeVal, node: REFANY;
      rl: RefList.T := NIL;
  BEGIN
    WHILE iter.next(nodeVal, node) DO
      rl := RefList.Cons(node, rl)
    END (* WHILE *);
    rl := RefListSort.SortD(rl, NodeCompare);
    VAR i := 0; BEGIN
      WHILE rl # NIL DO
        newArr[i] := rl.head; INC(i); rl := rl.tail
      END (* WHILE *)
    END (* BEGIN *);
    RETURN newArr
  END MakeNodeArray;

PROCEDURE NodeCompare(node1Ref, node2Ref: REFANY): [-1..1] =
  VAR
    node1, node2: Node;
  BEGIN
    node1 := NARROW(node1Ref, Node);
    node2 := NARROW(node2Ref, Node);
    RETURN NodeVal.Compare(node1.value, node2.value);
  END NodeCompare;


(* EXTERNAL *)

PROCEDURE AddEdge(self: T;
                  node1: NodeVal.T; edgeVal: EdgeVal.T; node2: NodeVal.T;
                  addNodes: BOOLEAN := FALSE)
    RAISES { NoSuchNode, DupEdge } =
  VAR
    newEdge: Edge;
    fromNode, toNode: Node;
    edgeDummy: Edge;
  BEGIN
    (* These raise NoSuchNode when necessary. *)
    fromNode := self.nodeValToNode(node1, addNodes);
    toNode := self.nodeValToNode(node2, addNodes);

    (* Check to see if an edge exists... *)
    IF FindEdge(fromNode, toNode, edgeDummy) THEN RAISE DupEdge END;
    newEdge := NEW(Edge, value := edgeVal, from := fromNode, to := toNode);
    fromNode.succ := RefList.Cons(newEdge, fromNode.succ);
    toNode.pred := RefList.Cons(newEdge, toNode.pred);
    INC(self.edges);
    IF self.undoable THEN
      PushUndo(self, UndoType.AddEdge, NIL, newEdge)
    END (* IF *)
  END AddEdge;


(* INTERNAL *)
(* If addNodes is FALSE, and either of self.nodeExists(node1) or
   self.nodeExists(node2) is FALSE, then raises "NoSuchNode."  Otherwise, adds
   nodes corresponding to the values 'node1' and 'node2' to 'g' if no
   such nodes already exist, and returns those nodes in 'fromNode' and
   'toNode', respectively.
*)
PROCEDURE NodeValToNode(self: T; nodeVal: NodeVal.T;
                        addNodes: BOOLEAN): Node
    RAISES { NoSuchNode } =
  VAR nodeRA: REFANY;
  BEGIN
    WITH nvr = NEW(NodeValRef) DO
      nvr^ := nodeVal;
      IF NOT self.nodeTbl.get(nvr, nodeRA) THEN
        IF addNodes THEN
          self.addNode(nodeVal); <*NOWARN*>
          VAR dummy := self.nodeTbl.get(nvr, nodeRA); BEGIN
            <*ASSERT dummy*>
            RETURN nodeRA
          END (* BEGIN *)
        ELSE
          RAISE NoSuchNode;
        END (* IF *);
      ELSE
        RETURN nodeRA
      END (* IF *);
    END (* WITH *);
  END NodeValToNode;


(* EXTERNAL *)

PROCEDURE EdgeExists(self: T; node1, node2: NodeVal.T): BOOLEAN =
  VAR
    fromNode, toNode: Node;
    edgeDummy: Edge;
  BEGIN
    TRY
      fromNode := self.nodeValToNode(node1, FALSE);
      toNode := self.nodeValToNode(node2, FALSE);
    EXCEPT
    | NoSuchNode => RETURN FALSE;
    END;
    RETURN FindEdge(fromNode, toNode, edgeDummy);
  END EdgeExists;


PROCEDURE GetEdge(self: T; node1, node2: NodeVal.T;
                  VAR ev: EdgeVal.T): BOOLEAN =
  VAR fromNode, toNode: Node;
      edge: Edge;
  BEGIN
    TRY
      fromNode := self.nodeValToNode(node1, FALSE);
      toNode := self.nodeValToNode(node2, FALSE);
    EXCEPT
    | NoSuchNode => RETURN FALSE;
    END;
    IF NOT FindEdge(fromNode, toNode, edge) THEN
      RETURN FALSE;
    ELSE
      ev := edge.value;
      RETURN TRUE;
    END (* IF *);
  END GetEdge;


(* INTERNAL *)

(* Requires that 'fromNode' and 'toNode' are nodes in 'g'.  If no edge
   exists between 'fromNode' and 'toNode', returns FALSE; if such an edge
   does exist, return TRUE and the value of that edge in 'edgeVal'.
*)
PROCEDURE FindEdge(fromNode, toNode: Node;
                   VAR (*OUT*) edge: Edge): BOOLEAN =
  VAR
    succs: RefList.T (* OF Edge *);
  BEGIN
    succs := fromNode.succ;
    WHILE succs # NIL DO
      edge := succs.head;
      IF edge.to = toNode THEN RETURN TRUE; END;
      succs := succs.tail
    END;
    RETURN FALSE;
  END FindEdge;


(* EXTERNAL *)

PROCEDURE EdgeValue(self: T; node1, node2: NodeVal.T): EdgeVal.T
    RAISES { NoSuchNode, NoSuchEdge } =
  VAR
    fromNode, toNode: Node;
    edge: Edge;
  BEGIN
    (* These raise NoSuchNode. *)
    fromNode := self.nodeValToNode(node1, FALSE);
    toNode := self.nodeValToNode(node2, FALSE);
    IF NOT FindEdge(fromNode, toNode, edge) THEN
      RAISE NoSuchEdge;
    ELSE
      RETURN edge.value;
    END;
  END EdgeValue;


PROCEDURE DeleteEdge(self: T; node1, node2: NodeVal.T)
    RAISES { NoSuchNode, NoSuchEdge } =
  VAR
    fromNode, toNode: Node;
    foundFrom, foundTo: BOOLEAN;
  BEGIN
    (* These raise NoSuchNode. *)
    fromNode := self.nodeValToNode(node1, FALSE);
    toNode := self.nodeValToNode(node2, FALSE);

    IF self.undoable THEN
      VAR edge: Edge; BEGIN
        IF FindEdge(fromNode, toNode, edge) THEN
          PushUndo(self, UndoType.DeleteEdge, NIL, edge)
        ELSE
          RAISE NoSuchEdge
        END (* IF *)
      END (* BEGIN *)
    END (* IF *);

    foundFrom := DeleteFromEdgeList(fromNode.succ, FALSE, toNode);
    foundTo := DeleteFromEdgeList(toNode.pred, TRUE, fromNode);
    IF foundFrom THEN
      <*ASSERT foundTo*>
      DEC(self.edges)
    ELSE
      <*ASSERT NOT foundTo*>
      RAISE NoSuchEdge;
    END;
  END DeleteEdge;


(* INTERNAL *)

(* Attempts to deletes an edge whose "target" is 'targetNode' from
   'realEdges'.  If 'targetIsFromNode' is TRUE, "target" is interpreted
   to mean the "from" field of an edge, else the "to" field.  Returns
   TRUE iff found and deleted a matching edge. *)

PROCEDURE DeleteFromEdgeList(VAR realEdges: RefList.T (* Of Edge *);
                             targetIsFromNode: BOOLEAN;
                             targetNode: Node): BOOLEAN =
  VAR edges, prevEdges: RefList.T (* Of Edge *);
      edge: Edge;
  BEGIN
    prevEdges := NIL;
    IF realEdges = NIL THEN RETURN FALSE; END;
    edges := realEdges;
    WHILE edges # NIL DO
      edge := edges.head;
      IF targetIsFromNode AND (edge.from = targetNode) THEN
        IF prevEdges = NIL THEN realEdges := edges.tail
        ELSE prevEdges.tail := edges.tail
        END;
        RETURN TRUE;
      ELSIF (NOT targetIsFromNode) AND (edge.to = targetNode) THEN
        IF prevEdges = NIL THEN realEdges := edges.tail
        ELSE prevEdges.tail := edges.tail
        END;
        RETURN TRUE;
      END;
      prevEdges := edges; edges := edges.tail;
    END;
    RETURN FALSE;
  END DeleteFromEdgeList;


(* EXTERNAL *)

PROCEDURE ChangeEdge(self: T; node1: NodeVal.T;
                     edgeVal: EdgeVal.T; node2: NodeVal.T)
            RAISES { NoSuchNode, NoSuchEdge } =
  VAR
    fromNode, toNode: Node;
    edge: Edge;
  BEGIN
    (* These raise NoSuchNode. *)
    fromNode := self.nodeValToNode(node1, FALSE);
    toNode := self.nodeValToNode(node2, FALSE);
    IF NOT FindEdge(fromNode, toNode, edge) THEN
      RAISE NoSuchEdge;
    ELSE
      IF self.undoable THEN PushEdgeVal(self, edge, edge.value) END (* IF *);
      edge.value := edgeVal;
    END;
  END ChangeEdge;


PROCEDURE SetEdge(self: T; node1: NodeVal.T;
                  edgeVal: EdgeVal.T; node2: NodeVal.T)
            RAISES { NoSuchNode } =
  VAR
    fromNode, toNode: Node;
    edge: Edge;
  BEGIN
    (* These raise NoSuchNode. *)
    fromNode := self.nodeValToNode(node1, FALSE);
    toNode := self.nodeValToNode(node2, FALSE);
    IF NOT FindEdge(fromNode, toNode, edge) THEN
      edge := NEW(Edge, value := edgeVal, from := fromNode, to := toNode);
      fromNode.succ := RefList.Cons(edge, fromNode.succ);
      toNode.pred := RefList.Cons(edge, toNode.pred);
      IF self.undoable THEN
        PushUndo(self, UndoType.AddEdge, NIL, edge)
      END (* IF *);
      INC(self.edges);
    ELSE
      IF self.undoable THEN PushEdgeVal(self, edge, edge.value) END (* IF *);
      edge.value := edgeVal;
    END;
  END SetEdge;


PROCEDURE NSucc(self: T; nodeVal: NodeVal.T): CARDINAL
    RAISES { NoSuchNode } =
  BEGIN
    RETURN RefList.Length(self.nodeValToNode(nodeVal, FALSE).succ);    
  END NSucc;
  

PROCEDURE GetSuccN(self: T; nodeVal: NodeVal.T; n: CARDINAL): NodeVal.T
    RAISES { NoSuchNode, RangeFault } =
  VAR
    node: Node;
  BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    IF (n < 0) OR (n >= RefList.Length(node.succ)) THEN
      RAISE RangeFault;
    ELSE
      RETURN NARROW(RefList.Nth(node.succ, n), Edge).to.value;
    END;
  END GetSuccN;

PROCEDURE GetSuccIter(self: T; nodeVal: NodeVal.T): NodeIter
    RAISES { NoSuchNode } =
  VAR
    node: Node;
    ni: NodeIter;
  BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    ni := NEW(NodeIterImpl, toNotFrom := TRUE, list := node.succ);
    RETURN ni;
  END GetSuccIter;

PROCEDURE GetSuccList(self: T; nodeVal: NodeVal.T): RefList.T
    RAISES { NoSuchNode } =
  VAR node: Node; BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    RETURN node.succ
  END GetSuccList;

PROCEDURE NPred(self: T; nodeVal: NodeVal.T): CARDINAL
    RAISES { NoSuchNode } =
  BEGIN
    RETURN RefList.Length(self.nodeValToNode(nodeVal, FALSE).pred);    
  END NPred;

PROCEDURE GetPredN(self: T; nodeVal: NodeVal.T; n: CARDINAL): NodeVal.T
    RAISES { NoSuchNode, RangeFault } =
  VAR
    node: Node;
  BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    IF (n < 0) OR (n >= RefList.Length(node.pred)) THEN
      RAISE RangeFault;
    ELSE
      RETURN NARROW(RefList.Nth(node.pred, n), Edge).from.value;
    END;
  END GetPredN;

PROCEDURE GetPredIter(self: T; nodeVal: NodeVal.T): NodeIter
    RAISES { NoSuchNode } =
  VAR
    node: Node;
    ni: NodeIter;
  BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    ni := NEW(NodeIterImpl, toNotFrom := FALSE, list := node.pred);
    RETURN ni;
  END GetPredIter;

PROCEDURE GetPredList(self: T; nodeVal: NodeVal.T): RefList.T
    RAISES { NoSuchNode } =
  VAR node: Node; BEGIN
    node := self.nodeValToNode(nodeVal, FALSE);
    RETURN node.succ
  END GetPredList;

PROCEDURE NodeIterNext(self: NodeIterImpl; VAR next: NodeVal.T): BOOLEAN =
  VAR
    edge: Edge;
  BEGIN
    IF self.list = NIL THEN RETURN FALSE; END;
    edge := self.list.head;
    self.list := self.list.tail;
    IF self.toNotFrom THEN
      next := edge.to.value;
    ELSE
      next := edge.from.value;
    END;
    RETURN TRUE;
  END NodeIterNext;


(*==================== Whole-Graph Iteration ====================*)

PROCEDURE SetMiscs(g: T; i: INTEGER) =
  VAR iter := g.nodeTbl.iterate();
      nodeVal, nodeRA: REFANY;
  BEGIN
    WHILE iter.next(nodeVal, nodeRA) DO
      VAR node: Node := nodeRA; BEGIN
        node.misc := i
      END (* BEGIN *)
    END (* WHILE *)
  END SetMiscs;

PROCEDURE MapOverEdges(self: T; emp: EdgeMapProc) RAISES ANY =
  VAR iter := self.nodeTbl.iterate(); nodeVal, nodeRA: REFANY; BEGIN
    SetMiscs(self, 0);
    WHILE iter.next(nodeVal, nodeRA) DO
      DfsEdges(nodeRA, emp)
    END (* WHILE *);
    SetMiscs(self, 0);
  END MapOverEdges;

PROCEDURE DfsEdges(node: Node; emp: EdgeMapProc) RAISES ANY =
  BEGIN
    IF node.misc = 0 THEN
      VAR succs := node.succ; BEGIN
        WHILE succs # NIL DO
          VAR e: Edge := succs.head; BEGIN
            emp(node.value, e.value, e.to.value);
            node.misc := 1;
            DfsEdges(e.to, emp);
          END (* BEGIN *);
          succs := succs.tail
        END (* WHILE *)
      END (* BEGIN *)
    END (* IF *);
  END DfsEdges;


PROCEDURE MapOverNodes(self: T; nmp: NodeMapProc) =
  VAR iter := self.nodeTbl.iterate(); nodeValRA, nodeRA: REFANY; BEGIN
    WHILE iter.next(nodeValRA, nodeRA) DO
      VAR nodeVal: NodeValRef := nodeValRA; BEGIN
        nmp(nodeVal^)
      END (* BEGIN *)
    END (* WHILE *)
  END MapOverNodes;

(*
PROCEDURE DfsNodes(ra: REFANY; <*UNUSED*> key: REFANY;
                   VAR nodeRA: REFANY): BOOLEAN RAISES ANY =
  BEGIN
    WITH node = NARROW(nodeRA, Node),
         nmpRR = NARROW(ra, NMPRefRec) DO
      DfsNodesMap(node, nmpRR.proc);
    END (* WITH *);
    RETURN FALSE;
  END DfsNodes;

PROCEDURE DfsNodesMap(n: Node; nmp: NodeMapProc) RAISES ANY =
  VAR succs: RefList.T (* OF Edge *);
  BEGIN
    IF n.misc = 1 THEN RETURN;
    ELSE
      n.misc := 1;
      nmp(n.value);
      succs := n.succ;
      WHILE succs # NIL DO
        VAR e: Edge := succs.head; BEGIN
          DfsNodesMap(e.to, nmp);
        END (* BEGIN *)
      END (* WHILE *)
    END (* IF *);
  END DfsNodesMap;
*)

(*====================== Transitive closure ======================*)
(* Modifies 'g' so that the final value of 'g' is the transitive closure
   of the initial value.   If all of etPlus, etTimes, etPlusIdent, and
   etTimesIdent are NIL, then edge with value NIL is added between nodes
   'n1' and 'n2' iff no edge connected them in the original value of 'g',
   but a path between 'n1' and 'n2' did exist in that original value.
   If any of the optional arguments are non-NIL, all must be, and they
   must form a "closed semi-ring" on the edge type.  We then run algorithm
   5.5, p. 198, "The Design and Analysis of Computer Algorithms", by Aho,
   Hopcroft, and Ullman, Addison-Wesley, 1974.
*)


PROCEDURE TransitiveClose(self: T; edgeChange: EdgeMapProc := NIL): BOOLEAN =
  VAR nodei, nodej, nodek: Node;
      edge, kkedge, ikedge, ijedge, kjedge: Edge;
      kkValClosure, ikVal, oldijVal, newijVal, kjVal: EdgeVal.T;
      succs: RefList.T (* OF Edge *);
      nodeArr: NodeArr;
      nNodes: CARDINAL;
  BEGIN
    <*ASSERT self.csr # NIL *>
    (* Repack the array so we can index the nodes. *)
    nodeArr := self.makeNodeArray();
    nNodes := self.nodeSize();
    (* I'm going to code up an algorithm that assumes a sparse graph,
       where most of the values are represented by the lack of an edge
       (which corresponds to self.csr.plusIdent).  We might want to
       measure the number of edges against the number of nodes, and
       decide whether to do a "dense" version, in which we allocate an
       n^2 array... *) 
    FOR k := 0 TO nNodes-1 DO
      nodek := nodeArr[k];
      
      IF NOT FindEdge(nodek, nodek, kkedge) THEN
        kkValClosure := self.csr.closure(self.csr.plusIdent);
      ELSE
        kkValClosure := self.csr.closure(kkedge.value);
      END;
      IF kkValClosure = self.csr.bottom THEN RETURN FALSE END (* IF *);

      FOR i := 0 TO nNodes-1 DO
        nodei := nodeArr[i];

        IF NOT FindEdge(nodei, nodek, ikedge) THEN
          ikVal := self.csr.plusIdent;
        ELSE
          ikVal := ikedge.value;
        END;

        FOR j := 0 TO nNodes-1 DO
          nodej := nodeArr[j];
          IF NOT FindEdge(nodei, nodej, ijedge) THEN
            oldijVal := self.csr.plusIdent;
          ELSE
            oldijVal := ijedge.value;
          END;

          IF NOT FindEdge(nodek, nodej, kjedge) THEN
            kjVal := self.csr.plusIdent;
          ELSE
            kjVal := kjedge.value;
          END;
    
          newijVal := self.csr.plus(
                               oldijVal,
                               self.csr.times(ikVal,
                                              self.csr.times(kkValClosure,
                                                             kjVal)));
          IF (newijVal # self.csr.plusIdent) THEN
            (* There needs to be an edge... *)
            IF (oldijVal = self.csr.plusIdent) THEN
              (* ...but there was no edge before, so make one. *)
              ijedge := NEW(Edge);
              (* To make sure rest of this iteration is right. *)
              ijedge.value := self.csr.plusIdent;
              ijedge.nextValue := newijVal;
              ijedge.from := nodei;
              ijedge.to := nodej;
              nodei.succ := RefList.Cons(ijedge, nodei.succ);
              nodej.pred := RefList.Cons(ijedge, nodej.pred);
              IF self.undoable THEN
                PushUndo(self, UndoType.AddEdge, NIL, ijedge)
              END (* IF *);
              IF edgeChange # NIL THEN
                edgeChange(nodei.value, ijedge.value, nodej.value)
              END (* IF *);
              INC(self.edges);
            ELSE
              (* ...and there is. *)
              ijedge.nextValue := newijVal;
            END;
          END;
        END;
      END;

      (* Now update the 'values' of the edges to the 'nextValues.' *)
      FOR i := 0 TO nNodes-1 DO
        nodei := nodeArr[i];
        succs := nodei.succ;
        WHILE succs # NIL DO
          edge := succs.head;
          IF self.undoable AND edge.value # edge.nextValue THEN
            PushEdgeVal(self, edge, edge.value)
          END (* IF *);
          edge.value := edge.nextValue;
          succs := succs.tail
        END;
      END
    END;
    RETURN TRUE
  END TransitiveClose;

PROCEDURE AddEdgeAndClose(self: T;
                          n1: NodeVal.T; ev: EdgeVal.T; n2: NodeVal.T;
                          addNodes := FALSE;
                          edgeChange: EdgeMapProc := NIL): BOOLEAN =
  VAR oldVal, newVal: EdgeVal.T; BEGIN
    <*ASSERT self.csr # NIL *>
    IF addNodes THEN
      IF NOT NodeExists(self, n1) THEN 
        AddNode(self, n1) <*NOWARN*>
      END (* IF *);
      IF NOT NodeExists(self, n2) THEN 
        AddNode(self, n2) <*NOWARN*>
      END (* IF *)
    END (* IF *);
    IF NOT self.getEdge(n1, n2, oldVal) THEN
      oldVal := self.csr.plusIdent;
    END (* IF *);
    newVal := self.csr.plus(oldVal, ev);
    IF oldVal = newVal THEN
      RETURN TRUE
    ELSIF newVal = self.csr.bottom THEN
      RETURN FALSE
    ELSE
      IF edgeChange # NIL THEN edgeChange(n1, newVal, n2) END (* IF *);
      self.setEdge(n1, newVal, n2); <*NOWARN*>
      RETURN CloseOnPreds(self, newVal, n1, n2, edgeChange) AND
             CloseOnSuccs(self, newVal, n1, n2, edgeChange)
    END (* IF *);
  END AddEdgeAndClose;


PROCEDURE CloseOnPreds(self: T; newVal: EdgeVal.T;
                       n1, n2: NodeVal.T;
                       edgeChange: EdgeMapProc): BOOLEAN =
  VAR ni: NodeIter := self.getPredIter(n1); <*NOWARN*>
      pred: NodeVal.T;
      oldEdge, predEdge, newEdge: EdgeVal.T;
  BEGIN
    WHILE ni.next(pred) DO
      predEdge := self.edgeValue(pred, n1); <*NOWARN*>
      IF NOT self.getEdge(pred, n2, oldEdge) THEN
        oldEdge := self.csr.plusIdent;
      END (* IF *);
      newEdge := self.csr.plus(oldEdge, self.csr.times(predEdge, newVal));
      IF newEdge # self.csr.plusIdent THEN
        IF pred = n2 THEN
          (* We have a cycle! Set the edges between n1 and n2 to the closure
             of the edge we we about to add. *)
          VAR closeVal := self.csr.closure(newEdge); BEGIN
            IF closeVal = self.csr.bottom THEN
              RETURN FALSE
            ELSE
              IF NOT self.addEdgeAndClose(n1, closeVal, n2,
                                          FALSE, edgeChange) THEN
                RETURN FALSE
              END (* IF *);
              IF NOT self.addEdgeAndClose(n2, closeVal, n1,
                                          FALSE, edgeChange) THEN
                RETURN FALSE
              END (* IF *);
            END (* IF *)
          END (* WITH *);
        ELSE
          IF NOT self.addEdgeAndClose(pred, newEdge, n2,
                                      FALSE, edgeChange) THEN
            RETURN FALSE
          END (* IF *)
        END (* IF *)
      END (* IF *)
    END (* WHILE *);
    RETURN TRUE
  END CloseOnPreds;
      
PROCEDURE CloseOnSuccs(self: T; newVal: EdgeVal.T;
                       n1, n2: NodeVal.T;
                       edgeChange: EdgeMapProc): BOOLEAN =
  VAR ni: NodeIter := self.getSuccIter(n2); <*NOWARN*>
      succ: NodeVal.T;
      oldEdge, succEdge, newEdge: EdgeVal.T;
  BEGIN
    WHILE ni.next(succ) DO
      succEdge := self.edgeValue(n2, succ); <*NOWARN*>
      IF NOT self.getEdge(n1, succ, oldEdge) THEN
        oldEdge := self.csr.plusIdent;
      END (* IF *);
      newEdge := self.csr.plus(oldEdge, self.csr.times(newVal, succEdge));
      IF newEdge # self.csr.plusIdent THEN
        IF n1 = succ THEN
          (* We have a cycle! Set the edges between n1 and n2 to the closure
             of the edge we we about to add. *)
          VAR closeVal := self.csr.closure(newEdge); BEGIN
            IF closeVal = self.csr.bottom THEN
              RETURN FALSE
            ELSE
              IF NOT self.addEdgeAndClose(n1, closeVal, n2,
                                          FALSE, edgeChange) THEN
                RETURN FALSE
              END (* IF *);
              IF NOT self.addEdgeAndClose(n2, closeVal, n1,
                                          FALSE, edgeChange) THEN
                RETURN FALSE
              END (* IF *);
            END (* IF *)
          END (* WITH *);
        ELSE
          IF NOT self.addEdgeAndClose(n1, newEdge, succ,
                                      FALSE, edgeChange) THEN
            RETURN FALSE
          END (* IF *)
        END (* IF *);
      END (* IF *);
    END (* WHILE *);
    RETURN TRUE
  END CloseOnSuccs;

PROCEDURE TopSort(self: T;
                  VAR (*OUT*) res: REF ARRAY OF NodeVal.T): BOOLEAN =
  VAR nodes := NEW(REF ARRAY OF Node, self.nodeSize());
      cycle := NEW(RefSeq.T).init();
      cur := LAST(nodes^);
  (* Returns TRUE and sets "res" only if it finds a cycle;
     otherwise, filles in "nodes" right to left. *)
  PROCEDURE TopSortWork(n: Node): BOOLEAN =
    BEGIN
      IF Word.And(n.misc, 2) # 0 THEN
        WHILE cycle.getlo() # n DO EVAL cycle.remlo() END (* WHILE *);
        res := NEW(REF ARRAY OF NodeVal.T, cycle.size());
        FOR k := 0 TO LAST(res^) DO
          res[k] := NARROW(cycle.get(k), Node).value
        END (* FOR *);
        RETURN TRUE
      ELSIF Word.And(n.misc, 1) # 0 THEN
        RETURN FALSE
      ELSE
        cycle.addhi(n); n.misc := 2;
        VAR succ := n.succ; BEGIN
          WHILE succ # NIL DO
            VAR e: Edge := succ.head; BEGIN
              IF TopSortWork(e.to) THEN RETURN TRUE END (* IF *)
            END (* BEGIN *);
            succ := succ.tail
          END (* WHILE *)
        END (* BEGIN *);
        EVAL cycle.remhi();
        nodes[cur] := n; DEC(cur);
        n.misc := 1;
        RETURN FALSE
      END (* IF *)
    END TopSortWork;
  BEGIN
    SetMiscs(self, 0);
    (* First, find the roots. *)
    VAR iter := self.nodeTbl.iterate(); nodeValRA, nodeRA: REFANY; BEGIN
      WHILE iter.next(nodeValRA, nodeRA) DO
        VAR node: Node := nodeRA; BEGIN
          IF TopSortWork(node) THEN RETURN FALSE END (* IF *)
        END (* BEGIN *)
      END (* WHILE *)
    END (* BEGIN *);
    res := NEW(REF ARRAY OF NodeVal.T, self.nodeSize());
    FOR i := 0 TO LAST(res^) DO res[i] := nodes[i].value END (* FOR *);
    RETURN TRUE
  END TopSort;
      

(*******************************************************************)

PROCEDURE PrintAsMatrix(self: T; wr: Wr.T;
                        np: NodePrintProc;
                        ep: EdgePrintProc;
                        between, colWidth: CARDINAL;
                        absentEV: EdgeVal.T) =
  VAR
    nodei, nodej: Node;
    edge: Edge;
    nodeArr: NodeArr;
    nNodes: CARDINAL;
  BEGIN
    (* Repack the array so we can index the nodes. *)
    nodeArr := self.makeNodeArray();
    nNodes := self.nodeSize();
    (* Print the top line *)
    FOR i := 1 TO colWidth+1 DO Wr.PutChar(wr, ' '); END;
    FOR i := 0 TO nNodes-1 DO
      FOR j := 1 TO between DO Wr.PutChar(wr, ' '); END;
      nodei := nodeArr[i];
      np(wr, nodei.value, colWidth);
    END;
    Wr.PutChar(wr, '\n');
    FOR i := 1 TO colWidth+between DO Wr.PutChar(wr, ' '); END;
    Wr.PutChar(wr, '+');
    FOR i := 1 TO nNodes*colWidth + (nNodes-1)*between DO
      Wr.PutChar(wr, '-');
    END;
    Wr.PutChar(wr, '\n');

    FOR i := 0 TO nNodes-1 DO
      nodei := nodeArr[i];
      np(wr, nodei.value, colWidth);
      FOR j := 1 TO between DO Wr.PutChar(wr, ' '); END;
      Wr.PutChar(wr, '|');
      FOR j := 0 TO nNodes-1 DO
        nodej := nodeArr[j];
        IF FindEdge(nodei, nodej, edge) THEN
          ep(wr, TRUE, edge.value, colWidth);
        ELSE
          ep(wr, FALSE, absentEV, colWidth);
        END;
        FOR k := 1 TO between DO Wr.PutChar(wr, ' '); END;
      END;
      Wr.PutChar(wr, '\n');
    END;
  END PrintAsMatrix;

PROCEDURE PushUndo(self: T; type: UndoType; n: Node; e: Edge := NIL) =
  BEGIN
    ExpandIfNeed(self);
    WITH top = self.undoStack[self.undoSP] DO
      top.type := type; top.n := n; top.e := e
    END (* WITH *);
    INC(self.undoSP)
  END PushUndo;

PROCEDURE PushEdgeVal(self: T; e: Edge; ev: EdgeVal.T) =
  BEGIN
    ExpandIfNeed(self);
    WITH top = self.undoStack[self.undoSP] DO
      top.type := UndoType.EdgeVal; top.e := e; top.ev := ev
    END (* WITH *);
    INC(self.undoSP)
  END PushEdgeVal;

PROCEDURE ExpandIfNeed(self: T) =
  BEGIN
    IF self.undoSP = NUMBER(self.undoStack^) THEN
      VAR new := NEW(REF ARRAY OF UndoRec, 2*self.undoSP); BEGIN
        SUBARRAY(new^, 0, self.undoSP) := self.undoStack^;
        self.undoStack := new
      END (* BEGIN *);
    END (* IF *)
  END ExpandIfNeed;

PROCEDURE Push(self: T) =
  BEGIN
    <*ASSERT self.undoable *>
    PushUndo(self, UndoType.Mark, NIL);
  END Push;

PROCEDURE Pop(self: T) =
  <*FATAL DupEdge, DupNode, NoSuchNode, NoSuchEdge *>
  BEGIN
    self.undoable := FALSE;
    LOOP
      IF self.undoSP < NUMBER(self.undoStack^) THEN
        self.undoStack[self.undoSP].n := NIL;
        self.undoStack[self.undoSP].e := NIL
      END (* IF *);
      DEC(self.undoSP);
      WITH top = self.undoStack[self.undoSP] DO
        CASE top.type OF
        | UndoType.Mark =>
            EXIT
        | UndoType.AddNode =>
            self.deleteNode(top.n.value)
        | UndoType.DeleteNode =>
            self.addNode(top.n.value)
        | UndoType.AddEdge =>
            self.deleteEdge(top.e.from.value, top.e.to.value)
        | UndoType.DeleteEdge =>
            self.addEdge(top.e.from.value, top.e.value, top.e.to.value)
        | UndoType.EdgeVal =>
            top.e.value := top.ev
        END (* CASE *)
      END (* WITH *)
    END (* LOOP *);
    self.undoable := TRUE
  END Pop;

BEGIN
END DiGraph.


