// RUN: %boogie "%s" > "%t"
// RUN: %diff "%s.expect" "%t"

function RightOpen (n: int) : [int]bool;
function RightClosed (n: int) : [int]bool;
axiom (forall x: int, y: int :: RightOpen(x)[y] <==> y < x);
axiom (forall x: int, y: int :: RightClosed(x)[y] <==> y <= x);

type {:linear "tid"} X;
const nil: X;
var {:layer 0,1} t: int;       // next ticket to issue
var {:layer 0,2} s: int;       // current ticket permitted to critical section
var {:layer 0,2} cs: X;        // current thread in critical section
var {:layer 0,2} T: [int]bool; // set of issued tickets

// ###########################################################################
// Invariants

function {:inline} Inv1 (tickets: [int]bool, ticket: int): (bool)
{
  tickets == RightOpen(ticket)
}

function {:inline} Inv2 (tickets: [int]bool, ticket: int, lock: X): (bool)
{
  if (lock == nil) then tickets == RightOpen(ticket)
                   else tickets == RightClosed(ticket)
}

// ###########################################################################
// Yield invariants

procedure {:yield_invariant} {:layer 2} YieldSpec ({:linear "tid"} tid: X);
requires tid != nil && cs == tid;

procedure {:yield_invariant} {:layer 1} Yield1 ();
requires Inv1(T, t);

procedure {:yield_invariant} {:layer 2} Yield2 ();
requires Inv2(T, s, cs);

// ###########################################################################
// Main program

procedure {:yields} {:layer 2} main ({:linear_in "tid"} xls':[X]bool)
requires {:layer 2} xls' == MapConst(true);
{
  var {:linear "tid"} tid: X;
  var {:linear "tid"} xls: [X]bool;

  call InitAbstract(xls');
  xls := xls';

  while (*)
  invariant {:yields} {:layer 1,2} {:yield_loop "Yield1"} {:yield_loop "Yield2"} true;
  {
    par xls, tid := Allocate(xls) | Yield1() | Yield2();
    async call Customer(tid);
  }
}

procedure {:yields} {:layer 2} Allocate ({:linear_in "tid"} xls':[X]bool) returns ({:linear "tid"} xls: [X]bool, {:linear "tid"} xl: X)
ensures {:layer 1,2} xl != nil;
{
  call xls, xl := AllocateLow(xls');
}

procedure {:yields} {:layer 2}
{:yield_requires "Yield1"}
{:yield_requires "Yield2"}
Customer ({:linear_in "tid"} tid: X)
requires {:layer 2} tid != nil;
{
  while (*)
  invariant {:yields} {:layer 1,2} {:yield_loop "Yield1"} {:yield_loop "Yield2"} true;
  {
    call Enter(tid);
    par Yield1() | Yield2() | YieldSpec(tid);
    call Leave(tid);
  }
}

procedure {:yields} {:layer 2}
{:yield_preserves "Yield1"}
{:yield_preserves "Yield2"}
{:yield_ensures   "YieldSpec", tid}
Enter ({:linear "tid"} tid: X)
requires {:layer 2} tid != nil;
{
  var m: int;

  call m := GetTicketAbstract(tid);
  call WaitAndEnter(tid, m);
}

// ###########################################################################
// Abstractions of primitive atomic actions

// Note how GetTicketAbstract becomes a right mover

procedure {:atomic} {:layer 2} AtomicInitAbstract ({:linear "tid"} xls:[X]bool)
modifies cs, s, T;
{ assert xls == MapConst(true); cs := nil; s := 0; T := RightOpen(0); }

procedure {:yields} {:layer 1} {:refines "AtomicInitAbstract"} InitAbstract ({:linear "tid"} xls:[X]bool)
ensures  {:layer 1} Inv1(T, t);
{
  call Init(xls);
}

procedure {:right} {:layer 2} AtomicGetTicketAbstract ({:linear "tid"} tid: X) returns (m: int)
modifies T;
{ assume !T[m]; T[m] := true; }

procedure {:yields} {:layer 1} {:refines "AtomicGetTicketAbstract"} GetTicketAbstract ({:linear "tid"} tid: X) returns (m: int)
requires {:layer 1} Inv1(T, t);
ensures  {:layer 1} Inv1(T, t);
{
  par Yield1();
  call m := GetTicket(tid);
  par Yield1();
}

// ###########################################################################
// Primitive atomic actions

procedure {:atomic} {:layer 1} AtomicInit ({:linear "tid"} xls:[X]bool)
modifies cs, t, s, T;
{ assert xls == MapConst(true); cs := nil; t := 0; s := 0; T := RightOpen(0); }

procedure {:yields} {:layer 0} {:refines "AtomicInit"} Init ({:linear "tid"} xls:[X]bool);

procedure {:atomic} {:layer 1} AtomicGetTicket ({:linear "tid"} tid: X) returns (m: int)
modifies t, T;
{ m := t; t := t + 1; T[m] := true; }

procedure {:yields} {:layer 0} {:refines "AtomicGetTicket"} GetTicket ({:linear "tid"} tid: X) returns (m: int);

procedure {:atomic} {:layer 1,2} AtomicWaitAndEnter ({:linear "tid"} tid: X, m:int)
modifies cs;
{ assume m == s; cs := tid; }

procedure {:yields} {:layer 0} {:refines "AtomicWaitAndEnter"} WaitAndEnter ({:linear "tid"} tid: X, m:int);

procedure {:atomic} {:layer 1,2} AtomicLeave ({:linear "tid"} tid: X)
modifies cs, s;
{ assert cs == tid; s := s + 1; cs := nil; }

procedure {:yields} {:layer 0} {:refines "AtomicLeave"} Leave ({:linear "tid"} tid: X);

procedure {:atomic} {:layer 1,2} AtomicAllocateLow ({:linear_in "tid"} xls':[X]bool) returns ({:linear "tid"} xls: [X]bool, {:linear "tid"} xl: X)
{ assume xl != nil && xls'[xl]; xls := xls'[xl := false]; }

procedure {:yields} {:layer 0} {:refines "AtomicAllocateLow"} AllocateLow ({:linear_in "tid"} xls':[X]bool) returns ({:linear "tid"} xls: [X]bool, {:linear "tid"} xl: X);
