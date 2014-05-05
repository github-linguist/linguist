module examples/systems/views

/*
 * Model of views in object-oriented programming.
 *
 * Two object references, called the view and the backing,
 * are related by a view mechanism when changes to the
 * backing are automatically propagated to the view. Note
 * that the state of a view need not be a projection of the
 * state of the backing; the keySet method of Map, for
 * example, produces two view relationships, and for the
 * one in which the map is modified by changes to the key
 * set, the value of the new map cannot be determined from
 * the key set. Note that in the iterator view mechanism,
 * the iterator is by this definition the backing object,
 * since changes are propagated from iterator to collection
 * and not vice versa. Oddly, a reference may be a view of
 * more than one backing: there can be two iterators on the
 * same collection, eg. A reference cannot be a view under
 * more than one view type.
 *
 * A reference is made dirty when it is a backing for a view
 * with which it is no longer related by the view invariant.
 * This usually happens when a view is modified, either
 * directly or via another backing. For example, changing a
 * collection directly when it has an iterator invalidates
 * it, as does changing the collection through one iterator
 * when there are others.
 *
 * More work is needed if we want to model more closely the
 * failure of an iterator when its collection is invalidated.
 *
 * As a terminological convention, when there are two
 * complementary view relationships, we will give them types
 * t and t'. For example, KeySetView propagates from map to
 * set, and KeySetView' propagates from set to map.
 *
 * author: Daniel Jackson
 */

open util/ordering[State] as so
open util/relation as rel

sig Ref {}
sig Object {}

-- t->b->v in views when v is view of type t of backing b
-- dirty contains refs that have been invalidated
sig State {
  refs: set Ref,
  obj: refs -> one Object,
  views: ViewType -> refs -> refs,
  dirty: set refs
--  , anyviews: Ref -> Ref -- for visualization
  }
-- {anyviews = ViewType.views}

sig Map extends Object {
  keys: set Ref,
  map: keys -> one Ref
  }{all s: State |  keys + Ref.map in s.refs}
sig MapRef extends Ref {}
fact {State.obj[MapRef] in Map}

sig Iterator extends Object {
  left, done: set Ref,
  lastRef: lone done
  }{all s: State | done + left + lastRef in s.refs}
sig IteratorRef extends Ref {}
fact {State.obj[IteratorRef] in Iterator}

sig Set extends Object {
  elts: set Ref
  }{all s: State | elts in s.refs}
sig SetRef extends Ref {}
fact {State.obj[SetRef] in Set}

abstract sig ViewType {}
one sig KeySetView, KeySetView', IteratorView extends ViewType {}
fact ViewTypes {
  State.views[KeySetView] in MapRef -> SetRef
  State.views[KeySetView'] in SetRef -> MapRef
  State.views[IteratorView] in IteratorRef -> SetRef
  all s: State | s.views[KeySetView] = ~(s.views[KeySetView'])
  }

/**
 * mods is refs modified directly or by view mechanism
 * doesn't handle possibility of modifying an object and its view at once?
 * should we limit frame conds to non-dirty refs?
 */
pred modifies [pre, post: State, rs: set Ref] {
  let vr = pre.views[ViewType], mods = rs.*vr {
    all r: pre.refs - mods | pre.obj[r] = post.obj[r]
    all b: mods, v: pre.refs, t: ViewType |
      b->v in pre.views[t] => viewFrame [t, pre.obj[v], post.obj[v], post.obj[b]]
    post.dirty = pre.dirty +
      {b: pre.refs | some v: Ref, t: ViewType |
          b->v in pre.views[t] && !viewFrame [t, pre.obj[v], post.obj[v], post.obj[b]]
      }
    }
  }

pred allocates [pre, post: State, rs: set Ref] {
  no rs & pre.refs
  post.refs = pre.refs + rs
  }

/** 
 * models frame condition that limits change to view object from v to v' when backing object changes to b'
 */
pred viewFrame [t: ViewType, v, v', b': Object] {
  t in KeySetView => v'.elts = dom [b'.map]
  t in KeySetView' => b'.elts = dom [v'.map]
  t in KeySetView' => (b'.elts) <: (v.map) = (b'.elts) <: (v'.map)
  t in IteratorView => v'.elts = b'.left + b'.done
  }

pred MapRef.keySet [pre, post: State, setRefs: SetRef] {
  post.obj[setRefs].elts = dom [pre.obj[this].map]
  modifies [pre, post, none]
  allocates [pre, post, setRefs]
  post.views = pre.views + KeySetView->this->setRefs + KeySetView'->setRefs->this
  }

pred MapRef.put [pre, post: State, k, v: Ref] {
  post.obj[this].map = pre.obj[this].map ++ k->v
  modifies [pre, post, this]
  allocates [pre, post, none]
  post.views = pre.views
  }

pred SetRef.iterator [pre, post: State, iterRef: IteratorRef] {
  let i = post.obj[iterRef] {
    i.left = pre.obj[this].elts
    no i.done + i.lastRef
    }
  modifies [pre,post,none]
  allocates [pre, post, iterRef]
  post.views = pre.views + IteratorView->iterRef->this
  }

pred IteratorRef.remove [pre, post: State] {
  let i = pre.obj[this], i' = post.obj[this] {
    i'.left = i.left
    i'.done = i.done - i.lastRef
    no i'.lastRef
    }
  modifies [pre,post,this]
  allocates [pre, post, none]
  pre.views = post.views
  }

pred IteratorRef.next [pre, post: State, ref: Ref] {
  let i = pre.obj[this], i' = post.obj[this] {
    ref in i.left
    i'.left = i.left - ref
    i'.done = i.done + ref
    i'.lastRef = ref
    }
  modifies [pre, post, this]
  allocates [pre, post, none]
  pre.views = post.views
  }

pred IteratorRef.hasNext [s: State] {
  some s.obj[this].left
  }

assert zippishOK {
  all
    ks, vs: SetRef,
    m: MapRef,
    ki, vi: IteratorRef,
    k, v: Ref |
    let s0=so/first,
    s1=so/next[s0],
    s2=so/next[s1],
    s3=so/next[s2],
    s4=so/next[s3],
    s5=so/next[s4],
    s6=so/next[s5],
    s7=so/next[s6] |
  ({
    precondition [s0, ks, vs, m]
    no s0.dirty
    ks.iterator [s0, s1, ki]
    vs.iterator [s1, s2, vi]
    ki.hasNext [s2]
    vi.hasNext [s2]
    ki.this/next [s2, s3, k]
    vi.this/next [s3, s4, v]
    m.put [s4, s5, k, v]
    ki.remove [s5, s6]
    vi.remove [s6, s7]
  } => no State.dirty)
  }

pred precondition [pre: State, ks, vs, m: Ref] {
  // all these conditions and other errors discovered in scope of 6 but 8,3
  // in initial state, must have view invariants hold
  (all t: ViewType, b, v: pre.refs |
    b->v in pre.views[t] => viewFrame [t, pre.obj[v], pre.obj[v], pre.obj[b]])
  // sets are not aliases
--  ks != vs
  // sets are not views of map
--  no (ks+vs)->m & ViewType.pre.views
  // no iterator currently on either set
--  no Ref->(ks+vs) & ViewType.pre.views
  }

check zippishOK for 6 but 8 State, 3 ViewType expect 1

/** 
 * experiment with controlling heap size
 */
fact {all s: State | #s.obj < 5}
