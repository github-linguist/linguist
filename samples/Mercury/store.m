%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 2000-2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: store.m.
% Main author: fjh.
% Stability: low.
% 
% This file provides facilities for manipulating mutable stores.
% A store can be considered a mapping from abstract keys to their values.
% A store holds a set of nodes, each of which may contain a value of any
% type.
%
% Stores may be used to implement cyclic data structures such as circular
% linked lists, etc.
%
% Stores can have two different sorts of keys:
% mutable variables (mutvars) and references (refs).
% The difference between mutvars and refs is that mutvars can only be updated
% atomically, whereas it is possible to update individual fields of a
% reference one at a time (presuming the reference refers to a structured
% term).
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module store.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Stores and keys are indexed by a type S of typeclass store(S) that
    % is used to distinguish between different stores.  By using an
    % existential type declaration for store.new (see below), we use the
    % type system to ensure at compile time that you never attempt to use
    % a key from one store to access a different store.
    %
:- typeclass store(T) where [].
:- type store(S).

:- instance store(io.state).
:- instance store(store(S)).

    % Initialize a new store.
    %
:- some [S] pred store.init(store(S)::uo) is det.

%-----------------------------------------------------------------------------%
%
% Mutvars
%

    % generic_mutvar(T, S):
    % A mutable variable holding a value of type T in store S.
    %
:- type generic_mutvar(T, S).
:- type io_mutvar(T) == generic_mutvar(T, io.state).
:- type store_mutvar(T, S) == generic_mutvar(T, store(S)).

    % Create a new mutable variable, initialized with the specified value.
    %
:- pred store.new_mutvar(T::in, generic_mutvar(T, S)::out, S::di, S::uo)
    is det <= store(S).

    % copy_mutvar(OldMutvar, NewMutvar, S0, S) is equivalent to the sequence
    %   get_mutvar(OldMutvar, Value, S0, S1),
    %   new_mutvar(NewMutvar, Value, S1, S )
    %
:- pred store.copy_mutvar(generic_mutvar(T, S)::in, generic_mutvar(T, S)::out,
    S::di, S::uo) is det <= store(S).

    % Lookup the value stored in a given mutable variable.
    %
:- pred store.get_mutvar(generic_mutvar(T, S)::in, T::out,
    S::di, S::uo) is det <= store(S).

    % Replace the value stored in a given mutable variable.
    %
:- pred store.set_mutvar(generic_mutvar(T, S)::in, T::in,
    S::di, S::uo) is det <= store(S).

    % new_cyclic_mutvar(Func, Mutvar):
    %
    % Create a new mutable variable, whose value is initialized
    % with the value returned from the specified function `Func'.
    % The argument passed to the function is the mutvar itself,
    % whose value has not yet been initialized (this is safe
    % because the function does not get passed the store, so
    % it can't examine the uninitialized value).
    %
    % This predicate is useful for creating self-referential values
    % such as circular linked lists.
    % For example:
    %
    %   :- type clist(T, S) ---> node(T, mutvar(clist(T, S))).
    %
    %   :- pred init_cl(T::in, clist(T, S)::out,
    %       store(S)::di, store(S)::uo) is det.
    %
    %   init_cl(X, CList, !Store) :-
    %       store.new_cyclic_mutvar(func(CL) = node(X, CL), CList,
    %       !Store).
    %
:- pred store.new_cyclic_mutvar((func(generic_mutvar(T, S)) = T)::in,
    generic_mutvar(T, S)::out, S::di, S::uo) is det <= store(S).

%-----------------------------------------------------------------------------%
%
% References
%

    % generic_ref(T, S):
    %
    % A reference to value of type T in store S.
    %
:- type generic_ref(T, S).
:- type io_ref(T, S) == generic_ref(T, io.state).
:- type store_ref(T, S) == generic_ref(T, store(S)).

    % new_ref(Val, Ref):
    %   /* In C: Ref = malloc(...); *Ref = Val; */
    %
    % Given a value of any type `T', insert a copy of the term
    % into the store and return a new reference to that term.
    % (This does not actually perform a copy, it just returns a view
    % of the representation of that value.
    % It does however allocate one cell to hold the reference;
    % you can use new_arg_ref to avoid that.)
    %
:- pred store.new_ref(T::di, generic_ref(T, S)::out,
    S::di, S::uo) is det <= store(S).

    % ref_functor(Ref, Functor, Arity):
    %
    % Given a reference to a term, return the functor and arity
    % of that term.
    %
:- pred store.ref_functor(generic_ref(T, S)::in, string::out, int::out,
    S::di, S::uo) is det <= store(S).

    % arg_ref(Ref, ArgNum, ArgRef):
    %   /* Pseudo-C code: ArgRef = &Ref[ArgNum]; */
    %
    % Given a reference to a term, return a reference to
    % the specified argument (field) of that term
    % (argument numbers start from zero).
    % It is an error if the argument number is out of range,
    % or if the argument reference has the wrong type.
    %
:- pred store.arg_ref(generic_ref(T, S)::in, int::in,
    generic_ref(ArgT, S)::out, S::di, S::uo) is det <= store(S).

    % new_arg_ref(Val, ArgNum, ArgRef):
    %   /* Pseudo-C code: ArgRef = &Val[ArgNum]; */
    %
    % Equivalent to `new_ref(Val, Ref), arg_ref(Ref, ArgNum, ArgRef)',
    % except that it is more efficient.
    % It is an error if the argument number is out of range,
    % or if the argument reference has the wrong type.
    %
:- pred store.new_arg_ref(T::di, int::in, generic_ref(ArgT, S)::out,
    S::di, S::uo) is det <= store(S).

    % set_ref(Ref, ValueRef):
    %   /* Pseudo-C code: *Ref = *ValueRef; */
    %
    % Given a reference to a term (Ref),
    % a reference to another term (ValueRef),
    % update the store so that the term referred to by Ref
    % is replaced with the term referenced by ValueRef.
    %
:- pred store.set_ref(generic_ref(T, S)::in, generic_ref(T, S)::in,
    S::di, S::uo) is det <= store(S).

    % set_ref_value(Ref, Value):
    %   /* Pseudo-C code: *Ref = Value; */
    %
    % Given a reference to a term (Ref), and a value (Value),
    % update the store so that the term referred to by Ref
    % is replaced with Value.
    %
:- pred store.set_ref_value(generic_ref(T, S)::in, T::di,
    S::di, S::uo) is det <= store(S).

    % Given a reference to a term, return that term.
    % Note that this requires making a copy, so this pred may
    % be inefficient if used to return large terms; it
    % is most efficient with atomic terms.
    % XXX current implementation buggy (does shallow copy)
    %
:- pred store.copy_ref_value(generic_ref(T, S)::in, T::uo,
    S::di, S::uo) is det <= store(S).

    % Same as above, but without making a copy. Destroys the store.
    %
:- pred store.extract_ref_value(S::di, generic_ref(T, S)::in, T::out)
    is det <= store(S).

%-----------------------------------------------------------------------------%
%
% Nasty performance hacks
%
% WARNING: use of these procedures is dangerous!
% Use them only as a last resort, only if performance is critical, and only if
% profiling shows that using the safe versions is a bottleneck.
%
% These procedures may vanish in some future version of Mercury.

    % `unsafe_arg_ref' is the same as `arg_ref',
    % and `unsafe_new_arg_ref' is the same as `new_arg_ref'
    % except that they doesn't check for errors,
    % and they don't work for `no_tag' types (types with
    % exactly one functor which has exactly one argument),
    % and they don't work for arguments which occupy a word with other
    % arguments,
    % and they don't work for types with >4 functors.
    % If the argument number is out of range,
    % or if the argument reference has the wrong type,
    % or if the argument is a `no_tag' type,
    % or if the argument uses a packed representation,
    % then the behaviour is undefined, and probably harmful.

:- pred store.unsafe_arg_ref(generic_ref(T, S)::in, int::in,
    generic_ref(ArgT, S)::out, S::di, S::uo) is det <= store(S).

:- pred store.unsafe_new_arg_ref(T::di, int::in, generic_ref(ArgT, S)::out,
    S::di, S::uo) is det <= store(S).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module require.

:- instance store(store(S)) where [].
:- instance store(io.state) where [].

% The store type itself is just a dummy type,
% with no real representation.

:- type store(S).
:- pragma foreign_type("C", store(S), "MR_Word", [can_pass_as_mercury_type])
    where equality is store_equal, comparison is store_compare.
:- pragma foreign_type("IL", store(S), "int32", [can_pass_as_mercury_type])
    where equality is store_equal, comparison is store_compare.
:- pragma foreign_type("C#", store(S), "int", [can_pass_as_mercury_type])
    where equality is store_equal, comparison is store_compare.
:- pragma foreign_type("Java", store(S), "int", [can_pass_as_mercury_type])
    where equality is store_equal, comparison is store_compare.
:- pragma foreign_type("Erlang", store(S), "", [can_pass_as_mercury_type])
    where equality is store_equal, comparison is store_compare.

:- pred store_equal(store(S)::in, store(S)::in) is semidet.

store_equal(_, _) :-
    error("attempt to unify two stores").

:- pred store_compare(comparison_result::uo, store(S)::in, store(S)::in)
    is det.

store_compare(_, _, _) :-
    error("attempt to compare two stores").

    % Mutvars and references are each represented as a pointer to a single word
    % on the heap.
:- type generic_mutvar(T, S) ---> mutvar(private_builtin.ref(T)).
:- type generic_ref(T, S) ---> ref(private_builtin.ref(T)).

store.init(S) :-
    store.do_init(S).

:- some [S] pred store.do_init(store(S)::uo) is det.

:- pragma foreign_proc("C",
    store.do_init(_S0::uo),
    [will_not_call_mercury, promise_pure, will_not_modify_trail],
"
    TypeInfo_for_S = 0;
").
:- pragma foreign_proc("C#",
    store.do_init(_S0::uo),
    [will_not_call_mercury, promise_pure],
"
    TypeInfo_for_S = null;
").
:- pragma foreign_proc("Java",
    store.do_init(_S0::uo),
    [will_not_call_mercury, promise_pure],
"
    TypeInfo_for_S = null;
").
:- pragma foreign_proc("Erlang",
    store.do_init(_S0::uo),
    [will_not_call_mercury, promise_pure],
"
    TypeInfo_for_S = 'XXX'
").

% Note -- the syntax for the operations on stores
% might be nicer if we used some new operators, e.g.
%
%   :- op(.., xfx, ('<-')).
%   :- op(.., fy, ('!')).
%   :- op(.., xfx, (':=')).
%
% Then we could do something like this:
%
%   Ptr <- new(Val)   -->   new_mutvar(Val, Ptr).
%   Val <- !Ptr       -->   get_mutvar(Ptr, Val).
%   !Ptr := Val   -->   set_mutvar(Ptr, Val).
%
% I wonder whether it is worth it?  Hmm, probably not.

:- pragma foreign_proc("C",
    new_mutvar(Val::in, Mutvar::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure, will_not_modify_trail],
"
    MR_offset_incr_hp_msg(Mutvar, MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1,
        MR_ALLOC_ID, ""store.mutvar/2"");
    MR_define_size_slot(0, Mutvar, 1);
    * (MR_Word *) Mutvar = Val;
    S = S0;
").

:- pragma foreign_proc("C",
    get_mutvar(Mutvar::in, Val::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure, will_not_modify_trail],
"
    Val = * (MR_Word *) Mutvar;
    S = S0;
").

:- pragma foreign_proc("C",
    set_mutvar(Mutvar::in, Val::in, S0::di, S::uo),
    [will_not_call_mercury, promise_pure, will_not_modify_trail],
"
    * (MR_Word *) Mutvar = Val;
    S = S0;
").

:- pragma foreign_type("C#", generic_mutvar(T, S), "object[]").

:- pragma foreign_proc("C#",
    new_mutvar(Val::in, Mutvar::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Mutvar = new object[] { Val };
").

:- pragma foreign_proc("C#",
    get_mutvar(Mutvar::in, Val::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Val = Mutvar[0];
").

:- pragma foreign_proc("C#",
    set_mutvar(Mutvar::in, Val::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Mutvar[0] = Val;
").

:- pragma foreign_type("Java", generic_mutvar(T, S), "mutvar.Mutvar").

:- pragma foreign_proc("Java",
    new_mutvar(Val::in, Mutvar::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Mutvar = new mutvar.Mutvar(Val);
").

:- pragma foreign_proc("Java",
    get_mutvar(Mutvar::in, Val::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Val = Mutvar.object;
").

:- pragma foreign_proc("Java",
    set_mutvar(Mutvar::in, Val::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Mutvar.object = Val;
").

% XXX ets are not garbage collected
% but shareable between processes

:- pragma foreign_type("Erlang", generic_mutvar(T, S), "").

:- pragma foreign_proc("Erlang",
    new_mutvar(Val::in, Mutvar::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    Mutvar = ets:new(mutvar, [set, public]),
    ets:insert(Mutvar, {value, Val}),
    S = S0
").

:- pragma foreign_proc("Erlang",
    get_mutvar(Mutvar::in, Val::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    [{value, Val}] = ets:lookup(Mutvar, value),
    S = S0
").

:- pragma foreign_proc("Erlang",
    set_mutvar(Mutvar::in, Val::in, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    ets:insert(Mutvar, {value, Val}),
    S = S0
").

copy_mutvar(Mutvar, Copy, !S) :-
    get_mutvar(Mutvar, Value, !S),
    new_mutvar(Value, Copy, !S).

:- pred store.unsafe_new_uninitialized_mutvar(generic_mutvar(T, S)::out,
    S::di, S::uo) is det <= store(S).

:- pragma foreign_proc("C",
    unsafe_new_uninitialized_mutvar(Mutvar::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure, will_not_modify_trail],
"
    MR_offset_incr_hp_msg(Mutvar, MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1,
        MR_ALLOC_ID, ""store.mutvar/2"");
    MR_define_size_slot(0, Mutvar, 1);
    S = S0;
").

:- pragma foreign_proc("C#",
    unsafe_new_uninitialized_mutvar(Mutvar::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Mutvar = new object[1];
").

:- pragma foreign_proc("Java",
    unsafe_new_uninitialized_mutvar(Mutvar::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Mutvar = new mutvar.Mutvar();
").

store.new_cyclic_mutvar(Func, MutVar, !Store) :-
    store.unsafe_new_uninitialized_mutvar(MutVar, !Store),
    Value = apply(Func, MutVar),
    store.set_mutvar(MutVar, Value, !Store).

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C#", generic_ref(T, S), "store.Ref").
:- pragma foreign_code("C#",
"
    public class Ref {
        // Object referenced.
        public object   obj;

        // Specific field of object referenced, or null to
        // specify the object itself.
        // XXX GetFields does not return fields in any particular order so
        // this is not really usable.
        public System.Reflection.FieldInfo  field;

        // Constructors
        public Ref(object init) {
            obj     = init;
            field   = null;
        }
        public Ref(object init, int num) {
            obj = init;
            setField(num);
        }

        // Set the field according to a given index.
        public void setField(int num) {
            field = obj.GetType().GetFields()[num];
        }

        // Return the value of the reference.
        public object getValue() {
            if (field == null) {
                return obj;
            } else {
                return field.GetValue(obj);
            }
        }

        // Update the value of the reference.
        public void setValue(object value) {
            field.SetValue(obj, value);
        }
    } // class Ref
").

:- pragma foreign_type(java, generic_ref(T, S), "store.Ref").
:- pragma foreign_code("Java",
"
    public static class Ref {
        // Object referenced.
        public java.lang.Object         object;

        // Specific field of object referenced, or null to
        // specify the object itself.
        // XXX getDeclaredFields does not return fields in any particular
        // order so this is not really usable.
        public java.lang.reflect.Field  field;

        // Constructors
        public Ref(java.lang.Object init) {
            object  = init;
            field   = null;
        }
        public Ref(java.lang.Object init, int num) {
            object  = init;
            setField(num);
        }

        // Set the field according to a given index.
        public void setField(int num) {
            try {
                field = object.getClass().getDeclaredFields()[num];
            } catch (java.lang.SecurityException se) {
                throw new java.lang.RuntimeException(
                    ""Security manager denied access to object fields"");
            } catch (java.lang.ArrayIndexOutOfBoundsException e) {
                throw new java.lang.RuntimeException(
                    ""No such field in object"");
            } catch (java.lang.Exception e) {
                throw new java.lang.RuntimeException(
                    ""Unable to set field: "" + e.getMessage());
            }
        }

        // Return the value of the reference.
        public java.lang.Object getValue() {
            if (field == null) {
                return object;
            } else {
                try {
                    return field.get(object);
                } catch (java.lang.IllegalAccessException e) {
                    throw new java.lang.RuntimeException(
                        ""Field inaccessible"");
                } catch (java.lang.IllegalArgumentException e)
                {
                    throw new java.lang.RuntimeException(
                        ""Field-object mismatch"");
                } catch (java.lang.NullPointerException e) {
                    throw new java.lang.RuntimeException(
                        ""Object is null"");
                } catch (java.lang.Exception e) {
                    throw new java.lang.RuntimeException(
                        ""Unable to access field: "" + e.getMessage());
                }
            }
        }

        // Update the value of the reference.
        public void setValue(java.lang.Object value) {
            try {
                field.set(object, value);
            } catch (java.lang.IllegalAccessException e) {
                throw new java.lang.RuntimeException(""Field inaccessible"");
            } catch (java.lang.IllegalArgumentException e) {
                throw new java.lang.RuntimeException(
                    ""Field-object mismatch"");
            } catch (java.lang.NullPointerException e) {
                throw new java.lang.RuntimeException(""Object is null"");
            } catch (java.lang.Exception e) {
                throw new java.lang.RuntimeException(
                        ""Unable to access field: "" + e.getMessage());
            }
        }
    } // class Ref
").

:- pragma foreign_proc("C",
    new_ref(Val::di, Ref::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure, will_not_modify_trail],
"
    MR_offset_incr_hp_msg(Ref, MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1,
        MR_ALLOC_ID, ""store.ref/2"");
    MR_define_size_slot(0, Ref, 1);
    * (MR_Word *) Ref = Val;
    S = S0;
").

:- pragma foreign_proc("C#",
    new_ref(Val::di, Ref::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Ref = new store.Ref(Val);
").

:- pragma foreign_proc("Java",
    new_ref(Val::di, Ref::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Ref = new store.Ref(Val);
").

:- pragma foreign_proc("Erlang",
    new_ref(Val::di, Ref::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    Ref = ets:new(mutvar, [set, public]),
    ets:insert(Ref, {value, Val}),
    S = S0
").

copy_ref_value(Ref, Val) -->
    % XXX Need to deep-copy non-atomic types.
    unsafe_ref_value(Ref, Val).

    % Unsafe_ref_value extracts the value that a reference refers to, without
    % making a copy; it is unsafe because the store could later be modified,
    % changing the returned value.
    %
:- pred store.unsafe_ref_value(generic_ref(T, S)::in, T::uo,
    S::di, S::uo) is det <= store(S).

:- pragma foreign_proc("C",
    unsafe_ref_value(Ref::in, Val::uo, S0::di, S::uo),
    [will_not_call_mercury, promise_pure, will_not_modify_trail],
"
    Val = * (MR_Word *) Ref;
    S = S0;
").

:- pragma foreign_proc("C#",
    unsafe_ref_value(Ref::in, Val::uo, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Val = Ref.getValue();
").

:- pragma foreign_proc("Java",
    unsafe_ref_value(Ref::in, Val::uo, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Val = Ref.getValue();
").

:- pragma foreign_proc("Erlang",
    unsafe_ref_value(Ref::in, Val::uo, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    [{value, Val}] = ets:lookup(Ref, value),
    S = S0
").

ref_functor(Ref, Functor, Arity, !Store) :-
    unsafe_ref_value(Ref, Val, !Store),
    functor(Val, canonicalize, Functor, Arity).

:- pragma foreign_decl("C",
"
    #include ""mercury_type_info.h""
    #include ""mercury_heap.h""
    #include ""mercury_misc.h""         /* for MR_fatal_error() */
    #include ""mercury_deconstruct.h""  /* for MR_arg() */
").

:- pragma foreign_proc("C",
    arg_ref(Ref::in, ArgNum::in, ArgRef::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"{
    MR_TypeInfo         type_info;
    MR_TypeInfo         arg_type_info;
    MR_TypeInfo         exp_arg_type_info;
    MR_Word             *arg_ref;
    const MR_DuArgLocn  *arg_locn;

    type_info = (MR_TypeInfo) TypeInfo_for_T;
    exp_arg_type_info = (MR_TypeInfo) TypeInfo_for_ArgT;

    MR_save_transient_registers();

    if (!MR_arg(type_info, (MR_Word *) Ref, ArgNum, &arg_type_info,
        &arg_ref, &arg_locn, MR_NONCANON_ABORT))
    {
        MR_fatal_error(""store.arg_ref: argument number out of range"");
    }

    if (MR_compare_type_info(arg_type_info, exp_arg_type_info) !=
        MR_COMPARE_EQUAL)
    {
        MR_fatal_error(""store.arg_ref: argument has wrong type"");
    }

    MR_restore_transient_registers();

    if (arg_locn != NULL && arg_locn->MR_arg_bits != 0) {
        MR_offset_incr_hp_msg(ArgRef, MR_SIZE_SLOT_SIZE,
            MR_SIZE_SLOT_SIZE + 1, MR_ALLOC_ID, ""store.ref/2"");
        MR_define_size_slot(0, ArgRef, 1);
        * (MR_Word *) ArgRef = MR_arg_value(arg_ref, arg_locn);
    } else {
        ArgRef = (MR_Word) arg_ref;
    }
    S = S0;
}").

:- pragma foreign_proc("C#",
    arg_ref(Ref::in, ArgNum::in, ArgRef::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    /*
    ** XXX Some dynamic type-checking should be done here to check that
    ** the type of the specified Arg matches the type supplied by the caller.
    ** This will require RTTI.
    */

    ArgRef = new store.Ref(Ref.getValue(), ArgNum);
").

:- pragma foreign_proc("Java",
    arg_ref(Ref::in, ArgNum::in, ArgRef::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    /*
    ** XXX Some dynamic type-checking should be done here to check that
    ** the type of the specified Arg matches the type supplied by the caller.
    ** This will require RTTI.
    */

    ArgRef = new store.Ref(Ref.getValue(), ArgNum);
").

:- pragma foreign_proc("C",
    new_arg_ref(Val::di, ArgNum::in, ArgRef::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"{
    MR_TypeInfo         type_info;
    MR_TypeInfo         arg_type_info;
    MR_TypeInfo         exp_arg_type_info;
    MR_Word             *arg_ref;
    const MR_DuArgLocn  *arg_locn;

    type_info = (MR_TypeInfo) TypeInfo_for_T;
    exp_arg_type_info = (MR_TypeInfo) TypeInfo_for_ArgT;

    MR_save_transient_registers();

    if (!MR_arg(type_info, (MR_Word *) &Val, ArgNum, &arg_type_info,
        &arg_ref, &arg_locn, MR_NONCANON_ABORT))
    {
        MR_fatal_error(""store.new_arg_ref: argument number out of range"");
    }

    if (MR_compare_type_info(arg_type_info, exp_arg_type_info) !=
        MR_COMPARE_EQUAL)
    {
        MR_fatal_error(""store.new_arg_ref: argument has wrong type"");
    }

    MR_restore_transient_registers();

    if (arg_locn != NULL && arg_locn->MR_arg_bits != 0) {
        MR_offset_incr_hp_msg(ArgRef, MR_SIZE_SLOT_SIZE,
            MR_SIZE_SLOT_SIZE + 1, MR_ALLOC_ID, ""store.ref/2"");
        MR_define_size_slot(0, ArgRef, 1);
        * (MR_Word *) ArgRef = MR_arg_value(arg_ref, arg_locn);
    } else if (arg_ref == &Val) {
        /*
        ** For no_tag types, the argument may have the same address as the
        ** term.  Since the term (Val) is currently on the C stack, we can't
        ** return a pointer to it; so if that is the case, then we need
        ** to copy it to the heap before returning.
        */

        MR_offset_incr_hp_msg(ArgRef, MR_SIZE_SLOT_SIZE,
            MR_SIZE_SLOT_SIZE + 1, MR_ALLOC_ID, ""store.ref/2"");
        MR_define_size_slot(0, ArgRef, 1);
        * (MR_Word *) ArgRef = Val;
    } else {
        ArgRef = (MR_Word) arg_ref;
    }
    S = S0;
}").

:- pragma foreign_proc("C#",
    new_arg_ref(Val::di, ArgNum::in, ArgRef::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    /*
    ** XXX Some dynamic type-checking should be done here to check that
    ** the type of the specified Arg matches the type supplied by the caller.
    ** This will require RTTI.
    */

    ArgRef = new store.Ref(Val, ArgNum);
").

:- pragma foreign_proc("Java",
    new_arg_ref(Val::di, ArgNum::in, ArgRef::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    /*
    ** XXX Some dynamic type-checking should be done here to check that
    ** the type of the specified Arg matches the type supplied by the caller.
    ** This will require RTTI.
    */

    ArgRef = new store.Ref(Val, ArgNum);
").

:- pragma foreign_proc("C",
    set_ref(Ref::in, ValRef::in, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    * (MR_Word *) Ref = * (MR_Word *) ValRef;
    S = S0;
").

:- pragma foreign_proc("C#",
    set_ref(Ref::in, ValRef::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Ref.setValue(ValRef.getValue());
").

:- pragma foreign_proc("Java",
    set_ref(Ref::in, ValRef::in, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Ref.setValue(ValRef.getValue());
").

:- pragma foreign_proc("C",
    set_ref_value(Ref::in, Val::di, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"
    * (MR_Word *) Ref = Val;
    S = S0;
").

:- pragma foreign_proc("Java",
    set_ref_value(Ref::in, Val::di, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    Ref.setValue(Val);
").

:- pragma foreign_proc("C",
    extract_ref_value(_S::di, Ref::in, Val::out),
    [will_not_call_mercury, promise_pure],
"
    Val = * (MR_Word *) Ref;
").

:- pragma foreign_proc("C#",
    extract_ref_value(_S::di, Ref::in, Val::out),
    [will_not_call_mercury, promise_pure],
"
    Val = Ref.getValue();
").

:- pragma foreign_proc("Java",
    extract_ref_value(_S::di, Ref::in, Val::out),
    [will_not_call_mercury, promise_pure],
"
    Val = Ref.getValue();
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    unsafe_arg_ref(Ref::in, Arg::in, ArgRef::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"{
    /* unsafe - does not check type & arity, won't handle no_tag types */
    MR_Word *Ptr;

    Ptr = (MR_Word *) MR_strip_tag((MR_Word) Ref);
    ArgRef = (MR_Word) &Ptr[Arg];
    S = S0;
}").

:- pragma foreign_proc("C#",
    unsafe_arg_ref(Ref::in, Arg::in, ArgRef::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    ArgRef = new store.Ref(Ref.getValue(), Arg);
").

:- pragma foreign_proc("Java",
    unsafe_arg_ref(Ref::in, Arg::in, ArgRef::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    ArgRef = new store.Ref(Ref.getValue(), Arg);
").

:- pragma foreign_proc("C",
    unsafe_new_arg_ref(Val::di, Arg::in, ArgRef::out, S0::di, S::uo),
    [will_not_call_mercury, promise_pure],
"{
    /* unsafe - does not check type & arity, won't handle no_tag types */
    MR_Word *Ptr;

    Ptr = (MR_Word *) MR_strip_tag((MR_Word) Val);
    ArgRef = (MR_Word) &Ptr[Arg];
    S = S0;
}").

:- pragma foreign_proc("C#",
    unsafe_new_arg_ref(Val::di, Arg::in, ArgRef::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    ArgRef = new store.Ref(Val, Arg);
").

:- pragma foreign_proc("Java",
    unsafe_new_arg_ref(Val::di, Arg::in, ArgRef::out, _S0::di, _S::uo),
    [will_not_call_mercury, promise_pure],
"
    ArgRef = new store.Ref(Val, Arg);
").
