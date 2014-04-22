%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: polymorphism.m.
% Main authors: fjh and zs.
%
% This module is a pass over the HLDS.
% It does a syntactic transformation to implement polymorphism, including
% typeclasses, by passing extra `type_info' and `typeclass_info' arguments.
% These arguments are structures that contain, amongst other things,
% higher order predicate terms for the polymorphic procedures or methods.
%
% See notes/type_class_transformation.html for a description of the
% transformation and data structures used to implement type classes.
%
% XXX The way the code in this module handles existential type classes
% and type class constraints is a bit ad hoc, in general; there are
% definitely parts of this code (marked with XXXs below) that could
% do with a rewrite to make it more consistent and hence more maintainable.
%
%-----------------------------------------------------------------------------%
%
% Transformation of polymorphic code:
%
% Every polymorphic predicate is transformed so that it takes one additional
% argument for every type variable in the predicate's type declaration.
% The argument gives information about the type, including higher order
% predicate variables for each of the builtin polymorphic operations
% (currently unify/2, compare/3).
%
%-----------------------------------------------------------------------------%
%
% Representation of type information:
%
% IMPORTANT: ANY CHANGES TO THE DOCUMENTATION HERE MUST BE REFLECTED BY
% SIMILAR CHANGES TO THE #defines IN "runtime/mercury_type_info.h" AND
% TO THE TYPE SPECIALIZATION CODE IN "compiler/higher_order.m".
%
% Type information is represented using one or two cells. The cell which
% is always present is the type_ctor_info structure, whose structure is
% defined in runtime/mercury_type_info.h. The other cell is the type_info
% structure, laid out like this:
%
%   word 0      <pointer to the type_ctor_info structure>
%   word 1+     <the type_infos for the type params, at least one>
%
%   (but see note below for how variable arity types differ)
%
%-----------------------------------------------------------------------------%
%
% Optimization of common case (zero arity types):
%
% The type_info structure itself is redundant if the type has no type
% parameters (i.e. its arity is zero). Therefore if the arity is zero,
% we pass the address of the type_ctor_info structure directly, instead of
% wrapping it up in another cell. The runtime system will look at the first
% field of the cell it is passed. If this field is zero, the cell is a
% type_ctor_info structure for an arity zero type. If this field is not zero,
% the cell is a new type_info structure, with the first field being the
% pointer to the type_ctor_info structure.
%
%-----------------------------------------------------------------------------%
%
% Variable arity types:
%
% There is a slight variation on this for variable-arity type constructors, of
% there are exactly three: pred, func and tuple. Typeinfos of these types
% always have a pointer to the pred/0, func/0 or tuple/0 type_ctor_info,
% regardless of their true arity, so we store the real arity in the type_info
% as well.
%
%   word 0      <pointer to the arity 0 type_ctor_info structure>
%   word 1      <arity of predicate>
%   word 2+     <the type_infos for the type params, if any>
%
%-----------------------------------------------------------------------------%
%
% Sharing type_ctor_info structures:
%
% For compilation models that can put code addresses in static ground terms,
% we can arrange to create one copy of the type_ctor_info structure statically,
% avoiding the need to create other copies at runtime. For compilation models
% that cannot put code addresses in static ground terms, there are a couple
% of things we could do:
%
%   1. allocate all cells at runtime.
%   2. use a shared static type_ctor_info, but initialize its code
%      addresses during startup (that is, during the module
%      initialization code).
%
% We use option 2.
%
%-----------------------------------------------------------------------------%
%
% Example of transformation:
%
% Take the following code as an example, ignoring the requirement for
% superhomogeneous form for clarity:
%
%   :- pred p(T1).
%   :- pred q(T2).
%   :- pred r(T3).
%
%   p(X) :- q([X]), r(0).
%
% We add an extra argument for each type variable:
%
%   :- pred p(type_info(T1), T1).
%   :- pred q(type_info(T2), T2).
%   :- pred r(type_info(T3), T3).
%
% We transform the body of p to this:
%
%   p(TypeInfoT1, X) :-
%       TypeCtorInfoT2 = type_ctor_info(list/1),
%       TypeInfoT2 = type_info(TypeCtorInfoT2, TypeInfoT1),
%       q(TypeInfoT2, [X]),
%       TypeInfoT3 = type_ctor_info(int/0),
%       r(TypeInfoT3, 0).
%
% Note that type_ctor_infos are actually generated as references to a
% single shared type_ctor_info.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Transformation of code using existentially quantified types:
%
% The transformation for existential types is similar to the transformation
% for universally quantified types, except that the type_infos and
% type_class_infos have mode `out' rather than mode `in'.
%
% The argument passing convention is that the new parameters
% introduced by this pass are placed in the following order:
%
%   First the type_infos for unconstrained universally quantified type
%   variables, in the order that the type variables first appear in the
%   argument types;
%
%   then the type_infos for unconstrained existentially quantified type
%   variables, in the order that the type variables first appear in the
%   argument types;
%
%   then the typeclass_infos for universally quantified constraints,
%   in the order that the constraints appear in the class context;
%
%   then the typeclass_infos for existentially quantified constraints,
%   in the order that the constraints appear in the class context;
%
%   and finally the original arguments of the predicate.
%
% Bear in mind that for the purposes of this (and most other) calculations,
% the return parameter of a function counts as the _last_ argument.
%
% The convention for class method implementations is slightly different
% to match the order that the type_infos and typeclass_infos are passed
% in by do_call_class_method (in runtime/mercury_ho_call.c):
%
%   First the type_infos for the unconstrained type variables in the
%   instance declaration, in the order that the type variables first appear
%   in the instance arguments;
%
%   then the typeclass_infos for the class constraints on the instance
%   declaration, in the order that the constraints appear in the declaration;
%
%   then the remainder of the arguments as above.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module check_hlds.polymorphism.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.
:- import_module term.

%-----------------------------------------------------------------------------%

    % Run the polymorphism pass over the whole HLDS.
    %
:- pred polymorphism_process_module(module_info::in, module_info::out) is det.

    % Run the polymorphism pass over a single pred. This is used to transform
    % clauses introduced by unify_proc.m for complicated unification predicates
    % for types for which unification predicates are generated lazily.
    %
    % This predicate should be used with caution. polymorphism.m expects that
    % the argument types of called predicates have not been transformed yet.
    % This predicate will not work correctly after the original pass of
    % polymorphism has been run if the predicate to be processed calls
    % any polymorphic predicates which require type_infos or typeclass_infos
    % to be added to the argument list.
    %
:- pred polymorphism_process_generated_pred(pred_id::in,
    module_info::in, module_info::out) is det.

    % Add the type_info variables for a complicated unification to
    % the appropriate fields in the unification and the goal_info.
    %
:- pred unification_typeinfos_rtti_varmaps(mer_type::in, rtti_varmaps::in,
    unification::in, unification::out, hlds_goal_info::in, hlds_goal_info::out)
    is det.

    % Add the type_info variables for a new call goal. This predicate assumes
    % that process_module has already been run so the called pred has already
    % been processed.
    %
    % XXX This predicate does not yet handle calls whose arguments include
    % existentially quantified types or type class constraints.
    %
:- pred polymorphism_process_new_call(pred_info::in, proc_info::in,
    pred_id::in, proc_id::in, list(prog_var)::in, builtin_state::in,
    maybe(call_unify_context)::in, sym_name::in, hlds_goal_info::in,
    hlds_goal::out, poly_info::in, poly_info::out) is det.

    % Given a list of types, create a list of variables to hold the type_info
    % for those types, and create a list of goals to initialize those type_info
    % variables to the appropriate type_info structures for the types.
    % Update the varset and vartypes accordingly.
    %
:- pred polymorphism_make_type_info_vars(list(mer_type)::in, term.context::in,
    list(prog_var)::out, list(hlds_goal)::out, poly_info::in, poly_info::out)
    is det.

    % Likewise, but for a single type.
    %
:- pred polymorphism_make_type_info_var(mer_type::in, term.context::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

:- type int_or_var
    --->    iov_int(int)
    ;       iov_var(prog_var).

    % gen_extract_type_info(ModuleInfo, TypeVar, Kind, TypeClassInfoVar,
    %   IndexIntOrVar, Goals, TypeInfoVar, ...):
    %
    % Generate code to extract a type_info variable from a given slot of a
    % typeclass_info variable, by calling type_info_from_typeclass_info from
    % private_builtin. TypeVar is the type variable to which this type_info
    % variable corresponds. Kind is the kind of the type variable.
    % TypeClassInfoVar is the variable holding the type_class_info.
    % Index specifies which slot it is. The procedure returns TypeInfoVar,
    % which is a fresh variable holding the type_info, and Goals, which is
    % the code generated to initialize TypeInfoVar.
    %
:- pred gen_extract_type_info(module_info::in, tvar::in, kind::in,
    prog_var::in, int_or_var::in, list(hlds_goal)::out, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

:- type poly_info.

    % Extract some fields from a pred_info and proc_info and use them to
    % create a poly_info, for use by the polymorphism transformation.
    %
:- pred create_poly_info(module_info::in, pred_info::in,
    proc_info::in, poly_info::out) is det.

    % Update the fields in a pred_info and proc_info with
    % the values in a poly_info.
    %
:- pred poly_info_extract(poly_info::in, pred_info::in, pred_info::out,
    proc_info::in, proc_info::out, module_info::out) is det.

    % Build the type describing the typeclass_info for the
    % given prog_constraint.
    %
:- pred build_typeclass_info_type(prog_constraint::in, mer_type::out) is det.

    % Check if a type is the `typeclass_info' type introduced by this pass.
    %
:- pred type_is_typeclass_info(mer_type::in) is semidet.

    % Check if a type is either the `type_info' type or the
    % `type_ctor_info' type introduced by this pass.
    %
:- pred type_is_type_info_or_ctor_type(mer_type::in) is semidet.

    % Construct the type of the type_info for the given type.
    %
:- pred build_type_info_type(mer_type::in, mer_type::out) is det.

    % Look up the pred_id and proc_id for a type specific
    % unification/comparison/index/initialise predicate.
    %
:- pred get_special_proc(mer_type::in, special_pred_id::in,
    module_info::in, sym_name::out, pred_id::out, proc_id::out) is semidet.
:- pred get_special_proc_det(mer_type::in, special_pred_id::in,
    module_info::in, sym_name::out, pred_id::out, proc_id::out) is det.

    % Convert a higher order pred term to a lambda goal.
    %
:- pred convert_pred_to_lambda_goal(purity::in, lambda_eval_method::in,
    prog_var::in, pred_id::in, proc_id::in, list(prog_var)::in,
    list(mer_type)::in, unify_context::in, hlds_goal_info::in, context::in,
    module_info::in, unify_rhs::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

    % fix_undetermined_mode_lambda_goal(ProcId, Functor0, Functor, ModuleInfo)
    %
    % This is called by mode checking when it figures out which mode that a
    % lambda goal converted from a higher order pred term should call.
    % Functor0 must have been produced by `convert_pred_to_lambda_goal'.
    %
:- pred fix_undetermined_mode_lambda_goal(proc_id::in,
    unify_rhs::in(rhs_lambda_goal), unify_rhs::out(rhs_lambda_goal),
    module_info::in) is det.

    % init_type_info_var(Type, ArgVars, TypeInfoVar, TypeInfoGoal,
    %   !VarSet, !VarTypes) :-
    %
    % Create the unification the constructs the second cell of a type_info
    % for Type. ArgVars should contain the arguments of this unification.
    %
    % This unification WILL lead to the creation of cells on the heap
    % at runtime.
    %
    % The first variable in ArgVars should be bound to the type_ctor_info
    % for Type's principal type constructor. If that type constructor is
    % variable arity, the next variable in ArgVars should be bound to an
    % integer giving Type's actual arity. The remaining variables in
    % ArgVars should be bound to the type_infos or type_ctor_infos giving
    % Type's argument types.
    %
:- pred init_type_info_var(mer_type::in, list(prog_var)::in,
    maybe(prog_var)::in, prog_var::out, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % init_const_type_ctor_info_var(Type, TypeCtor,
    %   TypeCtorInfoVar, TypeCtorConsId, TypeCtorInfoGoal,
    %   !VarSet, !VarTypes, !RttiVarMaps):
    %
    % Create the unification (returned as TypeCtorInfoGoal) that binds a
    % new variable (returned as TypeCtorInfoVar) to the type_ctor_info
    % representing TypeCtor. This will be the constant represented by
    % TypeCtorConsId.
    %
    % This unification WILL NOT lead to the creation of a cell on the
    % heap at runtime; it will cause TypeCtorInfoVar to refer to the
    % statically allocated type_ctor_info cell for the type, allocated
    % in the module that defines the type.
    %
    % We take Type as input for historical reasons: we record Type as
    % the type whose type constructor TypeCtor is, in the type of
    % TypeCtorInfoVar.
    %
:- pred init_const_type_ctor_info_var(mer_type::in, type_ctor::in,
    prog_var::out, cons_id::out, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

:- type type_info_kind
    --->    type_info
    ;       type_ctor_info.

:- pred new_type_info_var_raw(mer_type::in, type_info_kind::in,
    prog_var::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.from_ground_term_util.
:- import_module hlds.const_struct.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.special_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% This whole section just traverses the module structure.
% We do two passes, the first to fix up the clauses_info and proc_infos
% (and in fact everything except the pred_info argtypes), the second to fix up
% the pred_info argtypes. The reason we need two passes is that the first pass
% looks at the argtypes of the called predicates, and so we need to make
% sure we don't muck them up before we've finished the first pass.

polymorphism_process_module(!ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, Preds0),
    map.keys(Preds0, PredIds0),
    list.foldl(maybe_polymorphism_process_pred, PredIds0, !ModuleInfo),
    module_info_get_preds(!.ModuleInfo, Preds1),
    map.keys(Preds1, PredIds1),
    list.foldl(fixup_pred_polymorphism, PredIds1, !ModuleInfo),
    expand_class_method_bodies(!ModuleInfo).

:- pred maybe_polymorphism_process_pred(pred_id::in,
    module_info::in, module_info::out) is det.

maybe_polymorphism_process_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    (
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredArity = pred_info_orig_arity(PredInfo),
        no_type_info_builtin(PredModule, PredName, PredArity)
    ->
        % Just copy the clauses to the proc_infos.
        copy_module_clauses_to_procs([PredId], !ModuleInfo)
    ;
        polymorphism_process_pred_msg(PredId, !ModuleInfo)
    ).

%---------------------------------------------------------------------------%

:- pred fixup_pred_polymorphism(pred_id::in,
    module_info::in, module_info::out) is det.

fixup_pred_polymorphism(PredId, !ModuleInfo) :-
    % Recompute the arg types by finding the headvars and the var->type mapping
    % (from the clauses_info) and applying the type mapping to the extra
    % headvars to get the new arg types. Note that we are careful to only apply
    % the mapping to the extra head vars, not to the originals, because
    % otherwise we would stuff up the arg types for unification predicates for
    % equivalence types.

    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_get_vartypes(ClausesInfo0, VarTypes0),
    clauses_info_get_headvars(ClausesInfo0, HeadVars),

    pred_info_get_arg_types(PredInfo0, TypeVarSet, ExistQVars, ArgTypes0),
    proc_arg_vector_partition_poly_args(HeadVars, ExtraHeadVarList,
        OldHeadVarList),

    lookup_var_types(VarTypes0, ExtraHeadVarList, ExtraArgTypes),
    ArgTypes = ExtraArgTypes ++ ArgTypes0,
    pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes,
        PredInfo0, PredInfo1),

    % If the clauses bind some existentially quantified type variables,
    % introduce exists_casts goals for affected head variables, including
    % the new type_info and typeclass_info arguments. Make sure the types
    % of the internal versions of type_infos for those type variables in the
    % variable types map are as specific as possible.

    (
        ExistQVars = [_ | _],
        % This can fail for unification procedures of equivalence types.
        lookup_var_types(VarTypes0, OldHeadVarList, OldHeadVarTypes),
        type_list_subsumes(ArgTypes0, OldHeadVarTypes, Subn),
        \+ map.is_empty(Subn)
    ->
        pred_info_set_existq_tvar_binding(Subn, PredInfo1, PredInfo2),
        polymorphism_introduce_exists_casts_pred(!.ModuleInfo, PredInfo2,
            PredInfo)
    ;
        PredInfo = PredInfo1
    ),

    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

:- pred polymorphism_introduce_exists_casts_pred(module_info::in,
    pred_info::in, pred_info::out) is det.

polymorphism_introduce_exists_casts_pred(ModuleInfo, !PredInfo) :-
    pred_info_get_procedures(!.PredInfo, Procs0),
    map.map_values_only(
        (pred(!.ProcInfo::in, !:ProcInfo::out) is det :-
            % Add the extra goals to each procedure.
            introduce_exists_casts_proc(ModuleInfo, !.PredInfo, !ProcInfo)
        ), Procs0, Procs),
    pred_info_set_procedures(Procs, !PredInfo).

%---------------------------------------------------------------------------%

:- pred polymorphism_process_pred_msg(pred_id::in,
    module_info::in, module_info::out) is det.

polymorphism_process_pred_msg(PredId, !ModuleInfo) :-
    % Since polymorphism transforms not just the procedures defined
    % in the module being compiled, but also all the procedures in
    % all the imported modules, this message can be printed A LOT,
    % even though it is almost never of interest.
    % That is why we enable it only when requested.

    trace [compiletime(flag("poly_msgs")), io(!IO)] (
        write_pred_progress_message("% Transforming polymorphism for ",
            PredId, !.ModuleInfo, !IO)
    ),
    polymorphism_process_pred(PredId, !ModuleInfo).

polymorphism_process_generated_pred(PredId, !ModuleInfo) :-
    polymorphism_process_pred(PredId, !ModuleInfo),
    fixup_pred_polymorphism(PredId, !ModuleInfo).

:- mutable(selected_pred, bool, no, ground, [untrailed]).
:- mutable(level, int, 0, ground, [untrailed]).

:- pred polymorphism_process_pred(pred_id::in,
    module_info::in, module_info::out) is det.

polymorphism_process_pred(PredId, !ModuleInfo) :-
    trace [compiletime(flag("debug_poly_caches"))] (
        promise_pure (
            % Replace 99999 with the id of the predicate you want to debug.
            ( pred_id_to_int(PredId) = 15 ->
                impure set_selected_pred(yes)
            ;
                true
            )
        )
    ),

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),

    % Run the polymorphism pass over the clauses_info, updating the headvars,
    % goals, varsets, types, etc., and computing some information in the
    % poly_info.

    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    polymorphism_process_clause_info(!.ModuleInfo, PredInfo0,
        ClausesInfo0, ClausesInfo, Info, ExtraArgModes),
    poly_info_get_module_info(Info, !:ModuleInfo),
    poly_info_get_const_struct_db(Info, ConstStructDb),
    module_info_set_const_struct_db(ConstStructDb, !ModuleInfo),

    poly_info_get_typevarset(Info, TypeVarSet),
    pred_info_set_typevarset(TypeVarSet, PredInfo0, PredInfo1),
    pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo2),

    % Do a pass over the proc_infos, copying the relevant information
    % from the clauses_info and the poly_info, and updating all the argmodes
    % with modes for the extra arguments.

    ProcIds = pred_info_procids(PredInfo2),
    pred_info_get_procedures(PredInfo2, Procs0),
    list.foldl(polymorphism_process_proc_in_table(PredInfo2, ClausesInfo,
        ExtraArgModes), ProcIds, Procs0, Procs),
    pred_info_set_procedures(Procs, PredInfo2, PredInfo),

    trace [compiletime(flag("debug_poly_caches"))] (
        promise_pure (
            impure set_selected_pred(no)
        )
    ),

    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred polymorphism_process_clause_info(module_info::in, pred_info::in,
    clauses_info::in, clauses_info::out, poly_info::out,
    poly_arg_vector(mer_mode)::out) is det.

polymorphism_process_clause_info(ModuleInfo0, PredInfo0, !ClausesInfo, !:Info,
        ExtraArgModes) :-
    init_poly_info(ModuleInfo0, PredInfo0, !.ClausesInfo, !:Info),
    !.ClausesInfo = clauses_info(_VarSet, ExplicitVarTypes, _TVarNameMap,
        _VarTypes, HeadVars0, ClausesRep0, ItemNumbers,
        _RttiVarMaps, HaveForeignClauses),

    setup_headvars(PredInfo0, HeadVars0, HeadVars,
        ExtraArgModes, UnconstrainedTVars,
        ExtraTypeInfoHeadVars, ExistTypeClassInfoHeadVars, !Info),

    get_clause_list(ClausesRep0, Clauses0),
    list.map_foldl(
        polymorphism_process_clause(PredInfo0, HeadVars0, HeadVars,
            UnconstrainedTVars, ExtraTypeInfoHeadVars,
            ExistTypeClassInfoHeadVars),
        Clauses0, Clauses, !Info),

    % Set the new values of the fields in clauses_info.
    poly_info_get_varset(!.Info, VarSet),
    poly_info_get_var_types(!.Info, VarTypes),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps),
    set_clause_list(Clauses, ClausesRep),
    map.init(TVarNameMap), % This is only used while adding the clauses.
    !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, TVarNameMap,
        VarTypes, HeadVars, ClausesRep, ItemNumbers,
        RttiVarMaps, HaveForeignClauses).

:- pred polymorphism_process_clause(pred_info::in,
    proc_arg_vector(prog_var)::in, proc_arg_vector(prog_var)::in,
    list(tvar)::in, list(prog_var)::in, list(prog_var)::in,
    clause::in, clause::out, poly_info::in, poly_info::out) is det.

polymorphism_process_clause(PredInfo0, OldHeadVars, NewHeadVars,
        UnconstrainedTVars, ExtraTypeInfoHeadVars,
        ExistTypeClassInfoHeadVars, !Clause, !Info) :-
    ( pred_info_is_imported(PredInfo0) ->
        true
    ;
        Goal0 = !.Clause ^ clause_body,

        % Process any polymorphic calls inside the goal.
        empty_cache_maps(!Info),
        poly_info_set_num_reuses(0, !Info),
        polymorphism_process_goal(Goal0, Goal1, !Info),

        % Generate code to construct the typeclass_infos and type_infos
        % for existentially quantified type vars.
        produce_existq_tvars(PredInfo0, OldHeadVars,
            UnconstrainedTVars, ExtraTypeInfoHeadVars,
            ExistTypeClassInfoHeadVars, Goal1, Goal2, !Info),

        pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
        fixup_quantification(NewHeadVars, ExistQVars, Goal2, Goal, !Info),
        !Clause ^ clause_body := Goal
    ).

:- pred polymorphism_process_proc_in_table(pred_info::in, clauses_info::in,
    poly_arg_vector(mer_mode)::in, proc_id::in,
    proc_table::in, proc_table::out) is det.

polymorphism_process_proc_in_table(PredInfo, ClausesInfo, ExtraArgModes,
        ProcId, !ProcTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    polymorphism_process_proc(PredInfo, ClausesInfo, ExtraArgModes, ProcId,
        ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable).

:- pred polymorphism_process_proc(pred_info::in, clauses_info::in,
    poly_arg_vector(mer_mode)::in, proc_id::in, proc_info::in, proc_info::out)
    is det.

polymorphism_process_proc(PredInfo, ClausesInfo, ExtraArgModes, ProcId,
        !ProcInfo) :-
    % Copy all the information from the clauses_info into the proc_info.
    (
        (
            pred_info_is_imported(PredInfo)
        ;
            pred_info_is_pseudo_imported(PredInfo),
            hlds_pred.in_in_unification_proc_id(ProcId)
        )
    ->
        % We need to set these fields in the proc_info here, because some parts
        % of the compiler (e.g. unused_args.m) depend on these fields being
        % valid even for imported procedures.

        % XXX ARGVEC - when the proc_info uses the proc_arg_vector just
        % pass the headvar vector directly to the proc_info.
        clauses_info_get_headvars(ClausesInfo, HeadVars),
        HeadVarList = proc_arg_vector_to_list(HeadVars),
        clauses_info_get_rtti_varmaps(ClausesInfo, RttiVarMaps),
        clauses_info_get_varset(ClausesInfo, VarSet),
        clauses_info_get_vartypes(ClausesInfo, VarTypes),
        proc_info_set_headvars(HeadVarList, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo)
    ;
        copy_clauses_to_proc(ProcId, ClausesInfo, !ProcInfo)
    ),

    % Add the ExtraArgModes to the proc_info argmodes.
    % XXX ARGVEC - revisit this when the proc_info uses proc_arg_vectors.
    proc_info_get_argmodes(!.ProcInfo, ArgModes1),
    ExtraArgModesList = poly_arg_vector_to_list(ExtraArgModes),
    ArgModes = ExtraArgModesList ++ ArgModes1,
    proc_info_set_argmodes(ArgModes, !ProcInfo).

    % XXX document me
    %
    % XXX the following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred setup_headvars(pred_info::in, proc_arg_vector(prog_var)::in,
    proc_arg_vector(prog_var)::out, poly_arg_vector(mer_mode)::out,
    list(tvar)::out, list(prog_var)::out, list(prog_var)::out,
    poly_info::in, poly_info::out) is det.

setup_headvars(PredInfo, !HeadVars, ExtraArgModes,
        UnconstrainedTVars, ExtraHeadTypeInfoVars,
        ExistHeadTypeClassInfoVars, !Info) :-
    pred_info_get_origin(PredInfo, Origin),
    ExtraArgModes0 = poly_arg_vector_init : poly_arg_vector(mer_mode),
    (
        Origin = origin_instance_method(_, InstanceMethodConstraints),
        setup_headvars_instance_method(PredInfo,
            InstanceMethodConstraints, !HeadVars,
            UnconstrainedTVars, ExtraHeadTypeInfoVars,
            ExistHeadTypeClassInfoVars,
            ExtraArgModes0, ExtraArgModes, !Info)
    ;
        ( Origin = origin_special_pred(_)
        ; Origin = origin_transformed(_, _, _)
        ; Origin = origin_created(_)
        ; Origin = origin_assertion(_, _)
        ; Origin = origin_lambda(_, _, _)
        ; Origin = origin_user(_)
        ),
        pred_info_get_class_context(PredInfo, ClassContext),
        InstanceTVars = [],
        InstanceUnconstrainedTVars = [],
        InstanceUnconstrainedTypeInfoVars = [],
        setup_headvars_2(PredInfo, ClassContext, InstanceTVars,
            InstanceUnconstrainedTVars, InstanceUnconstrainedTypeInfoVars,
            !HeadVars, UnconstrainedTVars,
            ExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars,
            ExtraArgModes0, ExtraArgModes, !Info)
    ).

    % For class method implementations, do_call_class_method takes the
    % type_infos and typeclass_infos from the typeclass_info and pastes them
    % onto the front of the argument list. We need to match that order here.
    %
:- pred setup_headvars_instance_method(pred_info::in,
    instance_method_constraints::in,
    proc_arg_vector(prog_var)::in, proc_arg_vector(prog_var)::out,
    list(tvar)::out, list(prog_var)::out, list(prog_var)::out,
    poly_arg_vector(mer_mode)::in, poly_arg_vector(mer_mode)::out,
    poly_info::in, poly_info::out) is det.

setup_headvars_instance_method(PredInfo,
        InstanceMethodConstraints, !HeadVars,
        UnconstrainedTVars, ExtraHeadTypeInfoVars,
        ExistHeadTypeClassInfoVars, !ExtraArgModes, !Info) :-

    InstanceMethodConstraints = instance_method_constraints(_,
        InstanceTypes, InstanceConstraints, ClassContext),

    type_vars_list(InstanceTypes, InstanceTVars),
    get_unconstrained_tvars(InstanceTVars, InstanceConstraints,
        UnconstrainedInstanceTVars),
    pred_info_get_arg_types(PredInfo, ArgTypeVarSet, _, _),
    make_head_vars(UnconstrainedInstanceTVars,
        ArgTypeVarSet, UnconstrainedInstanceTypeInfoVars, !Info),
    make_typeclass_info_head_vars(do_record_type_info_locns,
        InstanceConstraints, InstanceHeadTypeClassInfoVars, !Info),

    proc_arg_vector_set_instance_type_infos(UnconstrainedInstanceTypeInfoVars,
        !HeadVars),
    proc_arg_vector_set_instance_typeclass_infos(InstanceHeadTypeClassInfoVars,
         !HeadVars),

    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    list.foldl(rtti_reuse_typeclass_info_var,
        InstanceHeadTypeClassInfoVars, RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info),

    in_mode(InMode),
    list.duplicate(list.length(UnconstrainedInstanceTypeInfoVars),
        InMode, UnconstrainedInstanceTypeInfoModes),
    list.duplicate(list.length(InstanceHeadTypeClassInfoVars),
        InMode, InstanceHeadTypeClassInfoModes),
    poly_arg_vector_set_instance_type_infos(
        UnconstrainedInstanceTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_instance_typeclass_infos(
        InstanceHeadTypeClassInfoModes, !ExtraArgModes),

    setup_headvars_2(PredInfo, ClassContext,
        InstanceTVars,
        UnconstrainedInstanceTVars, UnconstrainedInstanceTypeInfoVars,
        !HeadVars,
        UnconstrainedTVars, ExtraHeadTypeInfoVars,
        ExistHeadTypeClassInfoVars, !ExtraArgModes, !Info).

:- pred setup_headvars_2(pred_info::in, prog_constraints::in,
    list(tvar)::in, list(tvar)::in, list(prog_var)::in,
    proc_arg_vector(prog_var)::in, proc_arg_vector(prog_var)::out,
    list(tvar)::out, list(prog_var)::out, list(prog_var)::out,
    poly_arg_vector(mer_mode)::in, poly_arg_vector(mer_mode)::out,
    poly_info::in, poly_info::out) is det.

setup_headvars_2(PredInfo, ClassContext,
        InstanceTVars, UnconstrainedInstanceTVars,
        UnconstrainedInstanceTypeInfoVars, HeadVars0,
        HeadVars, AllUnconstrainedTVars,
        AllExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars,
        !ExtraArgModes, !Info) :-

    % Grab the appropriate fields from the pred_info.
    pred_info_get_arg_types(PredInfo, ArgTypeVarSet, ExistQVars, ArgTypes),

    % Insert extra head variables to hold the address of the type_infos
    % and typeclass_infos. We insert one variable for each unconstrained
    % type variable (for the type_info) and one variable for each
    % constraint (for the typeclass_info).
    %
    % The order of these variables is important, and must match the order
    % specified at the top of this file.

    % Make a fresh variable for each class constraint, returning a list of
    % variables that appear in the constraints, along with the location of
    % the type infos for them. For the existential constraints, we want
    % the rtti_varmaps to contain the internal view of the types (that is,
    % with type variables bound) so we may need to look up the actual
    % constraints in the constraint map. For the universal constraints there
    % is no distinction between the internal views and the external view, so
    % we just use the constraints from the class context.
    ClassContext = constraints(UnivConstraints, ExistConstraints),
    prog_type.constraint_list_get_tvars(UnivConstraints,
        UnivConstrainedTVars),
    prog_type.constraint_list_get_tvars(ExistConstraints,
        ExistConstrainedTVars),
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    get_improved_exists_head_constraints(ConstraintMap, ExistConstraints,
        ActualExistConstraints),
    (
        pred_info_get_markers(PredInfo, PredMarkers),
        check_marker(PredMarkers, marker_class_method)
    ->
        % For class methods we record the type_info_locns even for the
        % existential constraints. It is easier to do it here than when we
        % are expanding class method bodies, and we know there won't be any
        % references to the type_info after the instance method call so
        % recording them now won't be a problem.
        RecordExistQLocns = do_record_type_info_locns
    ;
        RecordExistQLocns = do_not_record_type_info_locns
    ),
    make_typeclass_info_head_vars(RecordExistQLocns, ActualExistConstraints,
        ExistHeadTypeClassInfoVars, !Info),
    make_typeclass_info_head_vars(do_record_type_info_locns, UnivConstraints,
        UnivHeadTypeClassInfoVars, !Info),

    type_vars_list(ArgTypes, HeadTypeVars),
    list.delete_elems(HeadTypeVars, UnivConstrainedTVars,
        UnconstrainedTVars0),
    list.delete_elems(UnconstrainedTVars0, ExistConstrainedTVars,
        UnconstrainedTVars1),

    % Typeinfos for the instance tvars have already been introduced by
    % setup_headvars_instance_method.
    list.delete_elems(UnconstrainedTVars1, InstanceTVars,
        UnconstrainedTVars2),
    list.remove_dups(UnconstrainedTVars2, UnconstrainedTVars),

    (
        ExistQVars = [],
        % Optimize common case.
        UnconstrainedUnivTVars = UnconstrainedTVars,
        UnconstrainedExistTVars = [],
        ExistHeadTypeInfoVars = []
    ;
        ExistQVars = [_ | _],
        list.delete_elems(UnconstrainedTVars, ExistQVars,
            UnconstrainedUnivTVars),
        list.delete_elems(UnconstrainedTVars, UnconstrainedUnivTVars,
            UnconstrainedExistTVars),
        make_head_vars(UnconstrainedExistTVars, ArgTypeVarSet,
            ExistHeadTypeInfoVars, !Info)
    ),

    make_head_vars(UnconstrainedUnivTVars, ArgTypeVarSet,
        UnivHeadTypeInfoVars, !Info),
    ExtraHeadTypeInfoVars = UnivHeadTypeInfoVars ++ ExistHeadTypeInfoVars,

    AllExtraHeadTypeInfoVars = UnconstrainedInstanceTypeInfoVars
        ++ ExtraHeadTypeInfoVars,
    list.condense([UnconstrainedInstanceTVars, UnconstrainedUnivTVars,
        UnconstrainedExistTVars], AllUnconstrainedTVars),

    proc_arg_vector_set_univ_type_infos(UnivHeadTypeInfoVars,
        HeadVars0, HeadVars1),
    proc_arg_vector_set_exist_type_infos(ExistHeadTypeInfoVars,
        HeadVars1, HeadVars2),
    proc_arg_vector_set_univ_typeclass_infos(UnivHeadTypeClassInfoVars,
        HeadVars2, HeadVars3),
    proc_arg_vector_set_exist_typeclass_infos(ExistHeadTypeClassInfoVars,
        HeadVars3, HeadVars),

    % Figure out the modes of the introduced type_info and typeclass_info
    % arguments.

    in_mode(In),
    out_mode(Out),
    list.length(UnconstrainedUnivTVars, NumUnconstrainedUnivTVars),
    list.length(UnconstrainedExistTVars, NumUnconstrainedExistTVars),
    list.length(UnivHeadTypeClassInfoVars, NumUnivClassInfoVars),
    list.length(ExistHeadTypeClassInfoVars, NumExistClassInfoVars),
    list.duplicate(NumUnconstrainedUnivTVars, In, UnivTypeInfoModes),
    list.duplicate(NumUnconstrainedExistTVars, Out, ExistTypeInfoModes),
    list.duplicate(NumUnivClassInfoVars, In, UnivTypeClassInfoModes),
    list.duplicate(NumExistClassInfoVars, Out, ExistTypeClassInfoModes),
    poly_arg_vector_set_univ_type_infos(UnivTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_exist_type_infos(ExistTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_univ_typeclass_infos(UnivTypeClassInfoModes,
        !ExtraArgModes),
    poly_arg_vector_set_exist_typeclass_infos(ExistTypeClassInfoModes,
        !ExtraArgModes),

    % Add the locations of the typeinfos for unconstrained, universally
    % quantified type variables to the initial rtti_varmaps. Also add the
    % locations of typeclass_infos.
    %
    some [!RttiVarMaps] (
        poly_info_get_rtti_varmaps(!.Info, !:RttiVarMaps),

        ToLocn = (pred(TheVar::in, TheLocn::out) is det :-
            TheLocn = type_info(TheVar)),

        list.map(ToLocn, UnivHeadTypeInfoVars, UnivTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            UnconstrainedUnivTVars, UnivTypeLocns, !RttiVarMaps),

        list.map(ToLocn, ExistHeadTypeInfoVars, ExistTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            UnconstrainedExistTVars, ExistTypeLocns, !RttiVarMaps),

        list.map(ToLocn, UnconstrainedInstanceTypeInfoVars,
            UnconstrainedInstanceTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            UnconstrainedInstanceTVars,
            UnconstrainedInstanceTypeLocns, !RttiVarMaps),

        list.foldl(rtti_reuse_typeclass_info_var,
            UnivHeadTypeClassInfoVars, !RttiVarMaps),

        poly_info_set_rtti_varmaps(!.RttiVarMaps, !Info)
    ).

    % Generate code to produce the values of type_infos and typeclass_infos
    % for existentially quantified type variables in the head.
    %
    % XXX The following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred produce_existq_tvars(pred_info::in, proc_arg_vector(prog_var)::in,
    list(tvar)::in, list(prog_var)::in, list(prog_var)::in,
    hlds_goal::in, hlds_goal::out, poly_info::in, poly_info::out) is det.

produce_existq_tvars(PredInfo, HeadVars, UnconstrainedTVars,
        TypeInfoHeadVars, ExistTypeClassInfoHeadVars, Goal0, Goal, !Info) :-
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_tvar_kinds(PredInfo, KindMap),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % Generate code to produce values for any existentially quantified
    % typeclass_info variables in the head.

    PredExistConstraints = PredClassContext ^ exist_constraints,
    get_improved_exists_head_constraints(ConstraintMap, PredExistConstraints,
        ActualExistConstraints),
    ExistQVarsForCall = [],
    Goal0 = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    make_typeclass_info_vars(ActualExistConstraints, ExistQVarsForCall,
        Context, ExistTypeClassVarsMCAs, ExtraTypeClassGoals, !Info),
    assoc_list.keys(ExistTypeClassVarsMCAs, ExistTypeClassVars),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    list.foldl(rtti_reuse_typeclass_info_var, ExistTypeClassVars,
        RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info),
    assign_var_list(ExistTypeClassInfoHeadVars,
        ExistTypeClassVars, ExtraTypeClassUnifyGoals),

    % Figure out the bindings for any unconstrained existentially quantified
    % type variables in the head.

    ( vartypes_is_empty(VarTypes0) ->
        % This can happen for compiler generated procedures.
        map.init(PredToActualTypeSubst)
    ;
        HeadVarList = proc_arg_vector_to_list(HeadVars),
        lookup_var_types(VarTypes0, HeadVarList, ActualArgTypes),
        type_list_subsumes(ArgTypes, ActualArgTypes, ArgTypeSubst)
    ->
        PredToActualTypeSubst = ArgTypeSubst
    ;
        % This can happen for unification procedures of equivalence types
        % error("polymorphism.m: type_list_subsumes failed")
        map.init(PredToActualTypeSubst)
    ),

    % Apply the type bindings to the unconstrained type variables to give
    % the actual types, and then generate code to initialize the type_infos
    % for those types.

    apply_subst_to_tvar_list(KindMap, PredToActualTypeSubst,
        UnconstrainedTVars, ActualTypes),
    polymorphism_do_make_type_info_vars(ActualTypes, Context,
        TypeInfoVarsMCAs, ExtraTypeInfoGoals, !Info),
    assoc_list.keys(TypeInfoVarsMCAs, TypeInfoVars),
    assign_var_list(TypeInfoHeadVars, TypeInfoVars, ExtraTypeInfoUnifyGoals),
    list.condense([[Goal0 | ExtraTypeClassGoals], ExtraTypeClassUnifyGoals,
        ExtraTypeInfoGoals, ExtraTypeInfoUnifyGoals], GoalList),
    conj_list_to_goal(GoalList, GoalInfo, Goal).

:- pred assign_var_list(list(prog_var)::in, list(prog_var)::in,
    list(hlds_goal)::out) is det.

assign_var_list([], [_ | _], _) :-
    unexpected($module, $pred, "length mismatch").
assign_var_list([_ | _], [], _) :-
    unexpected($module, $pred, "length mismatch").
assign_var_list([], [], []).
assign_var_list([Var1 | Vars1], [Var2 | Vars2], [Goal | Goals]) :-
    assign_var(Var1, Var2, Goal),
    assign_var_list(Vars1, Vars2, Goals).

:- pred assign_var(prog_var::in, prog_var::in, hlds_goal::out) is det.

assign_var(Var1, Var2, Goal) :-
    ( Var1 = Var2 ->
        Goal = true_goal
    ;
        term.context_init(Context),
        create_pure_atomic_complicated_unification(Var1, rhs_var(Var2),
            Context, umc_explicit, [], Goal)
    ).

:- pred get_improved_exists_head_constraints(constraint_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

get_improved_exists_head_constraints(ConstraintMap,  ExistConstraints,
        ActualExistConstraints) :-
    list.length(ExistConstraints, NumExistConstraints),
    (
        search_hlds_constraint_list(ConstraintMap, unproven, goal_id(0),
            NumExistConstraints, ActualExistConstraints0)
    ->
        ActualExistConstraints = ActualExistConstraints0
    ;
        % Some predicates, for example typeclass methods and predicates for
        % which we inferred the type, don't have constraint map entries for
        % the head constraints. In these cases we can just use the external
        % constraints, since there can't be any difference between them and
        % the internal ones.
        ActualExistConstraints = ExistConstraints
    ).

%-----------------------------------------------------------------------------%

:- pred polymorphism_process_goal(hlds_goal::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        % We don't need to add type_infos for higher order calls, since the
        % type_infos are added when the closures are constructed, not when
        % they are called.
        GoalExpr0 = generic_call(_, _, _, _, _),
        Goal = Goal0
    ;
        GoalExpr0 = plain_call(PredId, _, ArgVars0, _, _, _),
        polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
            ExtraVars, ExtraGoals, !Info),
        ArgVars = ExtraVars ++ ArgVars0,
        CallExpr = GoalExpr0 ^ call_args := ArgVars,
        Call = hlds_goal(CallExpr, GoalInfo),
        GoalList = ExtraGoals ++ [Call],
        conj_list_to_goal(GoalList, GoalInfo0, Goal)
    ;
        GoalExpr0 = call_foreign_proc(_, PredId, _, _, _, _, _),
        poly_info_get_module_info(!.Info, ModuleInfo),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredArity = pred_info_orig_arity(PredInfo),

        ( no_type_info_builtin(PredModule, PredName, PredArity) ->
            Goal = Goal0
        ;
            polymorphism_process_foreign_proc(PredInfo, GoalExpr0, GoalInfo0,
                Goal, !Info)
        )
    ;
        GoalExpr0 = unify(XVar, Y, Mode, Unification, UnifyContext),
        polymorphism_process_unify(XVar, Y, Mode, Unification, UnifyContext,
            GoalInfo0, Goal, !Info)
    ;
        % The rest of the cases just process goals recursively.
        (
            GoalExpr0 = conj(ConjType, Goals0),
            (
                ConjType = plain_conj,
                polymorphism_process_plain_conj(Goals0, Goals, !Info)
            ;
                ConjType = parallel_conj,
                get_cache_maps_snapshot("parconj", InitialSnapshot, !Info),
                polymorphism_process_par_conj(Goals0, Goals, InitialSnapshot,
                    !Info)
                % Unlike with disjunctions, we do not have to reset to
                % InitialSnapshot.
            ),
            GoalExpr = conj(ConjType, Goals)
        ;
            GoalExpr0 = disj(Goals0),
            get_cache_maps_snapshot("disj", InitialSnapshot, !Info),
            polymorphism_process_disj(Goals0, Goals, InitialSnapshot, !Info),
            set_cache_maps_snapshot("after disj", InitialSnapshot, !Info),
            GoalExpr = disj(Goals)
        ;
            GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
            get_cache_maps_snapshot("ite", InitialSnapshot, !Info),
            polymorphism_process_goal(Cond0, Cond, !Info),
            % If we allowed a type_info created inside Cond to be reused
            % in Then, then we are adding an output variable to Cond.
            % If Cond scope had no outputs to begin with, this would change
            % its determinism.
            set_cache_maps_snapshot("before then", InitialSnapshot, !Info),
            polymorphism_process_goal(Then0, Then, !Info),
            set_cache_maps_snapshot("before else", InitialSnapshot, !Info),
            polymorphism_process_goal(Else0, Else, !Info),
            set_cache_maps_snapshot("after ite", InitialSnapshot, !Info),
            GoalExpr = if_then_else(Vars, Cond, Then, Else)
        ;
            GoalExpr0 = negation(SubGoal0),
            get_cache_maps_snapshot("neg", InitialSnapshot, !Info),
            polymorphism_process_goal(SubGoal0, SubGoal, !Info),
            set_cache_maps_snapshot("after neg", InitialSnapshot, !Info),
            GoalExpr = negation(SubGoal)
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            get_cache_maps_snapshot("switch", InitialSnapshot, !Info),
            polymorphism_process_cases(Cases0, Cases, InitialSnapshot, !Info),
            set_cache_maps_snapshot("after switch", InitialSnapshot, !Info),
            GoalExpr = switch(Var, CanFail, Cases)
        ;
            GoalExpr0 = scope(Reason0, SubGoal0),
            (
                Reason0 = from_ground_term(TermVar, Kind),
                (
                    Kind = from_ground_term_initial,
                    polymorphism_process_from_ground_term_initial(TermVar,
                        GoalInfo0, SubGoal0, GoalExpr, !Info)
                ;
                    ( Kind = from_ground_term_construct
                    ; Kind = from_ground_term_deconstruct
                    ; Kind = from_ground_term_other
                    ),
                    polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                    GoalExpr = scope(Reason0, SubGoal)
                )
            ;
                Reason0 = promise_solutions(_, _),
                % polymorphism_process_goal may cause SubGoal to bind
                % variables (such as PolyConst variables) that SubGoal0 does
                % not bind. If we allowed such variables to be reused outside
                % the scope, that would change the set of variables that the
                % promise would have to cover. We cannot expect and do not want
                % user level programmers making promises about variables added
                % by the compiler.
                get_cache_maps_snapshot("promise_solns", InitialSnapshot,
                    !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after promise_solns", InitialSnapshot,
                    !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                ( Reason0 = promise_purity(_)
                ; Reason0 = require_detism(_)
                ; Reason0 = require_complete_switch(_)
                ; Reason0 = commit(_)
                ; Reason0 = barrier(_)
                ; Reason0 = loop_control(_, _, _)
                ),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                Reason0 = exist_quant(_),
                % If we allowed a type_info created inside SubGoal to be reused
                % outside GoalExpr, then we are adding an output variable to
                % the scope. If the scope had no outputs to begin with, this
                % would change the determinism of the scope.
                %
                % However, using a type_info from before the scope in SubGoal
                % is perfectly ok.

                get_cache_maps_snapshot("exists", InitialSnapshot, !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after exists", InitialSnapshot,
                    !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                Reason0 = trace_goal(_, _, _, _, _),
                % Trace goal scopes will be deleted after semantic analysis
                % if their compile-time condition turns out to be false.
                % If we let later code use type_infos introduced inside the
                % scope, the deletion of the scope would leave those variables
                % undefined.
                %
                % We *could* evaluate the compile-time condition here to know
                % whether the deletion will happen or not, but doing so would
                % require breaching the separation between compiler passes.

                get_cache_maps_snapshot("trace", InitialSnapshot, !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after trace", InitialSnapshot, !Info),
                GoalExpr = scope(Reason0, SubGoal)
            )
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, Vars,
                MainGoal0, OrElseGoals0, OrElseInners),
            get_cache_maps_snapshot("atomic", InitialSnapshot, !Info),
            polymorphism_process_goal(MainGoal0, MainGoal, !Info),
            polymorphism_process_disj(OrElseGoals0, OrElseGoals,
                InitialSnapshot, !Info),
            set_cache_maps_snapshot("after atomic", InitialSnapshot, !Info),
            ShortHand = atomic_goal(GoalType, Outer, Inner, Vars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            % We don't let the code inside and outside the try goal share
            % type_info variables for the same reason as with lambda
            % expressions; because those pieces of code will end up
            % in different procedures. However, for try goals, this is true
            % even for the first and second conjuncts.
            get_cache_maps_snapshot("try", InitialSnapshot, !Info),
            (
                SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo),
                SubGoalExpr0 = conj(plain_conj, Conjuncts0),
                Conjuncts0 = [ConjunctA0, ConjunctB0]
            ->
                empty_cache_maps(!Info),
                polymorphism_process_goal(ConjunctA0, ConjunctA, !Info),
                empty_cache_maps(!Info),
                polymorphism_process_goal(ConjunctB0, ConjunctB, !Info),

                Conjuncts = [ConjunctA, ConjunctB],
                SubGoalExpr = conj(plain_conj, Conjuncts),
                SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo)
            ;
                unexpected($module, $pred, "malformed try goal")
            ),
            set_cache_maps_snapshot("after try", InitialSnapshot, !Info),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($module, $pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ).

%-----------------------------------------------------------------------------%

:- pred polymorphism_process_from_ground_term_initial(prog_var::in,
    hlds_goal_info::in, hlds_goal::in, hlds_goal_expr::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_from_ground_term_initial(TermVar, GoalInfo0, SubGoal0,
        GoalExpr, !Info) :-
    SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo0),
    ( SubGoalExpr0 = conj(plain_conj, SubGoals0Prime) ->
        SubGoals0 = SubGoals0Prime
    ;
        unexpected($module, $pred,
            "from_ground_term_initial goal is not plain conj")
    ),
    polymorphism_process_fgti_goals(SubGoals0, [], RevMarkedSubGoals,
        fgt_invariants_kept, InvariantsStatus, !Info),
    (
        InvariantsStatus = fgt_invariants_kept,
        Reason = from_ground_term(TermVar, from_ground_term_initial),
        GoalExpr = scope(Reason, SubGoal0)
    ;
        InvariantsStatus = fgt_invariants_broken,
        introduce_partial_fgt_scopes(GoalInfo0, SubGoalInfo0,
            RevMarkedSubGoals, deconstruct_top_down, SubGoal),
        % Delete the scope wrapper around SubGoal0.
        SubGoal = hlds_goal(GoalExpr, _)
    ).

:- pred polymorphism_process_fgti_goals(list(hlds_goal)::in,
    list(fgt_marked_goal)::in, list(fgt_marked_goal)::out,
    fgt_invariants_status::in, fgt_invariants_status::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_fgti_goals([], !RevMarkedGoals, !InvariantsStatus, !Info).
polymorphism_process_fgti_goals([Goal0 | Goals0], !RevMarkedGoals,
        !InvariantsStatus, !Info) :-
    % This is used only if polymorphism_fgt_sanity_tests is enabled.
    OldInfo = !.Info,
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(XVarPrime, Y, ModePrime, UnificationPrime,
            UnifyContextPrime),
        Y = rhs_functor(ConsIdPrime, _, YVarsPrime)
    ->
        XVar = XVarPrime,
        Mode = ModePrime,
        Unification = UnificationPrime,
        UnifyContext = UnifyContextPrime,
        ConsId = ConsIdPrime,
        YVars = YVarsPrime
    ;
        unexpected($module, $pred,
            "from_ground_term_initial conjunct is not functor unify")
    ),
    polymorphism_process_unify_functor(XVar, ConsId, YVars, Mode,
        Unification, UnifyContext, GoalInfo0, Goal, Changed, !Info),
    (
        Changed = no,
        trace [compiletime(flag("polymorphism_fgt_sanity_tests"))] (
            poly_info_get_varset(OldInfo, VarSetBefore),
            MaxVarBefore = varset.max_var(VarSetBefore),
            poly_info_get_num_reuses(OldInfo, NumReusesBefore),

            poly_info_get_varset(!.Info, VarSetAfter),
            MaxVarAfter = varset.max_var(VarSetAfter),
            poly_info_get_num_reuses(!.Info, NumReusesAfter),

            expect(unify(MaxVarBefore, MaxVarAfter), $module, $pred,
                "MaxVarBefore != MaxVarAfter"),
            expect(unify(NumReusesBefore, NumReusesAfter), $module, $pred,
                "NumReusesBefore != NumReusesAfter"),
            expect(unify(Goal0, Goal), $module, $pred,
                "Goal0 != Goal")
        ),
        MarkedGoal = fgt_kept_goal(Goal0, XVar, YVars)
    ;
        Changed = yes,
        MarkedGoal = fgt_broken_goal(Goal, XVar, YVars),
        !:InvariantsStatus = fgt_invariants_broken
    ),
    !:RevMarkedGoals = [MarkedGoal | !.RevMarkedGoals],
    polymorphism_process_fgti_goals(Goals0, !RevMarkedGoals,
        !InvariantsStatus, !Info).

%-----------------------------------------------------------------------------%

:- pred polymorphism_process_unify(prog_var::in, unify_rhs::in,
    unify_mode::in, unification::in, unify_context::in, hlds_goal_info::in,
    hlds_goal::out, poly_info::in, poly_info::out) is det.

polymorphism_process_unify(XVar, Y, Mode, Unification0, UnifyContext,
        GoalInfo0, Goal, !Info) :-
    (
        Y = rhs_var(_YVar),

        % Var-var unifications (simple_test, assign, or complicated_unify)
        % are basically left unchanged. Complicated unifications will
        % eventually get converted into calls, but that is done later on,
        % by simplify.m, not now. At this point we just need to figure out
        % which type_info/typeclass_info variables the unification might need,
        % and insert them in the nonlocals. We have to do that for all var-var
        % unifications, because at this point we haven't done mode analysis so
        % we don't know which ones will become complicated_unifies.
        % Note that we also store the type_info/typeclass_info variables
        % in a field in the unification, which quantification.m uses when
        % requantifying things.

        poly_info_get_var_types(!.Info, VarTypes),
        lookup_var_type(VarTypes, XVar, Type),
        unification_typeinfos(Type, Unification0, Unification,
            GoalInfo0, GoalInfo, _Changed, !Info),
        Goal = hlds_goal(unify(XVar, Y, Mode, Unification, UnifyContext),
            GoalInfo)
    ;
        Y = rhs_functor(ConsId, _, Args),
        polymorphism_process_unify_functor(XVar, ConsId, Args, Mode,
            Unification0, UnifyContext, GoalInfo0, Goal, _Changed, !Info)
    ;
        Y = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            ArgVars0, LambdaVars, Modes, Det, LambdaGoal0),

        % For lambda expressions, we must recursively traverse the lambda goal.
        % Any type_info variables needed by the lambda goal are created in the
        % lambda goal (not imported from the outside), and any type_info
        % variables created by the lambda goal are not available outside.
        % This is because, after lambda expansion, the code inside and outside
        % the lambda goal will end up in different procedures.

        get_cache_maps_snapshot("lambda", InitialSnapshot, !Info),
        empty_cache_maps(!Info),
        polymorphism_process_goal(LambdaGoal0, LambdaGoal1, !Info),
        set_cache_maps_snapshot("after lambda", InitialSnapshot, !Info),

        % Currently we don't allow lambda goals to be existentially typed.
        ExistQVars = [],
        fixup_lambda_quantification(ArgVars0, LambdaVars, ExistQVars,
            LambdaGoal1, LambdaGoal, NonLocalTypeInfos, !Info),
        set_of_var.to_sorted_list(NonLocalTypeInfos, NonLocalTypeInfosList),
        ArgVars = NonLocalTypeInfosList ++ ArgVars0,
        Y1 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            ArgVars, LambdaVars, Modes, Det, LambdaGoal),
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.union(NonLocals0, NonLocalTypeInfos, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),

        % Complicated (in-in) argument unifications are impossible for lambda
        % expressions, so we don't need to worry about adding the type_infos
        % that would be required for such unifications.
        Goal = hlds_goal(unify(XVar, Y1, Mode, Unification0, UnifyContext),
            GoalInfo)
    ).

:- pred unification_typeinfos(mer_type::in,
    unification::in, unification::out, hlds_goal_info::in, hlds_goal_info::out,
    bool::out, poly_info::in, poly_info::out) is det.

unification_typeinfos(Type, !Unification, !GoalInfo, Changed, !Info) :-
    % Compute the type_info/type_class_info variables that would be used
    % if this unification ends up being a complicated_unify.
    type_vars(Type, TypeVars),
    (
        TypeVars = [],
        Changed = no
    ;
        TypeVars = [_ | _],
        list.map_foldl(get_type_info_locn, TypeVars, TypeInfoLocns, !Info),
        add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo),
        Changed = yes
    ).

unification_typeinfos_rtti_varmaps(Type, RttiVarMaps, !Unification,
        !GoalInfo) :-
    % This variant is for use by modecheck_unify.m. During mode checking,
    % all the type_infos should appear in the type_info_varmap.

    % Compute the type_info/type_class_info variables that would be used
    % if this unification ends up being a complicated_unify.
    type_vars(Type, TypeVars),
    list.map(rtti_lookup_type_info_locn(RttiVarMaps), TypeVars,
        TypeInfoLocns),
    add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo).

:- pred add_unification_typeinfos(list(type_info_locn)::in,
    unification::in, unification::out,
    hlds_goal_info::in, hlds_goal_info::out) is det.

add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo) :-
    list.map(type_info_locn_var, TypeInfoLocns, TypeInfoVars0),
    list.remove_dups(TypeInfoVars0, TypeInfoVars),

    % Insert the TypeInfoVars into the nonlocals field of the goal_info
    % for the unification goal.
    NonLocals0 = goal_info_get_nonlocals(!.GoalInfo),
    set_of_var.insert_list(TypeInfoVars, NonLocals0, NonLocals),
    goal_info_set_nonlocals(NonLocals, !GoalInfo),

    % Also save those type_info vars into a field in the complicated_unify,
    % so that quantification.m can recompute variable scopes properly.
    % This field is also used by modecheck_unify.m -- for complicated
    % unifications, it checks that all these variables are ground.
    (
        !.Unification = complicated_unify(Modes, CanFail, _),
        !:Unification = complicated_unify(Modes, CanFail, TypeInfoVars)
    ;
        % This can happen if an earlier stage of compilation has already
        % determined that this unification is particular kind of unification.
        % In that case, the type_info vars won't be needed.
        ( !.Unification = construct(_, _, _, _, _, _, _)
        ; !.Unification = deconstruct(_, _, _, _, _, _)
        ; !.Unification = assign(_, _)
        ; !.Unification = simple_test(_, _)
        )
    ).

:- pred polymorphism_process_unify_functor(prog_var::in, cons_id::in,
    list(prog_var)::in, unify_mode::in, unification::in, unify_context::in,
    hlds_goal_info::in, hlds_goal::out, bool::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_unify_functor(X0, ConsId0, ArgVars0, Mode0, Unification0,
        UnifyContext, GoalInfo0, Goal, Changed, !Info) :-
    poly_info_get_module_info(!.Info, ModuleInfo0),
    poly_info_get_var_types(!.Info, VarTypes0),
    lookup_var_type(VarTypes0, X0, TypeOfX),
    list.length(ArgVars0, Arity),

    % We replace any unifications with higher order pred constants
    % by lambda expressions. For example, we replace
    %
    %   X = list.append(Y)     % Y::in, X::out
    %
    % with
    %
    %   X = (pred(A1::in, A2::out) is ... :- list.append(Y, A1, A2))
    %
    % We do this because it makes two things easier. First, mode analysis
    % needs to check that the lambda goal doesn't bind any nonlocal variables
    % (e.g. `Y' in above example). This would require a bit of moderately
    % tricky special case code if we didn't expand them here. Second, this pass
    % (polymorphism.m) is a lot easier if we don't have to handle higher order
    % constants. If it turns out that the predicate was nonpolymorphic,
    % lambda.m will turn the lambda expression back into a higher order
    % constant again.
    %
    % Note that this transformation is also done by modecheck_unify.m, in case
    % we are rerunning mode analysis after lambda.m has already been run;
    % any changes to the code here will also need to be duplicated there.

    (
        % Check if variable has a higher order type.
        ConsId0 = closure_cons(ShroudedPredProcId, _),
        proc(PredId, ProcId0) = unshroud_pred_proc_id(ShroudedPredProcId),
        type_is_higher_order_details(TypeOfX, Purity, _PredOrFunc, EvalMethod,
            CalleeArgTypes)
    ->
        % An `invalid_proc_id' means the predicate is multi-moded. We can't
        % pick the right mode yet. Perform the rest of the transformation with
        % any mode (the first) but mark the goal with a feature so that mode
        % checking knows to fix it up later.
        ( ProcId0 = invalid_proc_id ->
            module_info_pred_info(ModuleInfo0, PredId, PredInfo),
            ProcIds = pred_info_procids(PredInfo),
            (
                ProcIds = [ProcId | _],
                goal_info_add_feature(feature_lambda_undetermined_mode,
                    GoalInfo0, GoalInfo1)
            ;
                ProcIds = [],
                unexpected($module, $pred, "no modes")
            )
        ;
            ProcId = ProcId0,
            GoalInfo1 = GoalInfo0
        ),
        % Convert the higher order pred term to a lambda goal.
        poly_info_get_varset(!.Info, VarSet0),
        Context = goal_info_get_context(GoalInfo0),
        convert_pred_to_lambda_goal(Purity, EvalMethod, X0, PredId, ProcId,
            ArgVars0, CalleeArgTypes, UnifyContext, GoalInfo1, Context,
            ModuleInfo0, Functor0, VarSet0, VarSet, VarTypes0, VarTypes),
        poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
        % Process the unification in its new form.
        polymorphism_process_unify(X0, Functor0, Mode0, Unification0,
            UnifyContext, GoalInfo1, Goal, !Info),
        Changed = yes
    ;
        % Is this a construction or deconstruction of an existentially
        % typed data type?

        % Check whether the functor had a "new " prefix.
        % If so, assume it is a construction, and strip off the prefix.
        % Otherwise, assume it is a deconstruction.

        ConsId0 = cons(Functor0, Arity, ConsTypeCtor),
        ( remove_new_prefix(Functor0, OrigFunctor) ->
            ConsId = cons(OrigFunctor, Arity, ConsTypeCtor),
            IsConstruction = yes
        ;
            ConsId = ConsId0,
            IsConstruction = no
        ),

        % Check whether the functor (with the "new " prefix removed)
        % is an existentially typed functor.
        type_util.get_existq_cons_defn(ModuleInfo0, TypeOfX, ConsId, ConsDefn)
    ->
        % Add extra arguments to the unification for the
        % type_info and/or type_class_info variables.

        lookup_var_types(VarTypes0, ArgVars0, ActualArgTypes),
        polymorphism_process_existq_unify_functor(ConsDefn,
            IsConstruction, ActualArgTypes, TypeOfX, GoalInfo0,
            ExtraVars, ExtraGoals, !Info),
        ArgVars = ExtraVars ++ ArgVars0,
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(ExtraVars, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),

        % Some of the argument unifications may be complicated unifications,
        % which may need type_infos.
        unification_typeinfos(TypeOfX, Unification0, Unification,
            GoalInfo1, GoalInfo, _Changed, !Info),

        UnifyExpr = unify(X0, rhs_functor(ConsId, IsConstruction, ArgVars),
            Mode0, Unification, UnifyContext),
        Unify = hlds_goal(UnifyExpr, GoalInfo),
        GoalList = ExtraGoals ++ [Unify],
        conj_list_to_goal(GoalList, GoalInfo0, Goal),
        Changed = yes
    ;
        % We leave construction/deconstruction unifications alone.
        % Some of the argument unifications may be complicated unifications,
        % which may need type_infos.

        % XXX Return original Goal0 if Changed = no.
        unification_typeinfos(TypeOfX, Unification0, Unification,
            GoalInfo0, GoalInfo, Changed, !Info),
        GoalExpr = unify(X0, rhs_functor(ConsId0, no, ArgVars0), Mode0,
            Unification, UnifyContext),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

convert_pred_to_lambda_goal(Purity, EvalMethod, X0, PredId, ProcId,
        ArgVars0, PredArgTypes, UnifyContext, GoalInfo0, Context,
        ModuleInfo0, Functor, !VarSet, !VarTypes) :-
    % Create the new lambda-quantified variables.
    create_fresh_vars(PredArgTypes, LambdaVars, !VarSet, !VarTypes),
    Args = ArgVars0 ++ LambdaVars,

    % Build up the hlds_goal_expr for the call that will form the lambda goal.
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
        PredInfo, ProcInfo),

    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    QualifiedPName = qualified(PredModule, PredName),

    % The ConsId's type_ctor shouldn't matter in a call_unify_context.
    ConsId = cons(QualifiedPName, list.length(ArgVars0),
        cons_id_dummy_type_ctor),
    RHS = rhs_functor(ConsId, no, ArgVars0),
    CallUnifyContext = call_unify_context(X0, RHS, UnifyContext),
    LambdaGoalExpr = plain_call(PredId, ProcId, Args, not_builtin,
        yes(CallUnifyContext), QualifiedPName),

    % Construct a goal_info for the lambda goal, making sure to set up
    % the nonlocals field in the goal_info correctly. The goal_id is needed
    % to compute constraint_ids correctly.

    NonLocals = goal_info_get_nonlocals(GoalInfo0),
    set_of_var.insert_list(LambdaVars, NonLocals, OutsideVars),
    set_of_var.list_to_set(Args, InsideVars),
    set_of_var.intersect(OutsideVars, InsideVars, LambdaNonLocals),
    GoalId = goal_info_get_goal_id(GoalInfo0),
    goal_info_init(LambdaGoalInfo0),
    goal_info_set_context(Context, LambdaGoalInfo0, LambdaGoalInfo1),
    goal_info_set_nonlocals(LambdaNonLocals, LambdaGoalInfo1, LambdaGoalInfo2),
    goal_info_set_purity(Purity, LambdaGoalInfo2, LambdaGoalInfo3),
    goal_info_set_goal_id(GoalId, LambdaGoalInfo3, LambdaGoalInfo),
    LambdaGoal = hlds_goal(LambdaGoalExpr, LambdaGoalInfo),

    % Work out the modes of the introduced lambda variables and the determinism
    % of the lambda goal.
    lambda_modes_and_det(ProcInfo, LambdaVars, LambdaModes, LambdaDet),

    % Construct the lambda expression.

    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    % Higher-order values created in this fashion are always ground.
    Groundness = ho_ground,
    Functor = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
        ArgVars0, LambdaVars, LambdaModes, LambdaDet, LambdaGoal).

fix_undetermined_mode_lambda_goal(ProcId, Functor0, Functor, ModuleInfo) :-
    Functor0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
        ArgVars0, LambdaVars, _LambdaModes0, _LambdaDet0, LambdaGoal0),
    LambdaGoal0 = hlds_goal(_, LambdaGoalInfo),
    goal_to_conj_list(LambdaGoal0, LambdaGoalList0),
    (
        list.split_last(LambdaGoalList0, LambdaGoalButLast0, LastGoal0),
        LastGoal0 = hlds_goal(LastGoalExpr0, LastGoalInfo0),
        LastGoalExpr0 = plain_call(PredId0, _DummyProcId, Args0, not_builtin,
            MaybeCallUnifyContext0, QualifiedPName0)
    ->
        LambdaGoalButLast = LambdaGoalButLast0,
        LastGoalInfo = LastGoalInfo0,
        PredId = PredId0,
        Args = Args0,
        MaybeCallUnifyContext = MaybeCallUnifyContext0,
        QualifiedPName = QualifiedPName0
    ;
        unexpected($module, $pred, "unmatched lambda goal")
    ),

    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),

    % Build up the lambda goal.
    LastGoalExpr = plain_call(PredId, ProcId, Args, not_builtin,
        MaybeCallUnifyContext, QualifiedPName),
    LastGoal = hlds_goal(LastGoalExpr, LastGoalInfo),
    conj_list_to_goal(LambdaGoalButLast ++ [LastGoal], LambdaGoalInfo,
        LambdaGoal),

    % Work out the modes of the introduced lambda variables and the determinism
    % of the lambda goal.
    lambda_modes_and_det(ProcInfo, LambdaVars, LambdaModes, LambdaDet),

    % Construct the lambda expression.
    Functor = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
        ArgVars0, LambdaVars, LambdaModes, LambdaDet, LambdaGoal).

:- pred lambda_modes_and_det(proc_info::in, prog_vars::in, list(mer_mode)::out,
    determinism::out) is det.

lambda_modes_and_det(ProcInfo, LambdaVars, LambdaModes, LambdaDet) :-
    proc_info_get_argmodes(ProcInfo, ArgModes),
    list.length(ArgModes, NumArgModes),
    list.length(LambdaVars, NumLambdaVars),
    ( list.drop(NumArgModes - NumLambdaVars, ArgModes, LambdaModesPrime) ->
        LambdaModes = LambdaModesPrime
    ;
        unexpected($module, $pred, "list.drop failed")
    ),
    proc_info_get_declared_determinism(ProcInfo, MaybeDet),
    (
        MaybeDet = yes(Det),
        LambdaDet = Det
    ;
        MaybeDet = no,
        sorry($module, $pred,
            "determinism inference for higher order predicate terms.")
    ).

:- pred create_fresh_vars(list(mer_type)::in, list(prog_var)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

create_fresh_vars([], [], !VarSet, !VarTypes).
create_fresh_vars([Type | Types], [Var | Vars], !VarSet, !VarTypes) :-
    varset.new_var(Var, !VarSet),
    add_var_type(Var, Type, !VarTypes),
    create_fresh_vars(Types, Vars, !VarSet, !VarTypes).

%-----------------------------------------------------------------------------%

    % Compute the extra arguments that we need to add to a unification with
    % an existentially quantified data constructor.
    %
:- pred polymorphism_process_existq_unify_functor(ctor_defn::in, bool::in,
    list(mer_type)::in, mer_type::in, hlds_goal_info::in, list(prog_var)::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism_process_existq_unify_functor(CtorDefn, IsConstruction,
        ActualArgTypes, ActualRetType, GoalInfo,
        ExtraVars, ExtraGoals, !Info) :-
    CtorDefn = ctor_defn(CtorTypeVarSet, CtorExistQVars, CtorKindMap,
        CtorExistentialConstraints, CtorArgTypes, CtorRetType),

    % Rename apart the type variables in the constructor definition.
    poly_info_get_typevarset(!.Info, TypeVarSet0),
    tvarset_merge_renaming(TypeVarSet0, CtorTypeVarSet, TypeVarSet,
        CtorToParentRenaming),
    apply_variable_renaming_to_tvar_list(CtorToParentRenaming,
        CtorExistQVars, ParentExistQVars),
    apply_variable_renaming_to_tvar_kind_map(CtorToParentRenaming,
        CtorKindMap, ParentKindMap),
    apply_variable_renaming_to_prog_constraint_list(CtorToParentRenaming,
        CtorExistentialConstraints, ParentExistentialConstraints),
    apply_variable_renaming_to_type_list(CtorToParentRenaming,
        CtorArgTypes, ParentArgTypes),
    apply_variable_renaming_to_type(CtorToParentRenaming, CtorRetType,
        ParentRetType),
    poly_info_set_typevarset(TypeVarSet, !Info),

    % Compute the type bindings resulting from the functor's actual argument
    % and return types. These are the ones that might bind the ExistQVars.
    type_list_subsumes_det([ParentRetType | ParentArgTypes],
        [ActualRetType | ActualArgTypes], ParentToActualTypeSubst),

    % Create type_class_info variables for the type class constraints.
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    GoalId = goal_info_get_goal_id(GoalInfo),
    list.length(ParentExistentialConstraints, NumExistentialConstraints),
    Context = goal_info_get_context(GoalInfo),
    (
        IsConstruction = yes,
        % Assume it is a construction.
        lookup_hlds_constraint_list(ConstraintMap, unproven, GoalId,
            NumExistentialConstraints, ActualExistentialConstraints),
        make_typeclass_info_vars(ActualExistentialConstraints, [], Context,
            ExtraTypeClassVarsMCAs, ExtraTypeClassGoals, !Info),
        assoc_list.keys(ExtraTypeClassVarsMCAs, ExtraTypeClassVars)
    ;
        IsConstruction = no,
        % Assume it is a deconstruction.
        lookup_hlds_constraint_list(ConstraintMap, assumed, GoalId,
            NumExistentialConstraints, ActualExistentialConstraints),
        make_existq_typeclass_info_vars(ActualExistentialConstraints,
            ExtraTypeClassVars, ExtraTypeClassGoals, !Info)
    ),

    % Compute the set of _unconstrained_ existentially quantified type
    % variables, and then apply the type bindings to those type variables
    % to figure out what types they are bound to.
    constraint_list_get_tvars(ParentExistentialConstraints,
        ParentExistConstrainedTVars),
    list.delete_elems(ParentExistQVars, ParentExistConstrainedTVars,
        ParentUnconstrainedExistQVars),
    apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
        ParentUnconstrainedExistQVars, ActualExistentialTypes),

    % Create type_info variables for the _unconstrained_ existentially
    % quantified type variables.
    polymorphism_do_make_type_info_vars(ActualExistentialTypes, Context,
        ExtraTypeInfoVarsMCAs, ExtraTypeInfoGoals, !Info),
    assoc_list.keys(ExtraTypeInfoVarsMCAs, ExtraTypeInfoVars),

    % The type_class_info variables go AFTER the type_info variables
    % (for consistency with the order for argument passing,
    % and because the RTTI support in the runtime system relies on it)

    ExtraGoals = ExtraTypeInfoGoals ++ ExtraTypeClassGoals,
    ExtraVars = ExtraTypeInfoVars ++ ExtraTypeClassVars.

%-----------------------------------------------------------------------------%

:- pred polymorphism_process_foreign_proc(pred_info::in,
    hlds_goal_expr::in(bound(call_foreign_proc(ground,ground,ground,ground,
    ground,ground,ground))), hlds_goal_info::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_foreign_proc(PredInfo, GoalExpr0, GoalInfo0,
        Goal, !Info) :-
    % Insert the type_info vars into the argname map, so that the foreign_proc
    % can refer to the type_info variable for type T as `TypeInfo_for_T'.
    GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId,
        Args0, ProcExtraArgs, MaybeTraceRuntimeCond, Impl),
    ArgVars0 = list.map(foreign_arg_var, Args0),
    polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
        ExtraVars, ExtraGoals, !Info),
    CanOptAwayUnnamed = yes,
    polymorphism_process_foreign_proc_args(PredInfo, CanOptAwayUnnamed, Impl,
        ExtraVars, ExtraArgs),
    Args = ExtraArgs ++ Args0,

    % Plug it all back together.
    CallExpr = call_foreign_proc(Attributes, PredId, ProcId,
        Args, ProcExtraArgs, MaybeTraceRuntimeCond, Impl),
    Call = hlds_goal(CallExpr, GoalInfo),
    GoalList = ExtraGoals ++ [Call],
    conj_list_to_goal(GoalList, GoalInfo0, Goal).

:- pred polymorphism_process_foreign_proc_args(pred_info::in, bool::in,
    pragma_foreign_code_impl::in, list(prog_var)::in, list(foreign_arg)::out)
    is det.

polymorphism_process_foreign_proc_args(PredInfo, CanOptAwayUnnamed, Impl, Vars,
        Args) :-
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, ExistQVars,
        PredArgTypes),

    % Find out which variables are constrained (so that we don't add
    % type_infos for them).
    pred_info_get_class_context(PredInfo, constraints(UnivCs, ExistCs)),
    UnivVars0 = list.map(get_constrained_vars, UnivCs),
    list.condense(UnivVars0, UnivConstrainedVars),
    ExistVars0 = list.map(get_constrained_vars, ExistCs),
    list.condense(ExistVars0, ExistConstrainedVars),

    type_vars_list(PredArgTypes, PredTypeVars0),
    list.remove_dups(PredTypeVars0, PredTypeVars1),
    list.delete_elems(PredTypeVars1, UnivConstrainedVars, PredTypeVars2),
    list.delete_elems(PredTypeVars2, ExistConstrainedVars, PredTypeVars),

    % The argument order is described at the top of this file.

    in_mode(In),
    out_mode(Out),

    list.map(foreign_proc_add_typeclass_info(CanOptAwayUnnamed, Out, Impl,
        PredTypeVarSet), ExistCs, ExistTypeClassArgInfos),
    list.map(foreign_proc_add_typeclass_info(CanOptAwayUnnamed, In, Impl,
        PredTypeVarSet), UnivCs, UnivTypeClassArgInfos),
    TypeClassArgInfos = UnivTypeClassArgInfos ++ ExistTypeClassArgInfos,

    list.filter((pred(X::in) is semidet :- list.member(X, ExistQVars)),
        PredTypeVars, ExistUnconstrainedVars, UnivUnconstrainedVars),

    list.map(foreign_proc_add_typeinfo(CanOptAwayUnnamed, Out, Impl,
        PredTypeVarSet), ExistUnconstrainedVars, ExistTypeArgInfos),
    list.map(foreign_proc_add_typeinfo(CanOptAwayUnnamed, In, Impl,
        PredTypeVarSet), UnivUnconstrainedVars, UnivTypeArgInfos),
    TypeInfoArgInfos = UnivTypeArgInfos ++ ExistTypeArgInfos,

    ArgInfos = TypeInfoArgInfos ++ TypeClassArgInfos,

    % Insert type_info/typeclass_info types for all the inserted
    % type_info/typeclass_info vars into the argument type list.

    TypeInfoTypes = list.map((func(_) = type_info_type), PredTypeVars),
    list.map(build_typeclass_info_type, UnivCs, UnivTypes),
    list.map(build_typeclass_info_type, ExistCs, ExistTypes),
    OrigArgTypes = TypeInfoTypes ++ UnivTypes ++ ExistTypes,

    make_foreign_args(Vars, ArgInfos, OrigArgTypes, Args).

:- pred foreign_proc_add_typeclass_info(bool::in, mer_mode::in,
    pragma_foreign_code_impl::in, tvarset::in, prog_constraint::in,
    pair(maybe(pair(string, mer_mode)), box_policy)::out) is det.

foreign_proc_add_typeclass_info(CanOptAwayUnnamed, Mode, Impl, TypeVarSet,
        Constraint, MaybeArgName - native_if_possible) :-
    Constraint = constraint(SymName, Types),
    Name = sym_name_to_string_sep(SymName, "__"),
    type_vars_list(Types, TypeVars),
    TypeVarNames = list.map(underscore_and_tvar_name(TypeVarSet), TypeVars),
    string.append_list(["TypeClassInfo_for_", Name | TypeVarNames],
        ConstraintVarName),
    % If the variable name corresponding to the typeclass_info isn't mentioned
    % in the C code fragment, don't pass the variable to the C code at all.
    (
        CanOptAwayUnnamed = yes,
        foreign_code_does_not_use_variable(Impl, ConstraintVarName)
    ->
        MaybeArgName = no
    ;
        MaybeArgName = yes(ConstraintVarName - Mode)
    ).

:- pred foreign_proc_add_typeinfo(bool::in, mer_mode::in,
    pragma_foreign_code_impl::in, tvarset::in, tvar::in,
    pair(maybe(pair(string, mer_mode)), box_policy)::out) is det.

foreign_proc_add_typeinfo(CanOptAwayUnnamed, Mode, Impl, TypeVarSet, TVar,
        MaybeArgName - native_if_possible) :-
    ( varset.search_name(TypeVarSet, TVar, TypeVarName) ->
        C_VarName = "TypeInfo_for_" ++ TypeVarName,
        % If the variable name corresponding to the type_info isn't mentioned
        % in the C code fragment, don't pass the variable to the C code at all.
        (
            CanOptAwayUnnamed = yes,
            foreign_code_does_not_use_variable(Impl, C_VarName)
        ->
            MaybeArgName = no
        ;
            MaybeArgName = yes(C_VarName - Mode)
        )
    ;
        MaybeArgName = no
    ).

:- pred foreign_code_does_not_use_variable(pragma_foreign_code_impl::in,
    string::in) is semidet.

foreign_code_does_not_use_variable(Impl, VarName) :-
    % XXX This test used to be turned off with the semidet_fail, as it caused
    % the compiler to abort when compiling declarative_execution.m in stage2,
    % but this is no longer the case.
    % semidet_fail,
    \+ foreign_code_uses_variable(Impl, VarName).

:- func underscore_and_tvar_name(tvarset, tvar) = string.

underscore_and_tvar_name(TypeVarSet, TVar) = TVarName :-
    varset.lookup_name(TypeVarSet, TVar, TVarName0),
    TVarName = "_" ++ TVarName0.

:- pred polymorphism_process_plain_conj(list(hlds_goal)::in,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism_process_plain_conj([], [], !Info).
polymorphism_process_plain_conj([Goal0 | Goals0], [Goal | Goals], !Info) :-
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_plain_conj(Goals0, Goals, !Info).

:- pred polymorphism_process_par_conj(list(hlds_goal)::in,
    list(hlds_goal)::out, cache_maps::in, poly_info::in, poly_info::out)
    is det.

polymorphism_process_par_conj([], [], _, !Info).
polymorphism_process_par_conj([Goal0 | Goals0], [Goal | Goals],
        InitialSnapshot, !Info) :-
    % Any variable that a later parallel conjunct reuses from an earlier
    % parallel conjunct (a) will definitely require synchronization, whose
    % cost will be greater than the cost of building a typeinfo from scratch,
    % and (b) may drastically reduce the available parallelism, if the earlier
    % conjunct produces the variable late but the later conjunct requires it
    % early.
    set_cache_maps_snapshot("par conjunct", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_par_conj(Goals0, Goals, InitialSnapshot, !Info).

:- pred polymorphism_process_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    cache_maps::in, poly_info::in, poly_info::out) is det.

polymorphism_process_disj([], [], _, !Info).
polymorphism_process_disj([Goal0 | Goals0], [Goal | Goals], InitialSnapshot,
        !Info) :-
    set_cache_maps_snapshot("disjunct", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_disj(Goals0, Goals, InitialSnapshot, !Info).

:- pred polymorphism_process_cases(list(case)::in, list(case)::out,
    cache_maps::in, poly_info::in, poly_info::out) is det.

polymorphism_process_cases([], [], _, !Info).
polymorphism_process_cases([Case0 | Cases0], [Case | Cases], InitialSnapshot,
        !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    set_cache_maps_snapshot("case", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    polymorphism_process_cases(Cases0, Cases, InitialSnapshot, !Info).

%-----------------------------------------------------------------------------%

    % XXX document me
    %
    % XXX the following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred polymorphism_process_call(pred_id::in, list(prog_var)::in,
    hlds_goal_info::in, hlds_goal_info::out,
    list(prog_var)::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
        ExtraVars, ExtraGoals, !Info) :-
    poly_info_get_var_types(!.Info, VarTypes),
    poly_info_get_typevarset(!.Info, TypeVarSet0),
    poly_info_get_module_info(!.Info, ModuleInfo),

    % The order of the added variables is important, and must match the
    % order specified at the top of this file.

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_tvar_kinds(PredInfo, PredKindMap),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % VarTypes, TypeVarSet* etc come from the caller.
    % PredTypeVarSet, PredArgTypes, PredExistQVars, etc come
    % directly from the callee.
    % ParentArgTypes, ParentExistQVars etc come from a version
    % of the callee that has been renamed apart from the caller.
    %
    % The difference between e.g. PredArgTypes and ParentArgTypes is the
    % application of PredToParentTypeRenaming, which maps the type variables
    % in the callee to new type variables in the caller. Adding the new type
    % variables to TypeVarSet0 yields TypeVarSet.

    ( varset.is_empty(PredTypeVarSet) ->
        % Optimize a common case.
        map.init(PredToParentTypeRenaming),
        TypeVarSet = TypeVarSet0,
        ParentArgTypes = PredArgTypes,
        ParentKindMap = PredKindMap,
        ParentTVars = [],
        ParentExistQVars = []
    ;
        % This merge might be a performance bottleneck?
        tvarset_merge_renaming(TypeVarSet0, PredTypeVarSet, TypeVarSet,
            PredToParentTypeRenaming),
        apply_variable_renaming_to_type_list(PredToParentTypeRenaming,
            PredArgTypes, ParentArgTypes),
        type_vars_list(ParentArgTypes, ParentTVars),
        apply_variable_renaming_to_tvar_kind_map(PredToParentTypeRenaming,
            PredKindMap, ParentKindMap),
        apply_variable_renaming_to_tvar_list(PredToParentTypeRenaming,
            PredExistQVars, ParentExistQVars)
    ),

    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    (
        (
            % Optimize for the common case of nonpolymorphic call
            % with no constraints.
            ParentTVars = [],
            PredClassContext = constraints([], [])
        ;
            % Some builtins don't need or want the type_info.
            no_type_info_builtin(PredModule, PredName, PredArity)
        )
    ->
        GoalInfo = GoalInfo0,
        ExtraGoals = [],
        ExtraVars = []
    ;
        poly_info_set_typevarset(TypeVarSet, !Info),

        % Compute which "parent" type variables are constrained
        % by the type class constraints.
        apply_variable_renaming_to_prog_constraints(PredToParentTypeRenaming,
            PredClassContext, ParentClassContext),
        ParentClassContext = constraints(ParentUnivConstraints,
            ParentExistConstraints),
        constraint_list_get_tvars(ParentUnivConstraints,
            ParentUnivConstrainedTVars),
        constraint_list_get_tvars(ParentExistConstraints,
            ParentExistConstrainedTVars),

        % Calculate the set of unconstrained type vars. Split these into
        % existential and universal type vars.
        list.remove_dups(ParentTVars, ParentUnconstrainedTVars0),
        list.delete_elems(ParentUnconstrainedTVars0,
            ParentUnivConstrainedTVars, ParentUnconstrainedTVars1),
        list.delete_elems(ParentUnconstrainedTVars1,
            ParentExistConstrainedTVars, ParentUnconstrainedTVars),
        list.delete_elems(ParentUnconstrainedTVars, ParentExistQVars,
            ParentUnconstrainedUnivTVars),
        list.delete_elems(ParentUnconstrainedTVars,
            ParentUnconstrainedUnivTVars, ParentUnconstrainedExistTVars),

        % Calculate the "parent to actual" binding.
        lookup_var_types(VarTypes, ArgVars0, ActualArgTypes),
        type_list_subsumes_det(ParentArgTypes, ActualArgTypes,
            ParentToActualTypeSubst),

        % Make the universally quantified typeclass_infos for the call.
        poly_info_get_constraint_map(!.Info, ConstraintMap),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        list.length(ParentUnivConstraints, NumUnivConstraints),
        lookup_hlds_constraint_list(ConstraintMap, unproven, GoalId,
            NumUnivConstraints, ActualUnivConstraints),
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentExistQVars, ActualExistQVarTypes),
        (
            prog_type.type_list_to_var_list(ActualExistQVarTypes,
                ActualExistQVars0)
        ->
            ActualExistQVars = ActualExistQVars0
        ;
            unexpected($module, $pred, "existq_tvar bound")
        ),
        Context = goal_info_get_context(GoalInfo0),
        make_typeclass_info_vars(ActualUnivConstraints, ActualExistQVars,
            Context, ExtraUnivClassVarsMCAs, ExtraUnivClassGoals,
            !Info),
        assoc_list.keys(ExtraUnivClassVarsMCAs, ExtraUnivClassVars),

        % Make variables to hold any existentially quantified typeclass_infos
        % in the call, insert them into the typeclass_info map.
        list.length(ParentExistConstraints, NumExistConstraints),
        lookup_hlds_constraint_list(ConstraintMap, assumed, GoalId,
            NumExistConstraints, ActualExistConstraints),
        make_existq_typeclass_info_vars(ActualExistConstraints,
            ExtraExistClassVars, ExtraExistClassGoals, !Info),

        % Make variables to hold typeinfos for unconstrained universal type
        % vars.
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentUnconstrainedUnivTVars, ActualUnconstrainedUnivTypes),
        polymorphism_do_make_type_info_vars(ActualUnconstrainedUnivTypes,
            Context, ExtraUnivTypeInfoVarsMCAs,
            ExtraUnivTypeInfoGoals, !Info),
        assoc_list.keys(ExtraUnivTypeInfoVarsMCAs,
            ExtraUnivTypeInfoVars),

        % Make variables to hold typeinfos for unconstrained existential type
        % vars.
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentUnconstrainedExistTVars, ActualUnconstrainedExistTypes),
        polymorphism_do_make_type_info_vars(ActualUnconstrainedExistTypes,
            Context, ExtraExistTypeInfoVarsMCAs,
            ExtraExistTypeInfoGoals, !Info),
        assoc_list.keys(ExtraExistTypeInfoVarsMCAs,
            ExtraExistTypeInfoVars),

        % Add up the extra vars and goals.
        ExtraGoals = ExtraUnivClassGoals ++ ExtraExistClassGoals
            ++ ExtraUnivTypeInfoGoals ++ ExtraExistTypeInfoGoals,
        ExtraVars = ExtraUnivTypeInfoVars ++ ExtraExistTypeInfoVars
            ++ ExtraUnivClassVars ++ ExtraExistClassVars,

        % Update the nonlocals.
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(ExtraVars, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo)
    ).

%-----------------------------------------------------------------------------%

polymorphism_process_new_call(CalleePredInfo, CalleeProcInfo, PredId, ProcId,
        CallArgs0, BuiltinState, MaybeCallUnifyContext, SymName,
        GoalInfo0, Goal, !Info) :-
    % document me better
    %
    poly_info_get_typevarset(!.Info, TVarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    lookup_var_types(VarTypes0, CallArgs0, ActualArgTypes0),
    pred_info_get_arg_types(CalleePredInfo, PredTVarSet, _PredExistQVars,
        PredArgTypes),
    proc_info_get_headvars(CalleeProcInfo, CalleeHeadVars),
    proc_info_get_rtti_varmaps(CalleeProcInfo, CalleeRttiVarMaps),

    % Work out how many type_info args we need to prepend.
    NCallArgs0 = list.length(ActualArgTypes0),
    NPredArgs  = list.length(PredArgTypes),
    NExtraArgs = NPredArgs - NCallArgs0,
    (
        list.drop(NExtraArgs, PredArgTypes, OrigPredArgTypes0),
        list.take(NExtraArgs, CalleeHeadVars, CalleeExtraHeadVars0)
    ->
        OrigPredArgTypes = OrigPredArgTypes0,
        CalleeExtraHeadVars = CalleeExtraHeadVars0
    ;
        unexpected($module, $pred, "extra args not found")
    ),

    % Work out the bindings of type variables in the call.
    tvarset_merge_renaming(TVarSet0, PredTVarSet, TVarSet,
        PredToParentRenaming),
    apply_variable_renaming_to_type_list(PredToParentRenaming,
        OrigPredArgTypes, OrigParentArgTypes),
    type_list_subsumes_det(OrigParentArgTypes, ActualArgTypes0,
        ParentToActualTSubst),
    poly_info_set_typevarset(TVarSet, !Info),

    % Look up the type variables that the type_infos in the caller are for,
    % and apply the type bindings to calculate the types that the caller
    % should pass type_infos for.
    GetTypeInfoTypes = (pred(ProgVar::in, TypeInfoType::out) is det :-
        rtti_varmaps_var_info(CalleeRttiVarMaps, ProgVar, VarInfo),
        (
            VarInfo = type_info_var(TypeInfoType)
        ;
            VarInfo = typeclass_info_var(_),
            unexpected($module, $pred,
                "unsupported: constraints on initialisation preds")
        ;
            VarInfo = non_rtti_var,
            unexpected($module, $pred,
                "missing rtti_var_info for initialisation pred")
        )
    ),
    list.map(GetTypeInfoTypes, CalleeExtraHeadVars, PredTypeInfoTypes),
    apply_variable_renaming_to_type_list(PredToParentRenaming,
        PredTypeInfoTypes, ParentTypeInfoTypes),
    apply_rec_subst_to_type_list(ParentToActualTSubst, ParentTypeInfoTypes,
        ActualTypeInfoTypes),

    % Construct goals to make the required type_infos.
    Ctxt = term.context_init,
    polymorphism_do_make_type_info_vars(ActualTypeInfoTypes, Ctxt,
        ExtraArgsConstArgs, ExtraGoals, !Info),
    assoc_list.keys(ExtraArgsConstArgs, ExtraArgs),
    CallArgs = ExtraArgs ++ CallArgs0,
    NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
    NonLocals1 = set_of_var.list_to_set(ExtraArgs),
    set_of_var.union(NonLocals0, NonLocals1, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    CallGoalExpr = plain_call(PredId, ProcId, CallArgs, BuiltinState,
        MaybeCallUnifyContext, SymName),
    CallGoal = hlds_goal(CallGoalExpr, GoalInfo),
    conj_list_to_goal(ExtraGoals ++ [CallGoal], GoalInfo, Goal).

%-----------------------------------------------------------------------------%

    % If the pred we are processing is a polymorphic predicate, or contains
    % polymorphically-typed goals, we may need to fix up the quantification
    % (nonlocal variables) of the goal so that it includes the extra type_info
    % variables and typeclass_info variables that we added to the headvars
    % or the arguments of existentially typed predicate calls, function calls
    % and deconstruction unifications.
    %
    % Type(class)-infos added for ground types passed to predicate calls,
    % function calls and existentially typed construction unifications
    % do not require requantification because they are local to the conjunction
    % containing the type(class)-info construction and the goal which uses the
    % type(class)-info. The nonlocals for those goals are adjusted by
    % the code which creates/alters them. However, reusing a type_info changes
    % it from being local to nonlocal.
    %
:- pred fixup_quantification(proc_arg_vector(prog_var)::in,
    existq_tvars::in, hlds_goal::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

fixup_quantification(HeadVars, ExistQVars, Goal0, Goal, !Info) :-
    (
        % Optimize common case.
        ExistQVars = [],
        rtti_varmaps_no_tvars(!.Info ^ poly_rtti_varmaps),
        poly_info_get_num_reuses(!.Info, NumReuses),
        NumReuses = 0
    ->
        Goal = Goal0
    ;
        poly_info_get_varset(!.Info, VarSet0),
        poly_info_get_var_types(!.Info, VarTypes0),
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        OutsideVars = proc_arg_vector_to_set(HeadVars),
        implicitly_quantify_goal_general(ordinary_nonlocals_maybe_lambda,
            set_to_bitset(OutsideVars), _Warnings, Goal0, Goal,
            VarSet0, VarSet, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
        poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
        poly_info_set_rtti_varmaps(RttiVarMaps, !Info)
    ).

    % If the lambda goal we are processing is polymorphically typed, we may
    % need to fix up the quantification (nonlocal variables) so that it
    % includes the type_info variables and typeclass_info variables for
    % any polymorphically typed variables in the nonlocals set or in the
    % arguments (either the lambda vars or the implicit curried argument
    % variables). Including typeinfos for arguments which are not in the
    % nonlocals set of the goal, i.e. unused arguments, is necessary only
    % if typeinfo_liveness is set, but we do it always, since we don't have
    % the options available here, and the since cost is pretty minimal.
    %
:- pred fixup_lambda_quantification(list(prog_var)::in,
    list(prog_var)::in, existq_tvars::in, hlds_goal::in, hlds_goal::out,
    set_of_progvar::out, poly_info::in, poly_info::out) is det.

fixup_lambda_quantification(ArgVars, LambdaVars, ExistQVars, !Goal,
        NewOutsideVars, !Info) :-
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    ( rtti_varmaps_no_tvars(RttiVarMaps0) ->
        set_of_var.init(NewOutsideVars)
    ;
        poly_info_get_varset(!.Info, VarSet0),
        poly_info_get_var_types(!.Info, VarTypes0),
        !.Goal = hlds_goal(_, GoalInfo0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(ArgVars, NonLocals, NonLocalsPlusArgs0),
        set_of_var.insert_list(LambdaVars,
            NonLocalsPlusArgs0, NonLocalsPlusArgs),
        goal_util.extra_nonlocal_typeinfos(RttiVarMaps0, VarTypes0,
            ExistQVars, NonLocalsPlusArgs, NewOutsideVars),
        set_of_var.union(NonLocals, NewOutsideVars, OutsideVars),
        implicitly_quantify_goal_general(ordinary_nonlocals_maybe_lambda,
            OutsideVars, _Warnings, !Goal,
            VarSet0, VarSet, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
        poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
        poly_info_set_rtti_varmaps(RttiVarMaps, !Info)
    ).

%-----------------------------------------------------------------------------%

    % Given the list of constraints for a called predicate, create a list of
    % variables to hold the typeclass_info for those constraints, and create
    % a list of goals to initialize those typeclass_info variables to the
    % appropriate typeclass_info structures for the constraints.
    %
    % Constraints should be renamed-apart and actual-to-formal substituted
    % constraints. Constraints which are already in the rtti_varmaps are
    % assumed to have already had their typeclass_infos initialized; for them,
    % we just return the variable in the rtti_varmaps.
    %
:- pred make_typeclass_info_vars(list(prog_constraint)::in,
    existq_tvars::in, prog_context::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_vars(Constraints, ExistQVars, Context,
        TypeClassInfoVarsMCAs, ExtraGoals, !Info) :-
    SeenInstances = [],
    make_typeclass_info_vars_2(Constraints, SeenInstances, ExistQVars, Context,
        TypeClassInfoVarsMCAs, ExtraGoals, !Info).

    % Accumulator version of the above.
    %
:- pred make_typeclass_info_vars_2(list(prog_constraint)::in,
    list(prog_constraint)::in, existq_tvars::in, prog_context::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_vars_2([], _Seen, _ExistQVars, _Context,
        [], [], !Info).
make_typeclass_info_vars_2([Constraint | Constraints], Seen, ExistQVars,
        Context, [TypeClassInfoVarMCA | TypeClassInfoVarsMCAs],
        ExtraGoals, !Info) :-
    make_typeclass_info_var(Constraint, [Constraint | Seen],
        ExistQVars, Context, TypeClassInfoVarMCA, HeadExtraGoals, !Info),
    make_typeclass_info_vars_2(Constraints, Seen, ExistQVars,
        Context, TypeClassInfoVarsMCAs, TailExtraGoals, !Info),
    ExtraGoals = HeadExtraGoals ++ TailExtraGoals.

:- pred make_typeclass_info_var(prog_constraint::in,
    list(prog_constraint)::in, existq_tvars::in, prog_context::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_var(Constraint, Seen, ExistQVars, Context,
        TypeClassInfoVarMCA, Goals, !Info) :-
    (
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_search_typeclass_info_var(RttiVarMaps0, Constraint,
            OldTypeClassInfoVar)
    ->
        % We already have a typeclass_info for this constraint, either from
        % a parameter to the pred or from an existentially quantified goal
        % that we have already processed.
        TypeClassInfoVar = OldTypeClassInfoVar,
        TypeClassInfoVarMCA = TypeClassInfoVar - no,
        Goals = []
    ;
        % We don't have the typeclass_info, so we must either have a proof
        % that tells us how to make it, or ...
        map.search(!.Info ^ poly_proof_map, Constraint, Proof)
    ->
        make_typeclass_info_from_proof(Constraint, Seen, Proof, ExistQVars,
            Context, TypeClassInfoVarMCA, Goals, !Info)
    ;
        % ... it will be produced by an existentially typed goal that
        % we will process later on.
        make_typeclass_info_head_var(do_record_type_info_locns, Constraint,
            TypeClassInfoVar, !Info),
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_reuse_typeclass_info_var(TypeClassInfoVar,
            RttiVarMaps0, RttiVarMaps),
        poly_info_set_rtti_varmaps(RttiVarMaps, !Info),
        TypeClassInfoVarMCA = TypeClassInfoVar - no,
        Goals = []
    ).

:- pred make_typeclass_info_from_proof(prog_constraint::in,
    list(prog_constraint)::in, constraint_proof::in, existq_tvars::in,
    prog_context::in, pair(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_typeclass_info_from_proof(Constraint, Seen, Proof,
        ExistQVars, Context, TypeClassInfoVarMCA, Goals, !Info) :-
    (
        % XXX MR_Dictionary should have MR_Dictionaries for superclass
        % We have to extract the typeclass_info from another one.
        Proof = superclass(SubClassConstraint),
        make_typeclass_info_from_subclass(Constraint, Seen, SubClassConstraint,
            ExistQVars, Context, TypeClassInfoVarMCA, Goals, !Info)
    ;
        % We have to construct the typeclass_info using an instance
        % declaration.
        Proof = apply_instance(InstanceNum),
        make_typeclass_info_from_instance(Constraint, Seen, InstanceNum,
            ExistQVars, Context, TypeClassInfoVarMCA, Goals, !Info)
    ).

:- pred make_typeclass_info_from_subclass(prog_constraint::in,
    list(prog_constraint)::in, prog_constraint::in, existq_tvars::in,
    prog_context::in, pair(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_typeclass_info_from_subclass(Constraint, Seen, SubClassConstraint,
        ExistQVars, Context, TypeClassInfoVar - MaybeTCIConstArg, Goals,
        !Info) :-
    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        some [SelectedPred, Level, IndentStr] (
            promise_pure (
                semipure get_selected_pred(SelectedPred),
                semipure get_level(Level),
                (
                    SelectedPred = no
                ;
                    SelectedPred = yes,
                    IndentStr = string.duplicate_char(' ', Level * 4),
                    impure set_level(Level + 1),

                    io.write_string(IndentStr, !IO),
                    io.write_string("make_typeclass_info_from_subclass", !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("Constraint: ", !IO),
                    io.write(Constraint, !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("Seen: ", !IO),
                    ( Seen = [Constraint] ->
                        io.write_string("[Constraint]\n", !IO)
                    ;
                        io.write(Seen, !IO),
                        io.nl(!IO)
                    ),
                    io.write_string(IndentStr, !IO),
                    io.write_string("SubClassConstraint: ", !IO),
                    io.write(SubClassConstraint, !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("ExistQVars: ", !IO),
                    io.write(ExistQVars, !IO),
                    io.nl(!IO),
                    io.nl(!IO)
                )
            )
        )
    ),

    % Work out where to extract the typeclass info from.
    SubClassConstraint = constraint(SubClassName, SubClassTypes),
    list.length(SubClassTypes, SubClassArity),
    SubClassId = class_id(SubClassName, SubClassArity),

    % Make the typeclass_info for the subclass.
    make_typeclass_info_var(SubClassConstraint, Seen, ExistQVars, Context,
        SubClassVarMCA, SubClassVarGoals, !Info),
    SubClassVarMCA = SubClassVar - SubClassMCA,

    % Look up the definition of the subclass.
    poly_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(ClassTable, SubClassId, SubClassDefn),

    % Work out which superclass typeclass_info to take.
    map.from_corresponding_lists(SubClassDefn ^ class_vars, SubClassTypes,
        SubTypeSubst),
    apply_subst_to_prog_constraint_list(SubTypeSubst,
        SubClassDefn ^ class_supers, SuperClasses),
    ( list.nth_member_search(SuperClasses, Constraint, SuperClassIndex0) ->
        SuperClassIndex0 = SuperClassIndex
    ;
        % We shouldn't have got this far if the constraints were not satisfied.
        unexpected($module, $pred, "constraint not in constraint list")
    ),

    (
        SubClassMCA = yes(SubClassConstArg),
        (
            SubClassConstArg = csa_constant(_, _),
            unexpected($module, $pred, "typeclass infos need a cell")
        ;
            SubClassConstArg = csa_const_struct(SubClassConstNum),
            poly_info_get_const_struct_db(!.Info, ConstStructDb),
            lookup_const_struct_num(ConstStructDb, SubClassConstNum,
                SubClassConstStruct),
            SubClassConstStruct = const_struct(SubClassConsId, SubClassArgs,
                _, _),
            (
                SubClassConsId = typeclass_info_cell_constructor,
                SubClassArgs = [BTCIArg | OtherArgs],
                BTCIArg = csa_constant(BTCIConsId, _),
                BTCIConsId = base_typeclass_info_const(_, SubClassId,
                    SubInstanceNum, _),
                module_info_get_instance_table(ModuleInfo, InstanceTable),
                map.lookup(InstanceTable, SubClassId, SubInstanceDefns),
                list.index1(SubInstanceDefns, SubInstanceNum, SubInstanceDefn),
                num_extra_instance_args(SubInstanceDefn, NumExtra),
                Index = NumExtra + SuperClassIndex,
                list.det_index1(OtherArgs, Index, SelectedArg),
                SelectedArg = csa_const_struct(SelectedConstNum)
            ->
                materialize_typeclass_info_var(Constraint, SelectedConstNum,
                    TypeClassInfoVar, Goals, !Info),
                MaybeTCIConstArg = yes(SelectedArg)
            ;
                unexpected($module, $pred,
                    "unexpected typeclass info structure")
            )
        ),

        trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
            some [SelectedPred, Level, IndentStr, ResultStr] (
                promise_pure (
                    semipure get_selected_pred(SelectedPred),
                    semipure get_level(Level),
                    impure set_level(Level - 1),
                    (
                        SelectedPred = no
                    ;
                        SelectedPred = yes,
                        IndentStr = string.duplicate_char(' ', (Level-1) * 4),
                        io.write_string(IndentStr, !IO),
                        io.write_string("subclass constant result ", !IO),
                        io.write(TypeClassInfoVar - MaybeTCIConstArg, !IO),
                        io.nl(!IO),
                        io.nl(!IO)
                    )
                )
            )
        )
    ;
        SubClassMCA = no,
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, _TypeClassInfoVarType, !Info),
        get_poly_const(SuperClassIndex, IndexVar, IndexGoals, !Info),

        % We extract the superclass typeclass_info by inserting a call
        % to superclass_from_typeclass_info in private_builtin.
        goal_util.generate_simple_call(mercury_private_builtin_module,
            "superclass_from_typeclass_info",
            pf_predicate, only_mode, detism_det, purity_pure,
            [SubClassVar, IndexVar, TypeClassInfoVar], [],
            instmap_delta_bind_no_var, ModuleInfo, term.context_init,
            SuperClassGoal),
        Goals = SubClassVarGoals ++ IndexGoals ++ [SuperClassGoal],
        MaybeTCIConstArg = no,

        trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
            some [SelectedPred, Level, IndentStr, ResultStr] (
                promise_pure (
                    semipure get_selected_pred(SelectedPred),
                    semipure get_level(Level),
                    impure set_level(Level - 1),
                    (
                        SelectedPred = no
                    ;
                        SelectedPred = yes,
                        IndentStr = string.duplicate_char(' ', (Level-1) * 4),
                        io.write_string(IndentStr, !IO),
                        io.write_string("subclass computed result ", !IO),
                        io.write(TypeClassInfoVar - MaybeTCIConstArg, !IO),
                        io.nl(!IO),
                        io.nl(!IO)
                    )
                )
            )
        )
    ).

:- pred make_typeclass_info_from_instance(prog_constraint::in,
    list(prog_constraint)::in, int::in, existq_tvars::in, prog_context::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_from_instance(Constraint, Seen, InstanceNum, ExistQVars,
        Context, TypeClassInfoVarMCA, Goals, !Info) :-
    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        some [SelectedPred, Level, IndentStr] (
            promise_pure (
                semipure get_selected_pred(SelectedPred),
                semipure get_level(Level),
                (
                    SelectedPred = no
                ;
                    SelectedPred = yes,
                    IndentStr = string.duplicate_char(' ', Level * 4),
                    impure set_level(Level + 1),

                    io.write_string(IndentStr, !IO),
                    io.write_string("make_typeclass_info_from_instance", !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("Constraint: ", !IO),
                    io.write(Constraint, !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("Seen: ", !IO),
                    ( Seen = [Constraint] ->
                        io.write_string("[Constraint]\n", !IO)
                    ;
                        io.write(Seen, !IO),
                        io.nl(!IO)
                    ),
                    io.write_string(IndentStr, !IO),
                    io.write_string("InstanceNum: ", !IO),
                    io.write(InstanceNum, !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("ExistQVars: ", !IO),
                    io.write(ExistQVars, !IO),
                    io.nl(!IO),
                    io.nl(!IO)
                )
            )
        )
    ),

    poly_info_get_const_struct_db(!.Info, ConstStructDb0),
    InstanceId = ciid(InstanceNum, Constraint, Seen),
    (
        ExistQVars = [],
        search_for_constant_instance(ConstStructDb0, InstanceId,
            InstanceIdConstNum)
    ->
        materialize_typeclass_info_var(Constraint, InstanceIdConstNum,
            TypeClassInfoVar, Goals, !Info),
        TypeClassInfoVarMCA =
            TypeClassInfoVar - yes(csa_const_struct(InstanceIdConstNum)),

        trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
            some [SelectedPred, Level, IndentStr, ResultStr] (
                promise_pure (
                    semipure get_selected_pred(SelectedPred),
                    semipure get_level(Level),
                    impure set_level(Level - 1),
                    (
                        SelectedPred = no
                    ;
                        SelectedPred = yes,
                        IndentStr = string.duplicate_char(' ', (Level-1) * 4),
                        (
                            Goals = [],
                            ResultStr = "instance doubly cached result "
                        ;
                            Goals = [_ | _],
                            ResultStr = "instance cached result "
                        ),

                        io.write_string(IndentStr, !IO),
                        io.write_string(ResultStr, !IO),
                        io.write(TypeClassInfoVarMCA, !IO),
                        io.nl(!IO),
                        io.nl(!IO)
                    )
                )
            )
        )
    ;
        do_make_typeclass_info_from_instance(InstanceId, ExistQVars,
            Context, TypeClassInfoVarMCA, Goals, !Info),
        trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
            some [SelectedPred, Level, IndentStr] (
                promise_pure (
                    semipure get_selected_pred(SelectedPred),
                    semipure get_level(Level),
                    impure set_level(Level - 1),
                    (
                        SelectedPred = no
                    ;
                        SelectedPred = yes,
                        IndentStr = string.duplicate_char(' ', (Level-1) * 4),
                        io.write_string(IndentStr, !IO),
                        io.write_string("instance computed result: ", !IO),
                        io.write(TypeClassInfoVarMCA, !IO),
                        io.nl(!IO),

                        io.write_string(IndentStr, !IO),
                        io.write_string("type_info_var_map ", !IO),
                        io.write(!.Info ^ poly_type_info_var_map, !IO),
                        io.nl(!IO),
                        io.write_string(IndentStr, !IO),
                        io.write_string("typeclass_info_map ", !IO),
                        io.write(!.Info ^ poly_typeclass_info_map, !IO),
                        io.nl(!IO),
                        io.write_string(IndentStr, !IO),
                        io.write_string("struct_var_map ", !IO),
                        io.write(!.Info ^ poly_const_struct_var_map, !IO),
                        io.nl(!IO),
                        io.nl(!IO)
                    )
                )
            )
        )
    ).

:- pred do_make_typeclass_info_from_instance(const_instance_id::in,
    existq_tvars::in, prog_context::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

do_make_typeclass_info_from_instance(InstanceId, ExistQVars, Context,
        TypeClassInfoVarMCA, Goals, !Info) :-
    poly_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    module_info_get_class_table(ModuleInfo, ClassTable),
    poly_info_get_typevarset(!.Info, TypeVarSet),
    poly_info_get_proofs(!.Info, Proofs0),

    InstanceId = ciid(InstanceNum, Constraint, Seen),
    Constraint = constraint(ClassName, ConstrainedTypes),

    list.length(ConstrainedTypes, ClassArity),
    ClassId = class_id(ClassName, ClassArity),

    map.lookup(InstanceTable, ClassId, InstanceList),
    list.det_index1(InstanceList, InstanceNum, ProofInstanceDefn),

    ProofInstanceDefn = hlds_instance_defn(_, _, _, InstanceConstraints,
        InstanceTypes, _, _, _, InstanceTVarset, InstanceProofs),

    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(KindMap),

    type_vars_list(InstanceTypes, InstanceTvars),
    get_unconstrained_tvars(InstanceTvars, InstanceConstraints,
        UnconstrainedTvars),

    % We can ignore the new typevarset because all the type variables
    % in the instance constraints and superclass proofs must appear
    % in the arguments of the instance, and all such variables
    % are bound when we call type_list_subsumes then apply
    % the resulting bindings.
    tvarset_merge_renaming(TypeVarSet, InstanceTVarset, _NewTVarset, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes,
        RenamedInstanceTypes),
    type_list_subsumes_det(RenamedInstanceTypes, ConstrainedTypes,
        InstanceSubst),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        InstanceConstraints, RenamedInstanceConstraints),
    apply_rec_subst_to_prog_constraint_list(InstanceSubst,
        RenamedInstanceConstraints, ActualInstanceConstraints0),
    % XXX document diamond as guess
    % XXX does anyone know what the preceding line means?
    list.delete_elems(ActualInstanceConstraints0, Seen,
        ActualInstanceConstraints),
    apply_variable_renaming_to_constraint_proofs(Renaming,
        InstanceProofs, RenamedInstanceProofs),
    apply_rec_subst_to_constraint_proofs(InstanceSubst,
        RenamedInstanceProofs, ActualInstanceProofs),

    apply_variable_renaming_to_tvar_list(Renaming, UnconstrainedTvars,
        RenamedUnconstrainedTvars),
    apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap,
        RenamedKindMap),
    apply_rec_subst_to_tvar_list(RenamedKindMap, InstanceSubst,
        RenamedUnconstrainedTvars, ActualUnconstrainedTypes),

    map.overlay(Proofs0, ActualInstanceProofs, Proofs),

    get_var_maps_snapshot("make_typeclass_info_from_instance",
        InitialVarMapsSnapshot, !Info),

    % Make the type_infos for the types that are constrained by this.
    % These are packaged in the typeclass_info.
    polymorphism_do_make_type_info_vars(ConstrainedTypes, Context,
        ArgTypeInfoVarsMCAs, TypeInfoGoals, !Info),

    % Make the typeclass_infos for the constraints from the context of the
    % instance decl.
    make_typeclass_info_vars_2(ActualInstanceConstraints, Seen, ExistQVars,
        Context, ArgTypeClassInfoVarsMCAs, InstanceConstraintGoals, !Info),

    % Make the type_infos for the unconstrained type variables
    % from the head of the instance declaration.
    polymorphism_do_make_type_info_vars(ActualUnconstrainedTypes, Context,
        ArgUnconstrainedTypeInfoVarsMCAs, UnconstrainedTypeInfoGoals, !Info),

    % --------------------- %

    map.lookup(ClassTable, ClassId, ClassDefn),

    get_arg_superclass_vars(ClassDefn, ConstrainedTypes, Proofs,
        ExistQVars, ArgSuperClassVarsMCAs, SuperClassGoals, !Info),

    PrevGoals = UnconstrainedTypeInfoGoals ++ TypeInfoGoals ++
        InstanceConstraintGoals ++ SuperClassGoals,
    % Lay out the argument variables as expected in the typeclass_info.
    ArgVarsMCAs = ArgUnconstrainedTypeInfoVarsMCAs ++
        ArgTypeClassInfoVarsMCAs ++
        ArgSuperClassVarsMCAs ++ ArgTypeInfoVarsMCAs,
    list.map(make_const_or_var_arg, ArgVarsMCAs, ArgCOVAs),

    Constraint = constraint(ConstraintClassName, ConstraintArgTypes),
    poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap0),
    (
        map.search(TypeClassInfoMap0, ConstraintClassName, ClassNameMap0),
        map.search(ClassNameMap0, ConstraintArgTypes, OldEntry0),
        OldEntry0 = typeclass_info_map_entry(_BaseConsId, ArgsMap0),
        map.search(ArgsMap0, ArgCOVAs, OldTypeClassInfoVarMCA0)
    ->
        TypeClassInfoVarMCA = OldTypeClassInfoVarMCA0,
        Goals = [],
        set_var_maps_snapshot("make_typeclass_info",
            InitialVarMapsSnapshot, !Info),
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 2, !Info)
    ;
        BaseConsId = base_typeclass_info_cons_id(InstanceTable,
            Constraint, InstanceNum, InstanceTypes),
        materialize_base_typeclass_info_var(Constraint, BaseConsId, BaseVar,
            BaseGoals, !Info),
        construct_typeclass_info(Constraint, BaseVar, BaseConsId, ArgVarsMCAs,
            InitialVarMapsSnapshot, TypeClassInfoVar, TypeClassInfoMCA,
            BaseGoals ++ PrevGoals, Goals, !Info),
        TypeClassInfoVarMCA = TypeClassInfoVar - TypeClassInfoMCA,

        % We must start the search from scratch, since construct_typeclass_info
        % may have reset all the cache maps.
        poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap1),
        ( map.search(TypeClassInfoMap1, ConstraintClassName, ClassNameMap1) ->
            ( map.search(ClassNameMap1, ConstraintArgTypes, OldEntry1) ->
                OldEntry1 = typeclass_info_map_entry(BaseConsId1, ArgsMap1),
                expect(unify(BaseConsId1, BaseConsId), $module, $pred,
                    "BaseConsId1 != BaseConsId"),
                map.det_insert(ArgCOVAs, TypeClassInfoVarMCA,
                    ArgsMap1, ArgsMap),
                Entry = typeclass_info_map_entry(BaseConsId, ArgsMap),
                map.det_update(ConstraintArgTypes, Entry,
                    ClassNameMap1, ClassNameMap),
                map.det_update(ConstraintClassName, ClassNameMap,
                    TypeClassInfoMap1, TypeClassInfoMap)
            ;
                ArgsMap = map.singleton(ArgCOVAs, TypeClassInfoVarMCA),
                Entry = typeclass_info_map_entry(BaseConsId, ArgsMap),
                map.det_insert(ConstraintArgTypes, Entry,
                    ClassNameMap1, ClassNameMap),
                map.det_update(ConstraintClassName, ClassNameMap,
                    TypeClassInfoMap1, TypeClassInfoMap)
            )
        ;
            ArgsMap = map.singleton(ArgCOVAs, TypeClassInfoVarMCA),
            Entry = typeclass_info_map_entry(BaseConsId, ArgsMap),
            ClassNameMap = map.singleton(ConstraintArgTypes, Entry),
            map.det_insert(ConstraintClassName, ClassNameMap,
                TypeClassInfoMap1, TypeClassInfoMap)
        ),
        poly_info_set_typeclass_info_map(TypeClassInfoMap, !Info)
    ),

    (
        TypeClassInfoVarMCA = _ - yes(TypeClassInfoConstArg),
        TypeClassInfoConstArg = csa_const_struct(TypeClassInfoConstArgNum)
    ->
        poly_info_get_const_struct_db(!.Info, ConstStructDb1),
        insert_constant_instance(InstanceId, TypeClassInfoConstArgNum,
            ConstStructDb1, ConstStructDb),
        poly_info_set_const_struct_db(ConstStructDb, !Info)
    ;
        true
    ).

:- pred construct_typeclass_info(prog_constraint::in,
    prog_var::in, cons_id::in,
    assoc_list(prog_var, maybe(const_struct_arg))::in, var_maps::in,
    prog_var::out, maybe(const_struct_arg)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

construct_typeclass_info(Constraint, BaseVar, BaseConsId, ArgVarsMCAs,
        InitialVarMapsSnapshot, TypeClassInfoVar, TypeClassInfoMCA,
        PrevGoals, AllGoals, !Info) :-
    % Build a unification to add the argvars to the base_typeclass_info.
    ConsId = typeclass_info_cell_constructor,

    poly_info_get_const_struct_db(!.Info, ConstStructDb0),
    const_struct_db_get_poly_enabled(ConstStructDb0, ConstStructEnabled),
    (
        ConstStructEnabled = yes,
        all_are_const_struct_args(ArgVarsMCAs, VarConstArgs)
    ->
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),

        set_var_maps_snapshot("construct_typeclass_info",
            InitialVarMapsSnapshot, !Info),
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, TypeClassInfoVarType, !Info),

        build_typeclass_info_type(Constraint, BaseConstArgType),
        BaseConstArg = csa_constant(BaseConsId, BaseConstArgType),
        StructArgs = [BaseConstArg | VarConstArgs],
        list.map(get_inst_of_const_struct_arg(ConstStructDb0),
            VarConstArgs, VarInsts),
        list.length(ArgVarsMCAs, NumArgs),
        InstConsId = cell_inst_cons_id(typeclass_info_cell, NumArgs),
        StructInst = bound(shared, inst_test_results_fgtc,
            [bound_functor(InstConsId, VarInsts)]),
        ConstStruct = const_struct(ConsId, StructArgs,
            TypeClassInfoVarType, StructInst),
        lookup_insert_const_struct(ConstStruct, ConstNum,
            ConstStructDb0, ConstStructDb),
        poly_info_set_const_struct_db(ConstStructDb, !Info),
        TypeClassInfoConstArg = csa_const_struct(ConstNum),
        TypeClassInfoMCA = yes(TypeClassInfoConstArg),

        % Create the construction unification to initialize the variable.
        ConstConsId = typeclass_info_const(ConstNum),
        Unification = construct(TypeClassInfoVar, ConstConsId, [], [],
            construct_statically, cell_is_shared, no_construct_sub_info),
        UnifyMode = (free -> ground(shared, none)) -
            (ground(shared, none) -> ground(shared, none)),
        % XXX The UnifyContext is wrong.
        UnifyContext = unify_context(umc_explicit, []),
        TypeClassInfoRHS = rhs_functor(ConstConsId, no, []),
        GoalExpr = unify(TypeClassInfoVar, TypeClassInfoRHS, UnifyMode,
            Unification, UnifyContext),

        % Create a goal_info for the unification.
        goal_info_init(GoalInfo0),
        NonLocals = set_of_var.make_singleton(TypeClassInfoVar),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
        % Note that we could perhaps be more accurate than `ground(shared)',
        % but it shouldn't make any difference.
        InstResults = inst_test_results(inst_result_is_ground,
            inst_result_does_not_contain_any,
            inst_result_contains_instnames_known(set.init),
            inst_result_contains_types_known(set.init)),
        TypeClassInfoInst = bound(shared, InstResults,
            [bound_functor(ConsId, [])]),
        TypeClassInfoVarInst = TypeClassInfoVar - TypeClassInfoInst,
        InstMapDelta = instmap_delta_from_assoc_list([TypeClassInfoVarInst]),
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo1, GoalInfo2),
        goal_info_set_determinism(detism_det, GoalInfo2, GoalInfo),

        Goal = hlds_goal(GoalExpr, GoalInfo),
        % XXX reset varset and vartypes
        AllGoals = [Goal]
    ;
        TypeClassInfoMCA = no,
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, _TypeClassInfoVarType, !Info),
        assoc_list.keys(ArgVarsMCAs, ArgVars),
        AllArgVars = [BaseVar | ArgVars],

        % Create the construction unification to initialize the variable.
        TypeClassInfoRHS = rhs_functor(ConsId, no, AllArgVars),
        UniMode = (free - ground(shared, none) ->
            ground(shared, none) - ground(shared, none)),
        list.length(AllArgVars, NumArgs),
        list.duplicate(NumArgs, UniMode, UniModes),
        Unification = construct(TypeClassInfoVar, ConsId, AllArgVars, UniModes,
            construct_dynamically, cell_is_unique, no_construct_sub_info),
        UnifyMode = (free -> ground(shared, none)) -
            (ground(shared, none) -> ground(shared, none)),
        UnifyContext = unify_context(umc_explicit, []),
        % XXX The UnifyContext is wrong.
        GoalExpr = unify(TypeClassInfoVar, TypeClassInfoRHS, UnifyMode,
            Unification, UnifyContext),

        % Create a goal_info for the unification.
        goal_info_init(GoalInfo0),
        set_of_var.list_to_set([TypeClassInfoVar | AllArgVars], NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
        list.duplicate(NumArgs, ground(shared, none), ArgInsts),
        % Note that we could perhaps be more accurate than `ground(shared)',
        % but it shouldn't make any difference.
        InstConsId = cell_inst_cons_id(typeclass_info_cell, NumArgs),
        InstResults = inst_test_results(inst_result_is_ground,
            inst_result_does_not_contain_any,
            inst_result_contains_instnames_known(set.init),
            inst_result_contains_types_unknown),
        % XXX that should be inst_result_contains_types_known(set.init),
        TypeClassInfoInst = bound(unique, InstResults,
            [bound_functor(InstConsId, ArgInsts)]),
        TypeClassInfoVarInst = TypeClassInfoVar - TypeClassInfoInst,
        InstMapDelta = instmap_delta_from_assoc_list([TypeClassInfoVarInst]),
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo1, GoalInfo2),
        goal_info_set_determinism(detism_det, GoalInfo2, GoalInfo),

        Goal = hlds_goal(GoalExpr, GoalInfo),
        AllGoals = PrevGoals ++ [Goal]
    ).

%---------------------------------------------------------------------------%

:- pred get_arg_superclass_vars(hlds_class_defn::in, list(mer_type)::in,
    constraint_proof_map::in, existq_tvars::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

get_arg_superclass_vars(ClassDefn, InstanceTypes, SuperClassProofs, ExistQVars,
        SuperClassTypeClassInfoVarsMCAs, SuperClassGoals, !Info) :-
    poly_info_get_proofs(!.Info, Proofs),

    poly_info_get_typevarset(!.Info, TVarSet0),
    SuperClasses0 = ClassDefn ^ class_supers,
    ClassVars0 = ClassDefn ^ class_vars,
    ClassTVarSet = ClassDefn ^ class_tvarset,
    tvarset_merge_renaming(TVarSet0, ClassTVarSet, TVarSet1, Renaming),
    poly_info_set_typevarset(TVarSet1, !Info),

    apply_variable_renaming_to_tvar_list(Renaming, ClassVars0, ClassVars),
    map.from_corresponding_lists(ClassVars, InstanceTypes, TypeSubst),

    apply_variable_renaming_to_prog_constraint_list(Renaming,
        SuperClasses0, SuperClasses1),
    apply_rec_subst_to_prog_constraint_list(TypeSubst,
        SuperClasses1, SuperClasses),

    poly_info_set_proofs(SuperClassProofs, !Info),
    make_superclasses_from_proofs(SuperClasses, ExistQVars,
        SuperClassTypeClassInfoVarsMCAs, SuperClassGoals, !Info),
    poly_info_set_proofs(Proofs, !Info).

:- pred make_superclasses_from_proofs(list(prog_constraint)::in,
    existq_tvars::in, assoc_list(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

make_superclasses_from_proofs([], _, [], [], !Info).
make_superclasses_from_proofs([Constraint | Constraints], ExistQVars,
        [TypeClassInfoVarMCA | TypeClassInfoVarsMCAs], Goals, !Info) :-
    term.context_init(Context),
    make_typeclass_info_var(Constraint, [], ExistQVars, Context,
        TypeClassInfoVarMCA, HeadGoals, !Info),
    make_superclasses_from_proofs(Constraints, ExistQVars,
        TypeClassInfoVarsMCAs, TailGoals, !Info),
    Goals = HeadGoals ++ TailGoals.

%-----------------------------------------------------------------------------%

    % Produce the typeclass_infos for the existential class constraints
    % for a call or deconstruction unification.
    %
:- pred make_existq_typeclass_info_vars(list(prog_constraint)::in,
    list(prog_var)::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

make_existq_typeclass_info_vars(ExistentialConstraints, ExtraTypeClassVars,
        ExtraGoals, !Info) :-
    poly_info_get_rtti_varmaps(!.Info, OldRttiVarMaps),
    make_typeclass_info_head_vars(do_record_type_info_locns,
        ExistentialConstraints, ExtraTypeClassVars, !Info),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    list.foldl(rtti_reuse_typeclass_info_var, ExtraTypeClassVars,
        RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info),

    constraint_list_get_tvars(ExistentialConstraints, TVars0),
    list.sort_and_remove_dups(TVars0, TVars),
    list.foldl2(polymorphism_maybe_extract_type_info(OldRttiVarMaps), TVars,
        [], ExtraGoals, !Info).

    % For code which requires mode reordering, we may have already seen uses
    % of some of the type variables produced by this call. At the point of the
    % use of a type variable that we haven't seen before, we assume that it is
    % unconstrained. If it turns out that the type variable is constrained,
    % and the type_info is contained in a typeclass_info, we need to generate
    % code to extract it here.
    %
:- pred polymorphism_maybe_extract_type_info(rtti_varmaps::in, tvar::in,
    list(hlds_goal)::in, list(hlds_goal)::out, poly_info::in, poly_info::out)
    is det.

polymorphism_maybe_extract_type_info(OldRttiVarMaps, TVar, !ExtraGoals,
        !Info) :-
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps),
    (
        rtti_search_type_info_locn(OldRttiVarMaps, TVar,
            type_info(TypeInfoVar0)),
        rtti_search_type_info_locn(RttiVarMaps, TVar,
            typeclass_info(TypeClassInfoVar, Index))
    ->
        polymorphism_extract_type_info(TVar, TypeClassInfoVar, Index, NewGoals,
            TypeInfoVar1, !Info),
        assign_var(TypeInfoVar0, TypeInfoVar1, AssignGoal),
        !:ExtraGoals = NewGoals ++ [AssignGoal | !.ExtraGoals]
    ;
        true
    ).

%---------------------------------------------------------------------------%

polymorphism_make_type_info_vars(Types, Context, ExtraVars,
        ExtraGoals, !Info) :-
    polymorphism_do_make_type_info_vars(Types, Context, ExtraVarsMCAs,
        ExtraGoals, !Info),
    assoc_list.keys(ExtraVarsMCAs, ExtraVars).

polymorphism_make_type_info_var(Type, Context, ExtraVar, ExtraGoals, !Info) :-
    polymorphism_do_make_type_info_var(Type, Context, ExtraVarMCA,
        ExtraGoals, !Info),
    ExtraVarMCA = ExtraVar - _.

:- pred polymorphism_do_make_type_info_vars(list(mer_type)::in,
    term.context::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_do_make_type_info_vars([], _, [], [], !Info).
polymorphism_do_make_type_info_vars([Type | Types], Context,
        VarsMCAs, Goals, !Info) :-
    polymorphism_do_make_type_info_var(Type, Context, HeadVarMCA,
        HeadGoals, !Info),
    polymorphism_do_make_type_info_vars(Types, Context, TailVarsMCAs,
        TailGoals, !Info),
    VarsMCAs = [HeadVarMCA | TailVarsMCAs],
    Goals = HeadGoals ++ TailGoals.

:- pred polymorphism_do_make_type_info_var(mer_type::in, term.context::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_do_make_type_info_var(Type, Context, VarMCA, ExtraGoals, !Info) :-
    % First handle statically known types (i.e. types which are not
    % type variables).
    ( type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs) ->
        % This occurs for code where a predicate calls a polymorphic predicate
        % with a type whose type constructor is of variable arity. The
        % transformation we perform is basically the same as in the usual case
        % below, except that we map pred types to pred/0, func types to func/0
        % and tuple types to tuple/0 for the purposes of creating type_infos.
        % To allow univ_to_type to check the type_infos correctly, the actual
        % arity is added to the type_info we create.
        %
        % XXX FIXME (RTTI for higher order impure code)
        % we should not ignore the purity of higher order procs;
        % it should get included in the RTTI.
        polymorphism_make_type_info(Type, TypeCtor, TypeArgs, yes,
            Context, VarMCA, ExtraGoals, !Info)
    ;
        (
            ( Type = defined_type(_, _, _)
            ; Type = builtin_type(_)
            ; Type = tuple_type(_, _)
            ; Type = higher_order_type(_,_, _, _)
            ; Type = apply_n_type(_, _, _)
            ; Type = kinded_type(_, _)
            ),
            type_to_ctor_and_args_det(Type, TypeCtor, TypeArgs),
            % This occurs for code where a predicate calls a polymorphic
            % predicate with a known value of the type variable. The
            % transformation we perform is shown in the comment at the top
            % of the module.
            polymorphism_make_type_info(Type, TypeCtor, TypeArgs, no,
                Context, VarMCA, ExtraGoals, !Info)
        ;
            % Now handle the cases of types which are not known statically,
            % i.e. type variables.
            Type = type_variable(TypeVar, _),
            get_type_info_locn(TypeVar, TypeInfoLocn, !Info),
            get_type_info_from_locn(TypeVar, TypeInfoLocn, Var, ExtraGoals,
                !Info),
            VarMCA = Var - no
        )
    ).

:- pred polymorphism_make_type_info(mer_type::in, type_ctor::in,
    list(mer_type)::in, bool::in, prog_context::in,
    pair(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism_make_type_info(Type, TypeCtor, TypeArgs, TypeCtorIsVarArity,
        Context, TypeInfoVarMCA, ExtraGoals, !Info) :-
    poly_info_get_type_info_var_map(!.Info, TypeInfoVarMap0),
    (
        map.search(TypeInfoVarMap0, TypeCtor, TypeCtorVarMap0),
        map.search(TypeCtorVarMap0, TypeArgs, OldTypeInfoVarMCA)
    ->
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        TypeInfoVarMCA = OldTypeInfoVarMCA,
        ExtraGoals = []
    ;
        polymorphism_construct_type_info(Type, TypeCtor, TypeArgs,
            TypeCtorIsVarArity, Context, TypeInfoVar, TypeInfoConstArg,
            ExtraGoals, !Info),
        TypeInfoVarMCA = TypeInfoVar - TypeInfoConstArg,
        % We have to get the type_info_var_map again since the call just above
        % could have added relevant new entries to it.
        poly_info_get_type_info_var_map(!.Info, TypeInfoVarMap1),
        ( map.search(TypeInfoVarMap1, TypeCtor, TypeCtorVarMap1) ->
            map.det_insert(TypeArgs, TypeInfoVarMCA,
                TypeCtorVarMap1, TypeCtorVarMap),
            map.det_update(TypeCtor, TypeCtorVarMap,
                TypeInfoVarMap1, TypeInfoVarMap)
        ;
            TypeCtorVarMap = map.singleton(TypeArgs, TypeInfoVarMCA),
            map.det_insert(TypeCtor, TypeCtorVarMap,
                TypeInfoVarMap1, TypeInfoVarMap)
        ),
        poly_info_set_type_info_var_map(TypeInfoVarMap, !Info)
    ).

:- pred polymorphism_construct_type_info(mer_type::in, type_ctor::in,
    list(mer_type)::in, bool::in, prog_context::in,
    prog_var::out, maybe(const_struct_arg)::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism_construct_type_info(Type, TypeCtor, TypeArgs, TypeCtorIsVarArity,
        Context, Var, MCA, ExtraGoals, !Info) :-
    get_var_maps_snapshot("polymorphism_construct_type_info",
        InitialVarMapsSnapshot, !Info),

    % Create the typeinfo vars for the arguments.
    polymorphism_do_make_type_info_vars(TypeArgs, Context,
        ArgTypeInfoVarsMCAs, ArgTypeInfoGoals, !Info),

    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),

    TypeCtorConsId = type_ctor_info_cons_id(TypeCtor),
    TypeCtorConsIdConstArg = csa_constant(TypeCtorConsId, type_info_type),
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap0),
    ( map.search(ConstStructVarMap0, TypeCtorConsIdConstArg, OldTypeCtorVar) ->
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        TypeCtorVar = OldTypeCtorVar,
        TypeCtorGoals = [],
        VarSet1 = VarSet0,
        VarTypes1 = VarTypes0,
        RttiVarMaps1 = RttiVarMaps0
    ;
        init_const_type_ctor_info_var_from_cons_id(Type, TypeCtorConsId,
            TypeCtorVar, TypeCtorGoal, VarSet0, VarSet1, VarTypes0, VarTypes1,
            RttiVarMaps0, RttiVarMaps1),
        TypeCtorGoals = [TypeCtorGoal],
        map.det_insert(TypeCtorConsIdConstArg, TypeCtorVar,
            ConstStructVarMap0, ConstStructVarMap1),
        poly_info_set_const_struct_var_map(ConstStructVarMap1, !Info)
    ),

    poly_info_set_varset_and_types(VarSet1, VarTypes1, !Info),
    poly_info_set_rtti_varmaps(RttiVarMaps1, !Info),

    % The rest of this predicate create code that constructs the second cell
    % of a type_info for Type if we need a second cell for Type. This cell
    % will usually be of the form:
    %
    %   TypeInfoVar = type_info(TypeCtorVar, ArgTypeInfoVars...)
    %
    % However, if TypeCtorIsVarArity is true, then it will be of the form
    %
    %   TypeInfoVar = type_info(TypeCtorVar, Arity, ArgTypeInfoVars...)
    %
    % TypeCtorVar should be the variable holding the type_ctor_info for the
    % principal type constructor of Type, and TypeCtorIsVarArity should be
    % true iff the type constructor it represents has a variable arity.
    %
    % ArgTypeInfoVars should be variables holding the type_infos (or
    % type_ctor_infos for zero-arity types) of the argument types of Type.
    %
    % The returned Var will be bound to the type_info cell of Type if such
    % a cell had to be allocated, and to the type_ctor_info of Type's only
    % type constructor if it didn't.

    (
        % Unfortunately, if the type's type constructor has variable arity,
        % we cannot use a one-cell representation for that type.
        TypeCtorIsVarArity = no,
        ArgTypeInfoVarsMCAs = []
    ->
        % We do not need a second cell for a separate typeinfo; we will use
        % the type_ctor_info as the type_info.

        % Since this type_ctor_info is pretending to be a type_info,
        % we need to adjust its type. We handle type_ctor_info_const cons_ids
        % specially to make sure that this causes no problems.
        TypeInfoType = type_info_type,
        Var = TypeCtorVar,
        TypeCtorConstArg = csa_constant(TypeCtorConsId, type_info_type),
        MCA = yes(TypeCtorConstArg),
        ExtraGoals = ArgTypeInfoGoals ++ TypeCtorGoals,
        update_var_type(TypeCtorVar, TypeInfoType, VarTypes1, VarTypes),
        poly_info_set_varset_and_types(VarSet1, VarTypes, !Info)
    ;
        % We do need a second cell for a separate typeinfo.
        Cell = type_info_cell(TypeCtor),
        ConsId = cell_cons_id(Cell),

        poly_info_get_const_struct_db(!.Info, ConstStructDb0),
        const_struct_db_get_poly_enabled(ConstStructDb0, Enabled),
        (
            Enabled = yes,
            all_are_const_struct_args(ArgTypeInfoVarsMCAs,
                ArgTypeInfoConstArgs)
        ->
            TypeCtorConstArg = csa_constant(TypeCtorConsId, type_info_type),
            TypeCtorInst = bound(shared, inst_test_results_fgtc,
                [bound_functor(TypeCtorConsId, [])]),
            list.map(get_inst_of_const_struct_arg(ConstStructDb0),
                ArgTypeInfoConstArgs, ArgTypeInfoInsts),
            (
                TypeCtorIsVarArity = yes,
                list.length(ArgTypeInfoVarsMCAs, ActualArity),
                ArityConstArg = csa_constant(int_const(ActualArity), int_type),
                ArityInst = bound(shared, inst_test_results_fgtc,
                    [bound_functor(int_const(ActualArity), [])]),
                StructConstArgs =
                    [TypeCtorConstArg, ArityConstArg | ArgTypeInfoConstArgs],
                StructArgInsts = [TypeCtorInst, ArityInst | ArgTypeInfoInsts]
            ;
                TypeCtorIsVarArity = no,
                StructConstArgs = [TypeCtorConstArg | ArgTypeInfoConstArgs],
                StructArgInsts = [TypeCtorInst | ArgTypeInfoInsts]
            ),
            StructType = type_info_type,
            list.length(ArgTypeInfoConstArgs, NumArgs),
            InstConsId = cell_inst_cons_id(Cell, NumArgs),
            StructInst = bound(shared, inst_test_results_fgtc,
                [bound_functor(InstConsId, StructArgInsts)]),

            ConstStruct = const_struct(ConsId, StructConstArgs,
                StructType, StructInst),
            lookup_insert_const_struct(ConstStruct, ConstNum,
                ConstStructDb0, ConstStructDb),
            MCA = yes(csa_const_struct(ConstNum)),
            poly_info_set_const_struct_db(ConstStructDb, !Info),

            set_var_maps_snapshot("maybe_init_second_cell",
                InitialVarMapsSnapshot, !Info),

            new_type_info_var(Type, type_info, Var, !Info),
            Unification = construct(Var, type_info_const(ConstNum),
                [], [], construct_statically, cell_is_shared,
                no_construct_sub_info),
            UnifyMode = (free -> ground(shared, none)) -
                (ground(shared, none) -> ground(shared, none)),
            UnifyContext = unify_context(umc_explicit, []),
            % XXX The UnifyContext is wrong.
            TypeInfoRHS = rhs_functor(type_info_const(ConstNum), no, []),
            Unify = unify(Var, TypeInfoRHS, UnifyMode, Unification,
                UnifyContext),

            % Create a goal_info for the unification.
            NonLocals = set_of_var.make_singleton(Var),
            % Note that we could be more accurate than `ground(shared)',
            % but it shouldn't make any difference.
            InstResults = inst_test_results(inst_result_is_ground,
                inst_result_does_not_contain_any,
                inst_result_contains_instnames_known(set.init),
                inst_result_contains_types_known(set.init)),
            VarInst = bound(shared, InstResults,
                [bound_functor(InstConsId, [])]),
            InstMapDelta = instmap_delta_from_assoc_list([Var - VarInst]),
            goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
                GoalInfo),
            TypeInfoGoal = hlds_goal(Unify, GoalInfo),
            ExtraGoals = [TypeInfoGoal]
        ;
            assoc_list.keys(ArgTypeInfoVarsMCAs, ArgTypeInfoVars),
            (
                TypeCtorIsVarArity = yes,
                list.length(ArgTypeInfoVars, ActualArity),
                get_poly_const(ActualArity, ArityVar, ArityGoals, !Info),
                % The call get_poly_const may (and probably will) allocate
                % a variable, so VarSet1, VarTypes1 and RttiVarMaps1 are
                % all out of date.
                poly_info_get_varset(!.Info, VarSet2),
                poly_info_get_var_types(!.Info, VarTypes2),
                poly_info_get_rtti_varmaps(!.Info, RttiVarMaps2),
                init_type_info_var(Type,
                    [TypeCtorVar, ArityVar | ArgTypeInfoVars],
                    no, Var, TypeInfoGoal,
                    VarSet2, VarSet, VarTypes2, VarTypes,
                    RttiVarMaps2, RttiVarMaps),
                ExtraGoals = TypeCtorGoals ++ ArityGoals ++ ArgTypeInfoGoals
                    ++ [TypeInfoGoal]
            ;
                TypeCtorIsVarArity = no,
                init_type_info_var(Type, [TypeCtorVar | ArgTypeInfoVars],
                    no, Var, TypeInfoGoal,
                    VarSet1, VarSet, VarTypes1, VarTypes,
                    RttiVarMaps1, RttiVarMaps),
                ExtraGoals = TypeCtorGoals ++ ArgTypeInfoGoals ++
                    [TypeInfoGoal]
            ),
            poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
            poly_info_set_rtti_varmaps(RttiVarMaps, !Info),
            MCA = no
        )
    ).

get_special_proc(Type, SpecialPredId, ModuleInfo, PredName, PredId, ProcId) :-
    TypeCategory = classify_type(ModuleInfo, Type),
    get_category_name(TypeCategory) = MaybeCategoryName,
    (
        MaybeCategoryName = no,
        module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
        type_to_ctor_det(Type, TypeCtor),
        map.search(SpecialPredMap, SpecialPredId - TypeCtor, PredId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Module = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        PredName = qualified(Module, Name),
        special_pred_mode_num(SpecialPredId, ProcInt),
        proc_id_to_int(ProcId, ProcInt)
    ;
        MaybeCategoryName = yes(CategoryName),
        special_pred_name_arity(SpecialPredId, SpecialName, _, Arity),
        Name = "builtin_" ++ SpecialName ++ "_" ++ CategoryName,
        lookup_builtin_pred_proc_id(ModuleInfo, mercury_private_builtin_module,
            Name, pf_predicate, Arity, only_mode, PredId, ProcId),
        PredName = qualified(mercury_private_builtin_module, Name)
    ).

get_special_proc_det(Type, SpecialPredId, ModuleInfo, PredName,
        PredId, ProcId) :-
    (
        get_special_proc(Type, SpecialPredId, ModuleInfo,
            PredNamePrime, PredIdPrime, ProcIdPrime)
    ->
        PredName = PredNamePrime,
        PredId = PredIdPrime,
        ProcId = ProcIdPrime
    ;
        unexpected($module, $pred, "get_special_proc failed")
    ).

:- func get_category_name(type_ctor_category) = maybe(string).

get_category_name(CtorCat) = MaybeName :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_int),
        MaybeName = yes("int")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        MaybeName = yes("character")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        MaybeName = yes("float")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        MaybeName = yes("string")
    ;
        CtorCat = ctor_cat_higher_order,
        MaybeName = yes("pred")
    ;
        CtorCat = ctor_cat_tuple,
        MaybeName = yes("tuple")
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_system(_)
        ),
        MaybeName = no
    ;
        CtorCat = ctor_cat_variable,
        unexpected($module, $pred, "variable type")
    ;
        CtorCat = ctor_cat_void,
        unexpected($module, $pred, "void_type")
    ).

init_type_info_var(Type, ArgVars, MaybePreferredVar, TypeInfoVar, TypeInfoGoal,
        !VarSet, !VarTypes, !RttiVarMaps) :-
    type_to_ctor_det(Type, TypeCtor),
    Cell = type_info_cell(TypeCtor),
    ConsId = cell_cons_id(Cell),
    do_init_type_info_var(Type, Cell, ConsId, ArgVars, MaybePreferredVar,
        TypeInfoVar, TypeInfoGoal, !VarSet, !VarTypes, !RttiVarMaps).

:- pred do_init_type_info_var(mer_type::in, polymorphism_cell::in, cons_id::in,
    list(prog_var)::in, maybe(prog_var)::in, prog_var::out, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

do_init_type_info_var(Type, Cell, ConsId, ArgVars, MaybePreferredVar,
        TypeInfoVar, TypeInfoGoal, !VarSet, !VarTypes, !RttiVarMaps) :-
    TypeInfoRHS = rhs_functor(ConsId, no, ArgVars),
    % Introduce a new variable.
    (
        MaybePreferredVar = yes(TypeInfoVar)
    ;
        MaybePreferredVar = no,
        new_type_info_var_raw(Type, type_info, TypeInfoVar,
            !VarSet, !VarTypes, !RttiVarMaps)
    ),

    % Create the construction unification to initialize the variable.
    UniMode = (free - ground(shared, none) ->
        ground(shared, none) - ground(shared, none)),
    list.length(ArgVars, NumArgVars),
    list.duplicate(NumArgVars, UniMode, UniModes),
    Unification = construct(TypeInfoVar, ConsId, ArgVars, UniModes,
        construct_dynamically, cell_is_unique, no_construct_sub_info),
    UnifyMode = (free -> ground(shared, none)) -
        (ground(shared, none) -> ground(shared, none)),
    UnifyContext = unify_context(umc_explicit, []),
    % XXX The UnifyContext is wrong.
    Unify = unify(TypeInfoVar, TypeInfoRHS, UnifyMode, Unification,
        UnifyContext),

    % Create a goal_info for the unification.
    set_of_var.list_to_set([TypeInfoVar | ArgVars], NonLocals),
    list.duplicate(NumArgVars, ground(shared, none), ArgInsts),
    % Note that we could perhaps be more accurate than `ground(shared)',
    % but it shouldn't make any difference.
    InstConsId = cell_inst_cons_id(Cell, NumArgVars),
    InstResults = inst_test_results(inst_result_is_ground,
        inst_result_does_not_contain_any,
        inst_result_contains_instnames_known(set.init),
        inst_result_contains_types_unknown),
    TypeInfoVarInst = bound(unique, InstResults,
        [bound_functor(InstConsId, ArgInsts)]),
    InstMapDelta = instmap_delta_from_assoc_list(
        [TypeInfoVar - TypeInfoVarInst]),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, GoalInfo),
    TypeInfoGoal = hlds_goal(Unify, GoalInfo).

init_const_type_ctor_info_var(Type, TypeCtor, TypeCtorInfoVar,
        ConsId, TypeCtorInfoGoal, !VarSet, !VarTypes, !RttiVarMaps) :-
    ConsId = type_ctor_info_cons_id(TypeCtor),
    init_const_type_ctor_info_var_from_cons_id(Type, ConsId,
        TypeCtorInfoVar, TypeCtorInfoGoal, !VarSet, !VarTypes, !RttiVarMaps).

:- pred init_const_type_ctor_info_var_from_cons_id(mer_type::in, cons_id::in,
    prog_var::out, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

init_const_type_ctor_info_var_from_cons_id(Type, ConsId,
        TypeCtorInfoVar, TypeCtorInfoGoal, !VarSet, !VarTypes, !RttiVarMaps) :-
    % Introduce a new variable.
    new_type_info_var_raw(Type, type_ctor_info, TypeCtorInfoVar,
        !VarSet, !VarTypes, !RttiVarMaps),

    % Create the construction unification to initialize the variable.
    TypeInfoRHS = rhs_functor(ConsId, no, []),
    Unification = construct(TypeCtorInfoVar, ConsId, [], [],
        construct_dynamically, cell_is_shared, no_construct_sub_info),
    UnifyMode = (free -> ground(shared, none)) -
        (ground(shared, none) -> ground(shared, none)),
    UnifyContext = unify_context(umc_explicit, []),
    % XXX The UnifyContext is wrong.
    Unify = unify(TypeCtorInfoVar, TypeInfoRHS, UnifyMode,
        Unification, UnifyContext),

    % Create a goal_info for the unification.
    NonLocals = set_of_var.make_singleton(TypeCtorInfoVar),
    InstmapDelta = instmap_delta_bind_var(TypeCtorInfoVar),
    goal_info_init(NonLocals, InstmapDelta, detism_det, purity_pure, GoalInfo),
    TypeCtorInfoGoal = hlds_goal(Unify, GoalInfo).

%---------------------------------------------------------------------------%

:- pred make_head_vars(list(tvar)::in, tvarset::in,
    list(prog_var)::out, poly_info::in, poly_info::out) is det.

make_head_vars([], _, [], !Info).
make_head_vars([TypeVar | TypeVars], TypeVarSet, TypeInfoVars, !Info) :-
    get_tvar_kind(!.Info ^ poly_tvar_kinds, TypeVar, Kind),
    Type = type_variable(TypeVar, Kind),
    new_type_info_var(Type, type_info, Var, !Info),
    ( varset.search_name(TypeVarSet, TypeVar, TypeVarName) ->
        poly_info_get_varset(!.Info, VarSet0),
        VarName = "TypeInfo_for_" ++ TypeVarName,
        varset.name_var(Var, VarName, VarSet0, VarSet),
        poly_info_set_varset(VarSet, !Info)
    ;
        true
    ),
    make_head_vars(TypeVars, TypeVarSet, TypeInfoVars1, !Info),
    TypeInfoVars = [Var | TypeInfoVars1].

:- pred new_type_info_var(mer_type::in, type_info_kind::in,
    prog_var::out, poly_info::in, poly_info::out) is det.

new_type_info_var(Type, Kind, Var, !Info) :-
    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    new_type_info_var_raw(Type, Kind, Var, VarSet0, VarSet,
        VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
    poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info).

new_type_info_var_raw(Type, Kind, Var, !VarSet, !VarTypes, !RttiVarMaps) :-
    % Introduce new variable.
    varset.new_var(Var, !VarSet),
    term.var_to_int(Var, VarNum),
    string.int_to_string(VarNum, VarNumStr),
    (
        Kind = type_info,
        Prefix = "TypeInfo_",
        rtti_det_insert_type_info_type(Var, Type, !RttiVarMaps)
    ;
        Kind = type_ctor_info,
        Prefix = "TypeCtorInfo_"

        % XXX Perhaps we should record the variables holding
        % type_ctor_infos in the rtti_varmaps somewhere.
    ),
    Name = Prefix ++ VarNumStr,
    varset.name_var(Var, Name, !VarSet),
    add_var_type(Var, type_info_type, !VarTypes).

%---------------------------------------------------------------------------%

:- pred get_type_info_locn(tvar::in, type_info_locn::out, poly_info::in,
    poly_info::out) is det.

get_type_info_locn(TypeVar, TypeInfoLocn, !Info) :-
    % If we have already allocated a location for this type_info, then all
    % we need to do is to extract the type_info variable from its location.
    (
        rtti_search_type_info_locn(!.Info ^ poly_rtti_varmaps, TypeVar,
            TypeInfoLocnPrime)
    ->
        TypeInfoLocn = TypeInfoLocnPrime
    ;
        % Otherwise, we need to create a new type_info variable, and set the
        % location for this type variable to be that type_info variable.
        %
        % This is wrong if the type variable is one of the existentially
        % quantified variables of a called predicate and the variable occurs
        % in an existential typeclass constraint. In that case the type_info
        % will be stored in the typeclass_info variable produced by the
        % predicate, not in a type_info variable. maybe_extract_type_info
        % will fix this up when the typeclass_info is created.

        get_tvar_kind(!.Info ^ poly_tvar_kinds, TypeVar, Kind),
        Type = type_variable(TypeVar, Kind),
        new_type_info_var(Type, type_info, Var, !Info),
        TypeInfoLocn = type_info(Var),
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_det_insert_type_info_locn(TypeVar, TypeInfoLocn,
            RttiVarMaps0, RttiVarMaps),
        poly_info_set_rtti_varmaps(RttiVarMaps, !Info)
    ).

    % Generate code to get the value of a type variable.
    %
:- pred get_type_info_from_locn(tvar::in, type_info_locn::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

get_type_info_from_locn(TypeVar, TypeInfoLocn, Var, ExtraGoals, !Info) :-
    (
        % If the typeinfo is available in a variable, just use it.
        TypeInfoLocn = type_info(TypeInfoVar),
        Var = TypeInfoVar,
        ExtraGoals = []
    ;
        % If the typeinfo is in a typeclass_info, then we need to extract it
        % before using it.
        TypeInfoLocn = typeclass_info(TypeClassInfoVar, Index),
        polymorphism_extract_type_info(TypeVar, TypeClassInfoVar, Index,
            ExtraGoals, Var, !Info)
    ).

:- pred polymorphism_extract_type_info(tvar::in, prog_var::in, int::in,
    list(hlds_goal)::out, prog_var::out, poly_info::in, poly_info::out) is det.

polymorphism_extract_type_info(TypeVar, TypeClassInfoVar, Index, Goals,
        TypeInfoVar, !Info) :-
    get_poly_const(Index, IndexVar, IndexGoals, !Info),
    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    poly_info_get_module_info(!.Info, ModuleInfo),
    poly_info_get_tvar_kinds(!.Info, TVarKinds),
    get_tvar_kind(TVarKinds, TypeVar, Kind),
    IndexIntOrVar = iov_var(IndexVar),
    gen_extract_type_info(ModuleInfo, TypeVar, Kind, TypeClassInfoVar,
        IndexIntOrVar, ExtractGoals, TypeInfoVar,
        VarSet0, VarSet, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
    Goals = IndexGoals ++ ExtractGoals,
    poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info).

gen_extract_type_info(ModuleInfo, TypeVar, Kind, TypeClassInfoVar,
        IndexIntOrVar, Goals, TypeInfoVar, !VarSet, !VarTypes, !RttiVarMaps) :-
    (
        IndexIntOrVar = iov_int(Index),
        % We cannot call get_poly_const since we don't have a poly_info.
        make_int_const_construction_alloc(Index, yes("TypeInfoIndex"),
            IndexGoal, IndexVar, !VarSet, !VarTypes),
        IndexGoals = [IndexGoal]
    ;
        IndexIntOrVar = iov_var(IndexVar),
        IndexGoals = []
    ),
    Type = type_variable(TypeVar, Kind),
    new_type_info_var_raw(Type, type_info, TypeInfoVar,
        !VarSet, !VarTypes, !RttiVarMaps),
    goal_util.generate_simple_call(mercury_private_builtin_module,
        "type_info_from_typeclass_info", pf_predicate, only_mode,
        detism_det, purity_pure, [TypeClassInfoVar, IndexVar, TypeInfoVar], [],
        instmap_delta_bind_var(TypeInfoVar), ModuleInfo, term.context_init,
        CallGoal),
    Goals = IndexGoals ++ [CallGoal].

:- pred get_poly_const(int::in, prog_var::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

get_poly_const(IntConst, IntVar, Goals, !Info) :-
    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),

    poly_info_get_int_const_map(!.Info, IntConstMap0),
    ( map.search(IntConstMap0, IntConst, IntVarPrime) ->
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        IntVar = IntVarPrime,
        Goals = []
    ;
        make_int_const_construction_alloc(IntConst,
            yes("PolyConst" ++ string.int_to_string(IntConst)),
            Goal, IntVar, VarSet0, VarSet, VarTypes0, VarTypes),
        map.det_insert(IntConst, IntVar, IntConstMap0, IntConstMap),
        poly_info_set_int_const_map(IntConstMap, !Info),
        poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
        Goals = [Goal]
    ).

%-----------------------------------------------------------------------------%

    % Usually when we call make_typeclass_info_head_var, we want to record
    % the type_info_locn for each constrained type var so that later goals
    % will know where to get the type_info from. However, when setting up
    % head vars for existential constraints on the predicate/function we
    % are processing, we assume that the type_infos will be produced
    % somewhere else in the goal. In this case, we don't want to record
    % the type_info_locns (if we did, then the code to actually produce the
    % type_info will just try to get it from here, which would be a mode
    % error).
    %
:- type record_type_info_locns
    --->    do_record_type_info_locns
    ;       do_not_record_type_info_locns.

    % Create a head var for each class constraint.
    %
:- pred make_typeclass_info_head_vars(record_type_info_locns::in,
    list(prog_constraint)::in, list(prog_var)::out,
    poly_info::in, poly_info::out) is det.

make_typeclass_info_head_vars(RecordLocns, Constraints, ExtraHeadVars,
        !Info) :-
    list.map_foldl(make_typeclass_info_head_var(RecordLocns), Constraints,
        ExtraHeadVars, !Info).

:- pred make_typeclass_info_head_var(record_type_info_locns::in,
    prog_constraint::in, prog_var::out, poly_info::in, poly_info::out) is det.

make_typeclass_info_head_var(RecordLocns, Constraint, TypeClassInfoVar,
        !Info) :-
    (
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_search_typeclass_info_var(RttiVarMaps0, Constraint,
            OldTypeClassInfoVar)
    ->
        TypeClassInfoVar = OldTypeClassInfoVar
    ;
        % Make a new variable to contain the dictionary for this typeclass
        % constraint.
        new_typeclass_info_var(Constraint, typeclass_info_kind,
            TypeClassInfoVar, _TypeClassInfoVarType, !Info),
        (
            RecordLocns = do_record_type_info_locns,
            record_constraint_type_info_locns(Constraint, TypeClassInfoVar,
                !Info)
        ;
            RecordLocns = do_not_record_type_info_locns
        )
    ).

:- pred record_constraint_type_info_locns(prog_constraint::in, prog_var::in,
    poly_info::in, poly_info::out) is det.

record_constraint_type_info_locns(Constraint, ExtraHeadVar, !Info) :-
    poly_info_get_module_info(!.Info, ModuleInfo),

    % Work out how many superclasses the class has.
    Constraint = constraint(ClassName, ClassTypes),
    list.length(ClassTypes, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(ClassTable, ClassId, ClassDefn),
    SuperClasses = ClassDefn ^ class_supers,
    list.length(SuperClasses, NumSuperClasses),

    % Find all the type variables in the constraint, and remember what
    % index they appear in in the typeclass info.

    % The first type_info will be just after the superclass infos.
    First = NumSuperClasses + 1,
    Last = NumSuperClasses + ClassArity,
    assoc_list.from_corresponding_lists(ClassTypes, First `..` Last,
        IndexedClassTypes),

    % Work out which type variables we haven't seen before, or which we
    % assumed earlier would be produced in a type_info (this can happen for
    % code which needs mode reordering and which calls existentially
    % quantified predicates or deconstructs existentially quantified
    % terms).
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    NewTVarAndIndex = (pred(TVarAndIndex::out) is nondet :-
        list.member(Type - Index, IndexedClassTypes),
        type_vars(Type, TypeVars),
        list.member(TypeVar, TypeVars),
        ( rtti_search_type_info_locn(RttiVarMaps0, TypeVar, TypeInfoLocn) ->
            TypeInfoLocn = type_info(_)
        ;
            true
        ),
        TVarAndIndex = TypeVar - Index
    ),
    solutions(NewTVarAndIndex, NewClassTypeVars),

    % Make an entry in the TypeInfo locations map for each new type variable.
    % The type variable can be found at the previously calculated offset
    % with the new typeclass_info.
    MakeEntry =
        (pred(IndexedTypeVar::in, R0::in, R::out) is det :-
            IndexedTypeVar = TheTypeVar - Index,
            Location = typeclass_info(ExtraHeadVar, Index),
            rtti_set_type_info_locn(TheTypeVar, Location, R0, R)
        ),
    list.foldl(MakeEntry, NewClassTypeVars, RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info).

:- type tci_var_kind
    --->    base_typeclass_info_kind
    ;       typeclass_info_kind.

:- pred new_typeclass_info_var(prog_constraint::in, tci_var_kind::in,
    prog_var::out, mer_type::out, poly_info::in, poly_info::out) is det.

new_typeclass_info_var(Constraint, VarKind, Var, VarType, !Info) :-
    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),

    Constraint = constraint(ClassName, _),
    ClassNameString = unqualify_name(ClassName),

    % Introduce new variable.
    varset.new_var(Var, VarSet0, VarSet1),
    (
        VarKind = base_typeclass_info_kind,
        Name = "BaseTypeClassInfo_for_" ++ ClassNameString
    ;
        VarKind = typeclass_info_kind,
        Name = "TypeClassInfo_for_" ++ ClassNameString
    ),
    varset.name_var(Var, Name, VarSet1, VarSet),
    build_typeclass_info_type(Constraint, VarType),
    add_var_type(Var, VarType, VarTypes0, VarTypes),
    rtti_det_insert_typeclass_info_var(Constraint, Var,
        RttiVarMaps0, RttiVarMaps),

    poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info).

build_typeclass_info_type(_Constraint, DictionaryType) :-
    PrivateBuiltin = mercury_private_builtin_module,
    TypeclassInfoTypeName = qualified(PrivateBuiltin, "typeclass_info"),
    DictionaryType = defined_type(TypeclassInfoTypeName, [], kind_star).

%---------------------------------------------------------------------------%

type_is_typeclass_info(TypeClassInfoType) :-
    type_to_ctor(TypeClassInfoType, TypeCtor),
    TypeCtor = type_ctor(qualified(ModuleName, "typeclass_info"), 0),
    ModuleName = mercury_private_builtin_module.

type_is_type_info_or_ctor_type(TypeInfoType) :-
    type_to_ctor_and_args(TypeInfoType, TypeCtor, []),
    TypeCtor = type_ctor(qualified(ModuleName, TypeName), 0),
    ModuleName = mercury_private_builtin_module,
    ( TypeName = "type_info"
    ; TypeName = "type_ctor_info"
    ).

build_type_info_type(Type, TypeInfoType) :-
    % XXX TypeInfoType = type_ctor_info_type.
    ( type_has_variable_arity_ctor(Type, _, _) ->
        % We cannot use a plain type_ctor_info because we need to
        % record the arity.
        TypeInfoType = type_info_type
    ; type_to_ctor_and_args(Type, _Ctor, Args) ->
        (
            Args = [],
            TypeInfoType = type_ctor_info_type
        ;
            Args = [_ | _],
            TypeInfoType = type_info_type
        )
    ;
        % The type is variable, which means we have a type_info for it.
        % That type_info may actually be a type_ctor_info, but the code
        % of the current predicate won't treat it as such.
        TypeInfoType = type_info_type
    ).

%---------------------------------------------------------------------------%

    % Expand the bodies of all class methods. Class methods for imported
    % classes are only expanded if we are performing type specialization,
    % so that method lookups for imported classes can be optimized.
    %
    % The expansion involves inserting a class_method_call with the appropriate
    % arguments, which is responsible for extracting the appropriate part
    % of the dictionary.
    %
:- pred expand_class_method_bodies(module_info::in, module_info::out) is det.

expand_class_method_bodies(!ModuleInfo) :-
    module_info_get_class_table(!.ModuleInfo, Classes),
    module_info_get_name(!.ModuleInfo, ModuleName),
    map.keys(Classes, ClassIds0),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, user_guided_type_specialization,
        TypeSpec),
    (
        TypeSpec = no,
        % Don't expand classes from other modules.
        list.filter(class_id_is_from_given_module(ModuleName),
            ClassIds0, ClassIds)
    ;
        TypeSpec = yes,
        ClassIds = ClassIds0
    ),
    map.apply_to_list(ClassIds, Classes, ClassDefns),
    list.foldl(expand_class_method_bodies_2, ClassDefns, !ModuleInfo).

:- pred class_id_is_from_given_module(module_name::in, class_id::in)
    is semidet.

class_id_is_from_given_module(ModuleName, ClassId) :-
    ClassId = class_id(qualified(ModuleName, _), _).

:- pred expand_class_method_bodies_2(hlds_class_defn::in,
    module_info::in, module_info::out) is det.

expand_class_method_bodies_2(ClassDefn, !ModuleInfo) :-
    Interface = ClassDefn ^ class_hlds_interface,
    list.foldl2(expand_class_method_body, Interface, 1, _, !ModuleInfo).

:- pred expand_class_method_body(hlds_class_proc::in, int::in, int::out,
    module_info::in, module_info::out) is det.

expand_class_method_body(hlds_class_proc(PredId, ProcId), !ProcNum,
        !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    % Find which of the constraints on the pred is the one introduced
    % because it is a class method.
    pred_info_get_class_context(PredInfo0, ClassContext),
    ( ClassContext = constraints([Head | _], _) ->
        InstanceConstraint = Head
    ;
        unexpected($module, $pred, "class method is not constrained")
    ),

    proc_info_get_rtti_varmaps(ProcInfo0, RttiVarMaps),
    rtti_lookup_typeclass_info_var(RttiVarMaps, InstanceConstraint,
        TypeClassInfoVar),

    proc_info_get_headvars(ProcInfo0, HeadVars0),
    proc_info_get_argmodes(ProcInfo0, Modes0),
    proc_info_get_declared_determinism(ProcInfo0, MaybeDetism0),
    (
        MaybeDetism0 = yes(Detism)
    ;
        MaybeDetism0 = no,
        % Omitting the determinism for a method is not allowed. But make_hlds
        % will have already detected and reported the error. So here we can
        % just pick some value at random; hopefully something that won't cause
        % flow-on errors. We also mark the predicate as invalid, also to avoid
        % flow-on errors.
        Detism = detism_non,
        module_info_remove_predid(PredId, !ModuleInfo)
    ),

    % Work out which argument corresponds to the constraint which is introduced
    % because this is a class method, then delete it from the list of args to
    % the class_method_call. That variable becomes the "dictionary" variable
    % for the class_method_call. (cf. the closure for a higher order call).
    (
        list.nth_member_search(HeadVars0, TypeClassInfoVar, N),
        delete_nth(HeadVars0, N, HeadVarsPrime),
        delete_nth(Modes0, N, ModesPrime)
    ->
        HeadVars = HeadVarsPrime,
        Modes = ModesPrime
    ;
        unexpected($module, $pred, "typeclass_info var not found")
    ),

    InstanceConstraint = constraint(ClassName, InstanceArgs),
    list.length(InstanceArgs, InstanceArity),
    pred_info_get_call_id(PredInfo0, CallId),
    BodyGoalExpr = generic_call(
        class_method(TypeClassInfoVar, !.ProcNum,
            class_id(ClassName, InstanceArity), CallId),
        HeadVars, Modes, arg_reg_types_unset, Detism),

    % Make the goal info for the call.
    set_of_var.list_to_set(HeadVars0, NonLocals),
    instmap_delta_from_mode_list(HeadVars0, Modes0, !.ModuleInfo,
        InstmapDelta),
    pred_info_get_purity(PredInfo0, Purity),
    goal_info_init(NonLocals, InstmapDelta, Detism, Purity, GoalInfo),
    BodyGoal = hlds_goal(BodyGoalExpr, GoalInfo),

    proc_info_set_goal(BodyGoal, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_procedures(ProcTable, PredInfo0, PredInfo1),
    ( pred_info_is_imported(PredInfo1) ->
        pred_info_set_import_status(status_opt_imported, PredInfo1, PredInfo)
    ;
        PredInfo = PredInfo1
    ),

    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),

    !:ProcNum = !.ProcNum + 1.

:- pred delete_nth(list(T)::in, int::in, list(T)::out) is semidet.

delete_nth([X | Xs], N0, Result) :-
    ( N0 > 1 ->
        N = N0 - 1,
        delete_nth(Xs, N, TheRest),
        Result = [X | TheRest]
    ;
        Result = Xs
    ).

%---------------------------------------------------------------------------%

:- func get_constrained_vars(prog_constraint) = list(tvar).

get_constrained_vars(Constraint) = CVars :-
    Constraint = constraint(_, CTypes),
    type_vars_list(CTypes, CVars).

%---------------------------------------------------------------------------%

:- pred all_are_const_struct_args(
    assoc_list(prog_var, maybe(const_struct_arg))::in,
    list(const_struct_arg)::out) is semidet.

all_are_const_struct_args([], []).
all_are_const_struct_args([VarMCA | VarsMCAs], [ConstArg | ConstArgs]) :-
    VarMCA = _Var - MCA,
    MCA = yes(ConstArg),
    all_are_const_struct_args(VarsMCAs, ConstArgs).

:- pred get_inst_of_const_struct_arg(const_struct_db::in, const_struct_arg::in,
    mer_inst::out)
    is det.

get_inst_of_const_struct_arg(ConstStructDb, ConstArg, Inst) :-
    (
        ConstArg = csa_constant(ConsId, _),
        Inst = bound(shared, inst_test_results_fgtc,
            [bound_functor(ConsId, [])])
    ;
        ConstArg = csa_const_struct(StructNum),
        lookup_const_struct_num(ConstStructDb, StructNum, Struct),
        Struct = const_struct(_, _, _, Inst)
    ).

%---------------------------------------------------------------------------%

:- pred materialize_base_typeclass_info_var(prog_constraint::in, cons_id::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

materialize_base_typeclass_info_var(Constraint, ConsId, Var, Goals, !Info) :-
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap0),
    build_typeclass_info_type(Constraint, ConstArgType),
    ConstArg = csa_constant(ConsId, ConstArgType),
    ( map.search(ConstStructVarMap0, ConstArg, OldVar) ->
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        Var = OldVar,
        Goals = []
    ;
        new_typeclass_info_var(Constraint, base_typeclass_info_kind, Var,
            _VarType, !Info),

        % Create the construction unification to initialize the variable.
        RHS = rhs_functor(ConsId, no, []),
        Unification = construct(Var, ConsId, [], [],
            construct_dynamically, cell_is_shared, no_construct_sub_info),
        UnifyMode = (free -> ground(shared, none)) -
            (ground(shared, none) -> ground(shared, none)),
        UnifyContext = unify_context(umc_explicit, []),
        % XXX The UnifyContext is wrong.
        Unify = unify(Var, RHS, UnifyMode, Unification, UnifyContext),

        % Create the unification goal.
        NonLocals = set_of_var.make_singleton(Var),
        InstmapDelta = instmap_delta_bind_var(Var),
        goal_info_init(NonLocals, InstmapDelta, detism_det, purity_pure,
            GoalInfo),
        Goal = hlds_goal(Unify, GoalInfo),
        Goals = [Goal]
    ).

:- pred materialize_typeclass_info_var(prog_constraint::in, int::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

materialize_typeclass_info_var(Constraint, InstanceIdConstNum, Var, Goals,
        !Info) :-
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap0),
    InstanceIdConstArg = csa_const_struct(InstanceIdConstNum),
    ( map.search(ConstStructVarMap0, InstanceIdConstArg, OldVar) ->
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        Var = OldVar,
        Goals = []
    ;
        new_typeclass_info_var(Constraint, typeclass_info_kind, Var, _VarType,
            !Info),
        map.det_insert(InstanceIdConstArg, Var,
            ConstStructVarMap0, ConstStructVarMap),
        poly_info_set_const_struct_var_map(ConstStructVarMap, !Info),

        % Create the construction unification to initialize the variable.
        ConsId = typeclass_info_const(InstanceIdConstNum),
        RHS = rhs_functor(ConsId, no, []),
        Unification = construct(Var, ConsId, [], [],
            construct_statically, cell_is_shared, no_construct_sub_info),
        UnifyMode = (free -> ground(shared, none)) -
            (ground(shared, none) -> ground(shared, none)),
        UnifyContext = unify_context(umc_explicit, []),
        % XXX The UnifyContext is wrong.
        GoalExpr = unify(Var, RHS, UnifyMode, Unification, UnifyContext),

        % Create a goal_info for the unification.
        NonLocals = set_of_var.make_singleton(Var),
        InstmapDelta = instmap_delta_bind_var(Var),
        goal_info_init(NonLocals, InstmapDelta, detism_det, purity_pure,
            GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Goals = [Goal]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type const_or_var_arg
    --->    cova_const(const_struct_arg)
    ;       cova_var(prog_var).

:- pred make_const_or_var_arg(pair(prog_var, maybe(const_struct_arg))::in,
    const_or_var_arg::out) is det.

make_const_or_var_arg(Var - MCA, ConstOrVarArg) :-
    (
        MCA = no,
        ConstOrVarArg = cova_var(Var)
    ;
        MCA = yes(ConstArg),
        ConstOrVarArg = cova_const(ConstArg)
    ).

:- type type_info_var_map ==
    map(type_ctor,
        map(list(mer_type), pair(prog_var, maybe(const_struct_arg)))).

:- type typeclass_info_map_entry
    --->    typeclass_info_map_entry(
                % The cons_id representing the base_typeclass_info.
                cons_id,

                % Maps the arguments of the typeclass_info_cell_constructor
                % after the base_typeclass_info to the variable that holds the
                % typeclass_info for that cell.
                map(list(const_or_var_arg),
                    pair(prog_var, maybe(const_struct_arg)))
            ).

:- type typeclass_info_map ==
    map(class_name, map(list(mer_type), typeclass_info_map_entry)).

:- type int_const_map == map(int, prog_var).

    % If the value that can be a constant structure argument is currently
    % available in a variable, give the id of that variable.
    %
:- type const_struct_var_map == map(const_struct_arg, prog_var).

:- type poly_info
    --->    poly_info(
                % The first two fields are from the proc_info.
                poly_varset                 :: prog_varset,
                poly_vartypes               :: vartypes,

                % The next two fields from the pred_info.
                poly_typevarset             :: tvarset,
                poly_tvar_kinds             :: tvar_kind_map,

                % Gives information about the locations of type_infos
                % and typeclass_infos.
                poly_rtti_varmaps           :: rtti_varmaps,

                % Specifies why each constraint that was eliminated from the
                % pred was able to be eliminated (this allows us to efficiently
                % construct the dictionary).
                % Note that the rtti_varmaps is separate from the
                % constraint_proof_map, since the second is the information
                % calculated by typecheck.m, while the first is the information
                % calculated here in polymorphism.m.
                poly_proof_map              :: constraint_proof_map,

                % Specifies the constraints at each location in the goal.
                poly_constraint_map         :: constraint_map,

                % The next four maps hold information about what
                % type_ctor_infos, type_infos, base_typeclass_infos,
                % typeclass_infos and ints are guaranteed to be available
                % (i.e. created by previous code on all execution paths)
                % at the current point in the code, so they can be reused.
                % The fifth field counts the number of times that one of these
                % variables has in fact been reused.
                %
                % The type_infos and typeclass_infos are in the first two maps.
                % The type_ctor_infos and base_typeclass_infos are in the
                % fourth map. The integers are in the third map.
                % The fourth map also caches typeclass_infos for instance ids.
                poly_type_info_var_map      :: type_info_var_map,
                poly_typeclass_info_map     :: typeclass_info_map,
                poly_int_const_map          :: int_const_map,
                poly_const_struct_var_map   :: const_struct_var_map,
                poly_num_reuses             :: int,

                poly_snapshot_num           :: int,

                % The database of constant structures of the module.
                % If a type_info or typeclass_info we construct is a constant
                % term, we allocate it in this database.
                poly_const_struct_db        :: const_struct_db,

                poly_pred_info              :: pred_info,
                poly_module_info            :: module_info
            ).

%---------------------------------------------------------------------------%

    % Init_poly_info initializes a poly_info from a pred_info and clauses_info.
    % (See also create_poly_info.)
    %
:- pred init_poly_info(module_info::in, pred_info::in, clauses_info::in,
    poly_info::out) is det.

init_poly_info(ModuleInfo, PredInfo, ClausesInfo, PolyInfo) :-
    clauses_info_get_varset(ClausesInfo, VarSet),
    clauses_info_get_vartypes(ClausesInfo, VarTypes),
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    pred_info_get_tvar_kinds(PredInfo, TypeVarKinds),
    pred_info_get_constraint_proofs(PredInfo, Proofs),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    rtti_varmaps_init(RttiVarMaps),
    map.init(TypeInfoVarMap),
    map.init(TypeClassInfoMap),
    map.init(IntConstMap),
    map.init(ConstStructVarMap),
    NumReuses = 0,
    SnapshotNum = 0,
    module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
    PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet, TypeVarKinds,
        RttiVarMaps, Proofs, ConstraintMap,
        TypeInfoVarMap, TypeClassInfoMap, IntConstMap, ConstStructVarMap,
        NumReuses, SnapshotNum, ConstStructDb, PredInfo, ModuleInfo).

    % Create_poly_info creates a poly_info for an existing procedure.
    % (See also init_poly_info.)
    %
create_poly_info(ModuleInfo, PredInfo, ProcInfo, PolyInfo) :-
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    pred_info_get_tvar_kinds(PredInfo, TypeVarKinds),
    pred_info_get_constraint_proofs(PredInfo, Proofs),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    map.init(TypeInfoVarMap),
    map.init(TypeClassInfoMap),
    map.init(IntConstMap),
    map.init(ConstStructVarMap),
    NumReuses = 0,
    SnapshotNum = 0,
    module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
    PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet, TypeVarKinds,
        RttiVarMaps, Proofs, ConstraintMap, TypeInfoVarMap,
        TypeClassInfoMap, IntConstMap, ConstStructVarMap,
        NumReuses, SnapshotNum, ConstStructDb, PredInfo, ModuleInfo).

poly_info_extract(Info, !PredInfo, !ProcInfo, !:ModuleInfo) :-
    Info = poly_info(VarSet, VarTypes, TypeVarSet, TypeVarKinds,
        RttiVarMaps, _Proofs, _ConstraintMap,
        _TypeInfoVarMap, _TypeClassInfoMap, _IntConstMap, _ConstStructVarMap,
        _NumReuses, _SnapshotNum, ConstStructDb, _OldPredInfo, !:ModuleInfo),

    module_info_set_const_struct_db(ConstStructDb, !ModuleInfo),

    % Set the new values of the fields in proc_info and pred_info.
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
    pred_info_set_typevarset(TypeVarSet, !PredInfo),
    pred_info_set_tvar_kinds(TypeVarKinds, !PredInfo).

%---------------------------------------------------------------------------%

:- pred poly_info_get_varset(poly_info::in, prog_varset::out) is det.
:- pred poly_info_get_var_types(poly_info::in, vartypes::out) is det.
:- pred poly_info_get_typevarset(poly_info::in, tvarset::out) is det.
:- pred poly_info_get_tvar_kinds(poly_info::in, tvar_kind_map::out) is det.
:- pred poly_info_get_rtti_varmaps(poly_info::in, rtti_varmaps::out) is det.
:- pred poly_info_get_proofs(poly_info::in, constraint_proof_map::out) is det.
:- pred poly_info_get_constraint_map(poly_info::in, constraint_map::out)
    is det.
:- pred poly_info_get_type_info_var_map(poly_info::in,
    type_info_var_map::out) is det.
:- pred poly_info_get_typeclass_info_map(poly_info::in,
    typeclass_info_map::out) is det.
:- pred poly_info_get_int_const_map(poly_info::in, int_const_map::out) is det.
:- pred poly_info_get_num_reuses(poly_info::in, int::out) is det.
:- pred poly_info_get_const_struct_db(poly_info::in, const_struct_db::out)
    is det.
:- pred poly_info_get_const_struct_var_map(poly_info::in,
    const_struct_var_map::out) is det.
:- pred poly_info_get_pred_info(poly_info::in, pred_info::out) is det.
:- pred poly_info_get_module_info(poly_info::in, module_info::out) is det.

poly_info_get_varset(PolyInfo, PolyInfo ^ poly_varset).
poly_info_get_var_types(PolyInfo, PolyInfo ^ poly_vartypes).
poly_info_get_typevarset(PolyInfo, PolyInfo ^ poly_typevarset).
poly_info_get_tvar_kinds(PolyInfo, PolyInfo ^ poly_tvar_kinds).
poly_info_get_rtti_varmaps(PolyInfo, PolyInfo ^ poly_rtti_varmaps).
poly_info_get_proofs(PolyInfo, PolyInfo ^ poly_proof_map).
poly_info_get_constraint_map(PolyInfo, PolyInfo ^ poly_constraint_map).
poly_info_get_type_info_var_map(PolyInfo, PolyInfo ^ poly_type_info_var_map).
poly_info_get_typeclass_info_map(PolyInfo, PolyInfo ^ poly_typeclass_info_map).
poly_info_get_int_const_map(PolyInfo, PolyInfo ^ poly_int_const_map).
poly_info_get_num_reuses(PolyInfo, PolyInfo ^ poly_num_reuses).
poly_info_get_const_struct_db(PolyInfo, PolyInfo ^ poly_const_struct_db).
poly_info_get_const_struct_var_map(PolyInfo,
    PolyInfo ^ poly_const_struct_var_map).
poly_info_get_pred_info(PolyInfo, PolyInfo ^ poly_pred_info).
poly_info_get_module_info(PolyInfo, PolyInfo ^ poly_module_info).

:- pred poly_info_set_varset(prog_varset::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_varset_and_types(prog_varset::in, vartypes::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_typevarset(tvarset::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_tvar_kinds(tvar_kind_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_rtti_varmaps(rtti_varmaps::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_proofs(constraint_proof_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_type_info_var_map(type_info_var_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_typeclass_info_map(typeclass_info_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_int_const_map(int_const_map::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_num_reuses(int::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_const_struct_db(const_struct_db::in,
    poly_info::in, poly_info::out) is det.
:- pred poly_info_set_const_struct_var_map(const_struct_var_map::in,
    poly_info::in, poly_info::out) is det.

poly_info_set_varset(VarSet, !PI) :-
    !PI ^ poly_varset := VarSet.
poly_info_set_varset_and_types(VarSet, VarTypes, !PI) :-
    !PI ^ poly_varset := VarSet,
    !PI ^ poly_vartypes := VarTypes.
poly_info_set_typevarset(TVarSet, !PI) :-
    !PI ^ poly_typevarset := TVarSet.
poly_info_set_tvar_kinds(TVarKinds, !PI) :-
    !PI ^ poly_tvar_kinds := TVarKinds.
poly_info_set_rtti_varmaps(RttiVarMaps, !PI) :-
    !PI ^ poly_rtti_varmaps := RttiVarMaps.
poly_info_set_proofs(Proofs, !PI) :-
    !PI ^ poly_proof_map := Proofs.
poly_info_set_type_info_var_map(TypeInfoVarMap, !PI) :-
    !PI ^ poly_type_info_var_map := TypeInfoVarMap.
poly_info_set_typeclass_info_map(TypeClassInfoMap, !PI) :-
    !PI ^ poly_typeclass_info_map := TypeClassInfoMap.
poly_info_set_int_const_map(IntConstMap, !PI) :-
    !PI ^ poly_int_const_map := IntConstMap.
poly_info_set_num_reuses(NumReuses, !PI) :-
    !PI ^ poly_num_reuses := NumReuses.
poly_info_set_const_struct_db(ConstStructDb, !PI) :-
    !PI ^ poly_const_struct_db := ConstStructDb.
poly_info_set_const_struct_var_map(ConstStructVarMap, !PI) :-
    !PI ^ poly_const_struct_var_map := ConstStructVarMap.

%---------------------------------------------------------------------------%

:- type cache_maps
    --->    cache_maps(
                cm_snapshot_num             :: int,
                cm_type_info_var_map        :: type_info_var_map,
                cm_typeclass_info_map       :: typeclass_info_map,
                cm_int_const_map            :: int_const_map,
                cm_const_struct_var_map     :: const_struct_var_map
            ).

:- pred get_cache_maps_snapshot(string::in, cache_maps::out,
    poly_info::in, poly_info::out) is det.
:- pred set_cache_maps_snapshot(string::in, cache_maps::in,
    poly_info::in, poly_info::out) is det.
:- pred empty_cache_maps(poly_info::in, poly_info::out) is det.

get_cache_maps_snapshot(Name, CacheMaps, !Info) :-
    SnapshotNum = !.Info ^ poly_snapshot_num,
    TypeInfoVarMap = !.Info ^ poly_type_info_var_map,
    TypeClassInfoMap = !.Info ^ poly_typeclass_info_map,
    IntConstMap = !.Info ^ poly_int_const_map,
    ConstStructVarMap = !.Info ^ poly_const_struct_var_map,
    CacheMaps = cache_maps(SnapshotNum, TypeInfoVarMap, TypeClassInfoMap,
        IntConstMap, ConstStructVarMap),
    !Info ^ poly_snapshot_num := SnapshotNum + 1,

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        some [SelectedPred, Level, IndentStr] (
            promise_pure (
                semipure get_selected_pred(SelectedPred),
                semipure get_level(Level),
                (
                    SelectedPred = yes,
                    Name \= ""
                ->
                    IndentStr = string.duplicate_char(' ', Level * 4),
                    io.write_string(IndentStr, !IO),
                    io.format("get_cache_maps_snapshot %d %s\n",
                        [i(SnapshotNum), s(Name)], !IO),
                    io.write_string(IndentStr, !IO),
                    NumVars = varset.num_allocated(!.Info ^ poly_varset),
                    io.format("num_allocated vars: %d\n\n", [i(NumVars)], !IO)
                ;
                    true
                )
            )
        )
    ).

set_cache_maps_snapshot(Name, CacheMaps, !Info) :-
    CacheMaps = cache_maps(SnapshotNum, TypeInfoVarMap, TypeClassInfoMap,
        IntConstMap, ConstStructVarMap),
    !Info ^ poly_type_info_var_map := TypeInfoVarMap,
    !Info ^ poly_typeclass_info_map := TypeClassInfoMap,
    !Info ^ poly_int_const_map := IntConstMap,
    !Info ^ poly_const_struct_var_map := ConstStructVarMap,

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        some [SelectedPred, Level, IndentStr] (
            promise_pure (
                semipure get_selected_pred(SelectedPred),
                semipure get_level(Level),
                (
                    SelectedPred = yes,
                    Name \= ""
                ->
                    IndentStr = string.duplicate_char(' ', Level * 4),
                    io.write_string(IndentStr, !IO),
                    io.format("set_cache_maps_snapshot %d %s\n",
                        [i(SnapshotNum), s(Name)], !IO),
                    io.write_string(IndentStr, !IO),
                    NumVars = varset.num_allocated(!.Info ^ poly_varset),
                    io.format("num_allocated vars: %d\n\n", [i(NumVars)], !IO),

                    io.write_string(IndentStr, !IO),
                    io.write_string("type_info_var_map ", !IO),
                    io.write(CacheMaps ^ cm_type_info_var_map, !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("typeclass_info_map ", !IO),
                    io.write(CacheMaps ^ cm_typeclass_info_map, !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("struct_var_map ", !IO),
                    io.write(CacheMaps ^ cm_const_struct_var_map, !IO),
                    io.nl(!IO),
                    io.nl(!IO)
                ;
                    true
                )
            )
        )
    ).

empty_cache_maps(!Info) :-
    !Info ^ poly_type_info_var_map := map.init,
    !Info ^ poly_typeclass_info_map := map.init,
    !Info ^ poly_int_const_map := map.init,
    !Info ^ poly_const_struct_var_map := map.init.

%---------------------------------------------------------------------------%

:- type var_maps
    --->    var_maps(
                vm_snapshot_num             :: int,
                vm_varset                   :: prog_varset,
                vm_vartypes                 :: vartypes,
                vm_rtti_varmaps             :: rtti_varmaps,
                vm_cache_maps               :: cache_maps
            ).

:- pred get_var_maps_snapshot(string::in, var_maps::out,
    poly_info::in, poly_info::out) is det.

get_var_maps_snapshot(Name, VarMaps, !Info) :-
    SnapshotNum = !.Info ^ poly_snapshot_num,
    VarSet = !.Info ^ poly_varset,
    VarTypes = !.Info ^ poly_vartypes,
    RttiVarMaps = !.Info ^ poly_rtti_varmaps,

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        some [SelectedPred, Level, IndentStr] (
            promise_pure (
                semipure get_selected_pred(SelectedPred),
                semipure get_level(Level),
                (
                    SelectedPred = no
                ;
                    SelectedPred = yes,
                    IndentStr = string.duplicate_char(' ', Level * 4),
                    io.write_string(IndentStr, !IO),
                    io.format("get_var_maps_snapshot %d %s\n",
                        [i(SnapshotNum), s(Name)], !IO),
                    io.write_string(IndentStr, !IO),
                    NumVars = varset.num_allocated(VarSet),
                    io.format("num_allocated vars: %d\n\n", [i(NumVars)], !IO)
                )
            )
        )
    ),

    get_cache_maps_snapshot("", CacheMaps, !Info),
    VarMaps = var_maps(SnapshotNum, VarSet, VarTypes, RttiVarMaps, CacheMaps).

:- pred set_var_maps_snapshot(string::in, var_maps::in,
    poly_info::in, poly_info::out) is det.

set_var_maps_snapshot(Name, VarMaps, !Info) :-
    VarMaps = var_maps(SnapshotNum, VarSet, VarTypes, RttiVarMaps, CacheMaps),

    trace [compiletime(flag("debug_poly_caches")), io(!IO)] (
        some [SelectedPred, Level, IndentStr] (
            promise_pure (
                semipure get_selected_pred(SelectedPred),
                semipure get_level(Level),
                (
                    SelectedPred = no
                ;
                    SelectedPred = yes,
                    IndentStr = string.duplicate_char(' ', Level * 4),
                    io.write_string(IndentStr, !IO),
                    io.format("set_var_maps_snapshot %d %s\n",
                        [i(SnapshotNum), s(Name)], !IO),

                    io.write_string(IndentStr, !IO),
                    io.write_string("type_info_var_map ", !IO),
                    io.write(CacheMaps ^ cm_type_info_var_map, !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("typeclass_info_map ", !IO),
                    io.write(CacheMaps ^ cm_typeclass_info_map, !IO),
                    io.nl(!IO),
                    io.write_string(IndentStr, !IO),
                    io.write_string("struct_var_map ", !IO),
                    io.write(CacheMaps ^ cm_const_struct_var_map, !IO),
                    io.nl(!IO),
                    io.nl(!IO)
                )
            )
        )
    ),

    !Info ^ poly_varset := VarSet,
    !Info ^ poly_vartypes := VarTypes,
    !Info ^ poly_rtti_varmaps := RttiVarMaps,
    set_cache_maps_snapshot("", CacheMaps, !Info).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism.
%---------------------------------------------------------------------------%
