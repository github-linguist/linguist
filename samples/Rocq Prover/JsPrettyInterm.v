Set Implicit Arguments.
Require Export JsSyntax JsSyntaxAux JsPreliminary.

(**************************************************************)
(** ** Implicit Types -- copied from JsPreliminary  *)

Implicit Type b : bool.
Implicit Type n : number.
Implicit Type k : int.
Implicit Type s : string.
Implicit Type i : literal.
Implicit Type l : object_loc.
Implicit Type w : prim.
Implicit Type v : value.
Implicit Type r : ref.
(*Implicit Type B : builtin.*)
Implicit Type ty : type.

Implicit Type rt : restype.
Implicit Type rv : resvalue.
Implicit Type lab : label.
Implicit Type labs : label_set.
Implicit Type R : res.
Implicit Type o : out.

Implicit Type x : prop_name.
Implicit Type str : strictness_flag.
Implicit Type m : mutability.
Implicit Type Ad : attributes_data.
Implicit Type Aa : attributes_accessor.
Implicit Type A : attributes.
Implicit Type Desc : descriptor.
Implicit Type D : full_descriptor.

Implicit Type L : env_loc.
Implicit Type E : env_record.
Implicit Type Ed : decl_env_record.
Implicit Type X : lexical_env.
Implicit Type O : object.
Implicit Type S : state.
Implicit Type C : execution_ctx.
Implicit Type P : object_properties_type.

Implicit Type e : expr.
Implicit Type p : prog.
Implicit Type t : stat.


(****************************************************************)
(** ** Intermediate expression for the Pretty-Big-Step semantic *)

(** Grammar of extended expressions *)

Inductive ext_expr :=

  (** Extended expressions include expressions *)

  | expr_basic : expr -> ext_expr

  (** Extended expressions associated with primitive expressions *)

  | expr_identifier_1 : specret ref -> ext_expr

  | expr_object_0 : out -> propdefs -> ext_expr
  | expr_object_1 : object_loc -> propdefs -> ext_expr
  | expr_object_2 : object_loc -> string -> propbody -> propdefs -> ext_expr 
  | expr_object_3_val : object_loc -> string -> specret value -> propdefs -> ext_expr
  | expr_object_3_get : object_loc -> string -> out -> propdefs -> ext_expr
  | expr_object_3_set : object_loc -> string -> out -> propdefs -> ext_expr
  | expr_object_4 : object_loc -> string -> descriptor -> propdefs -> ext_expr
  | expr_object_5 : object_loc -> propdefs -> out -> ext_expr





  (* _ARRAYS_ : support for array intermediate forms *)
  | expr_array_0 : out -> list (option expr) -> ext_expr
  | expr_array_1 : object_loc -> list (option expr) -> ext_expr
  | expr_array_2 : object_loc -> list (option expr) -> int -> ext_expr
  | expr_array_3 : object_loc -> list (option expr) -> int -> ext_expr
  | expr_array_3_1 : object_loc -> specret value -> list (option expr) -> int -> ext_expr
  | expr_array_3_2 : object_loc -> value -> out -> list (option expr) -> int -> ext_expr
  | expr_array_3_3 : object_loc -> value -> specret int -> list (option expr) -> int -> ext_expr
  | expr_array_3_4 : object_loc -> value -> out -> list (option expr) -> ext_expr
  | expr_array_3_5 : object_loc -> out -> list (option expr) -> ext_expr

  | expr_array_add_length   : object_loc -> int -> out -> ext_expr
  | expr_array_add_length_0 : object_loc -> int -> ext_expr
  | expr_array_add_length_1 : object_loc -> int -> out -> ext_expr
  | expr_array_add_length_2 : object_loc -> specret int -> int -> ext_expr
  | expr_array_add_length_3 : object_loc -> specret int -> ext_expr
  | expr_array_add_length_4 : object_loc -> out -> ext_expr





  | expr_function_1 : string -> list string -> funcbody -> env_loc -> lexical_env -> out -> ext_expr
  | expr_function_2 : string -> env_loc -> out -> ext_expr
  | expr_function_3 : object_loc -> out -> ext_expr

  | expr_access_1 : specret value -> expr -> ext_expr (* The left expression has been executed *)
  | expr_access_2 : value -> specret value -> ext_expr (* The right expression is executed. *)
  | expr_access_3 : value -> out -> value -> ext_expr
  | expr_access_4 : value -> out -> ext_expr

  | expr_new_1 : specret value -> list expr -> ext_expr (* The arguments too. *)
  | expr_new_2 : value -> specret (list value) -> ext_expr (* The call has been executed. *)

  | expr_call_1 : out -> bool -> list expr -> ext_expr
  | expr_call_2 : res -> bool -> list expr -> specret value -> ext_expr (* The function has been evaluated. *)
  | expr_call_3 : res -> value -> bool -> specret (list value) -> ext_expr (* The arguments have been executed. *)
  | expr_call_4 : res -> object_loc -> bool -> list value -> ext_expr
  | expr_call_5 : object_loc -> bool -> list value -> out -> ext_expr (* The call has been executed. *)

  | spec_eval : bool -> value -> list value -> ext_expr

  | expr_unary_op_1 : unary_op -> (specret value) -> ext_expr (* The argument have been executed. *)
  | expr_unary_op_2 : unary_op -> value -> ext_expr (* The argument is a value. *)
  | expr_delete_1 : out -> ext_expr
  | expr_delete_2 : ref -> ext_expr
  | expr_delete_3 : ref -> out -> ext_expr
  | expr_delete_4 : ref -> env_loc -> ext_expr
  | expr_typeof_1 : out -> ext_expr
  | expr_typeof_2 : specret value -> ext_expr
  | expr_prepost_1 : unary_op -> out -> ext_expr
  | expr_prepost_2 : unary_op -> res -> specret value -> ext_expr
  | expr_prepost_3 : unary_op -> res -> out -> ext_expr
  | expr_prepost_4 : value -> out -> ext_expr
  | expr_unary_op_neg_1 : out -> ext_expr
  | expr_unary_op_bitwise_not_1 : specret int -> ext_expr
  | expr_unary_op_not_1 : out -> ext_expr
  | expr_conditional_1 : specret value -> expr -> expr -> ext_expr
  | expr_conditional_1': out -> expr -> expr -> ext_expr
  | expr_conditional_2 : specret value -> ext_expr

  | expr_binary_op_1 : binary_op -> (specret value) -> expr -> ext_expr
  | expr_binary_op_2 : binary_op -> value -> (specret value) -> ext_expr
  | expr_binary_op_3 : binary_op -> value -> value -> ext_expr
  | expr_binary_op_add_1 : specret (value*value) -> ext_expr
  | expr_binary_op_add_string_1 : specret (value*value) -> ext_expr
  | expr_puremath_op_1 : (number -> number -> number) -> specret (value*value) -> ext_expr
  | expr_shift_op_1 : (int -> int -> int) -> specret int -> value -> ext_expr
  | expr_shift_op_2 : (int -> int -> int) -> int -> specret int -> ext_expr
  | expr_inequality_op_1 : bool -> bool -> value -> value -> ext_expr
  | expr_inequality_op_2 : bool -> bool -> specret (value*value) -> ext_expr
  | expr_binary_op_in_1 : object_loc -> out -> ext_expr
  | expr_binary_op_disequal_1 : out -> ext_expr
  | spec_equal : value -> value -> ext_expr
  | spec_equal_1 : type -> type -> value -> value -> ext_expr
  | spec_equal_2 : bool -> ext_expr
  | spec_equal_3 : value -> (value -> ext_expr) -> value -> ext_expr
  | spec_equal_4 : value -> out -> ext_expr
  | expr_bitwise_op_1 : (int -> int -> int) -> specret int -> value -> ext_expr
  | expr_bitwise_op_2 : (int -> int -> int) -> int -> specret int -> ext_expr
  | expr_lazy_op_1 : bool -> (specret value) -> expr -> ext_expr
  | expr_lazy_op_2 : bool -> value -> out -> expr -> ext_expr
  | expr_lazy_op_2_1 : (specret value) -> ext_expr

  | expr_assign_1 : out -> option binary_op -> expr -> ext_expr
  | expr_assign_2 : res -> (specret value) -> binary_op -> expr -> ext_expr
  | expr_assign_3 : res -> value -> binary_op -> (specret value) -> ext_expr
  | expr_assign_3' : res -> out -> ext_expr
  | expr_assign_4 : res -> (specret value) -> ext_expr
  | expr_assign_5 : value -> out -> ext_expr

  (** Extended expressions for conversions *)

  | spec_to_primitive : value -> option preftype -> ext_expr
  | spec_to_boolean : value -> ext_expr
  | spec_to_number : value -> ext_expr
  | spec_to_number_1 : out -> ext_expr
  | spec_to_integer : value -> ext_expr
  | spec_to_integer_1 : out -> ext_expr
  | spec_to_string : value -> ext_expr
  | spec_to_string_1 : out -> ext_expr
  | spec_to_object : value -> ext_expr
  | spec_check_object_coercible : value -> ext_expr

  (** Extended expressions for comparison *)

  | spec_eq : value -> value -> ext_expr
  | spec_eq0 : value -> value -> ext_expr
  | spec_eq1 : value -> value -> ext_expr
  | spec_eq2 : ext_expr -> value -> value -> ext_expr

  (** Extended expressions for operations on objects *)

  | spec_object_get : object_loc -> prop_name -> ext_expr
  | spec_object_get_1 : builtin_get -> value -> object_loc -> prop_name -> ext_expr
  | spec_object_get_2 : value -> specret full_descriptor -> ext_expr
  | spec_object_get_3 : value -> value -> ext_expr

  | spec_object_can_put : object_loc -> prop_name -> ext_expr
  | spec_object_can_put_1 : builtin_can_put -> object_loc -> prop_name -> ext_expr
  | spec_object_can_put_2 : object_loc -> prop_name -> (specret full_descriptor) -> ext_expr

  (* LATER: shift names since spec_object_can_put_3 is not used *)
  | spec_object_can_put_4 : object_loc -> prop_name -> value -> ext_expr
  | spec_object_can_put_5 : object_loc -> specret full_descriptor -> ext_expr
  | spec_object_can_put_6 : attributes_data -> bool -> ext_expr

  | spec_object_put : object_loc -> prop_name -> value -> bool -> ext_expr
  | spec_object_put_1 : builtin_put -> value -> object_loc -> prop_name -> value -> bool -> ext_expr
  | spec_object_put_2 : value -> object_loc -> prop_name -> value -> bool -> out -> ext_expr
  | spec_object_put_3 : value -> object_loc -> prop_name -> value -> bool -> specret full_descriptor -> ext_expr
  | spec_object_put_4 : value -> object_loc -> prop_name -> value -> bool -> specret full_descriptor -> ext_expr
  | spec_object_put_5 : out -> ext_expr

  | spec_object_has_prop : object_loc -> prop_name -> ext_expr
  | spec_object_has_prop_1 : builtin_has_prop -> object_loc -> prop_name -> ext_expr
  | spec_object_has_prop_2 : specret full_descriptor -> ext_expr

  | spec_object_delete : object_loc -> prop_name -> bool -> ext_expr
  | spec_object_delete_1 : builtin_delete -> object_loc -> prop_name -> bool -> ext_expr
  | spec_object_delete_2 : object_loc -> prop_name -> bool -> (specret full_descriptor) -> ext_expr
  | spec_object_delete_3 : object_loc -> prop_name -> bool -> bool -> ext_expr

  | spec_object_default_value : object_loc -> option preftype -> ext_expr
  | spec_object_default_value_1 : builtin_default_value -> object_loc -> option preftype -> ext_expr
  | spec_object_default_value_2 : object_loc -> preftype -> preftype -> ext_expr
  | spec_object_default_value_3 : object_loc -> preftype -> ext_expr
  | spec_object_default_value_4 : ext_expr
  | spec_object_default_value_sub_1 : object_loc -> string -> ext_expr -> ext_expr
  | spec_object_default_value_sub_2 : object_loc -> out -> ext_expr -> ext_expr
  | spec_object_default_value_sub_3 : out -> ext_expr -> ext_expr

  | spec_object_define_own_prop : object_loc -> prop_name -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_1 : builtin_define_own_prop -> object_loc -> prop_name -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_2 : object_loc -> prop_name -> descriptor -> bool -> (specret full_descriptor) -> ext_expr
  | spec_object_define_own_prop_3 : object_loc -> prop_name -> descriptor -> bool -> full_descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_4 : object_loc -> prop_name -> attributes -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_5 : object_loc -> prop_name -> attributes -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_6a : object_loc -> prop_name -> attributes -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_6b : object_loc -> prop_name -> attributes -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_6c : object_loc -> prop_name -> attributes -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_reject : bool -> ext_expr
  | spec_object_define_own_prop_write : object_loc -> prop_name -> attributes -> descriptor -> bool -> ext_expr

  | spec_prim_value_get : value -> prop_name -> ext_expr
  | spec_prim_value_get_1 : value -> prop_name -> out -> ext_expr
  | spec_prim_value_put : value -> prop_name -> value -> bool -> ext_expr
  | spec_prim_value_put_1 : prim -> prop_name -> value -> bool -> out -> ext_expr

  (* ARRAYS *)
  | spec_object_define_own_prop_array_2 : object_loc -> prop_name -> descriptor -> bool -> (specret full_descriptor) -> ext_expr
  | spec_object_define_own_prop_array_2_1 : object_loc -> prop_name -> descriptor -> bool -> descriptor -> value -> ext_expr
  | spec_object_define_own_prop_array_branch_3_4 : object_loc -> prop_name -> descriptor -> bool -> descriptor -> (specret int) -> ext_expr
  | spec_object_define_own_prop_array_branch_4_5   : object_loc -> prop_name -> descriptor -> bool -> descriptor -> int -> ext_expr
  | spec_object_define_own_prop_array_branch_4_5_a : object_loc -> prop_name -> (specret int) -> descriptor -> bool -> descriptor -> int -> ext_expr
  | spec_object_define_own_prop_array_branch_4_5_b : object_loc -> prop_name -> int -> out -> descriptor -> bool -> descriptor -> int -> ext_expr 
  | spec_object_define_own_prop_array_4a : object_loc -> prop_name -> descriptor -> bool -> descriptor -> int -> ext_expr
  | spec_object_define_own_prop_array_4b : object_loc -> prop_name -> (specret int) -> descriptor -> bool -> descriptor -> int -> ext_expr
  | spec_object_define_own_prop_array_4c : object_loc -> int -> descriptor -> bool -> int -> descriptor -> out -> ext_expr
  | spec_object_define_own_prop_array_5  : object_loc -> prop_name -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_array_3 : object_loc -> descriptor -> bool -> descriptor -> int -> ext_expr
  | spec_object_define_own_prop_array_3c : object_loc -> value -> (specret int) -> descriptor -> bool -> descriptor -> int -> ext_expr
  | spec_object_define_own_prop_array_3d_e : object_loc -> out -> int -> descriptor -> bool -> descriptor -> int -> ext_expr
  | spec_object_define_own_prop_array_3f_g : object_loc -> int -> int -> descriptor -> bool -> descriptor -> ext_expr
  | spec_object_define_own_prop_array_3h_i : object_loc -> int -> int -> descriptor -> bool -> descriptor -> ext_expr
  | spec_object_define_own_prop_array_3j : object_loc -> int -> int -> descriptor -> bool -> bool -> descriptor -> ext_expr
  | spec_object_define_own_prop_array_3k_l : object_loc -> out -> int -> int -> descriptor -> bool -> bool -> descriptor -> ext_expr
  | spec_object_define_own_prop_array_3l : object_loc -> int -> int -> descriptor -> bool -> bool -> ext_expr
  | spec_object_define_own_prop_array_3l_ii : object_loc -> int -> int -> descriptor -> bool -> bool -> ext_expr
  | spec_object_define_own_prop_array_3l_ii_1 : object_loc -> int -> int -> descriptor -> bool -> bool -> out -> ext_expr
  | spec_object_define_own_prop_array_3l_ii_2 : object_loc -> int -> int -> descriptor -> bool -> bool -> out -> ext_expr
  | spec_object_define_own_prop_array_3l_iii_1 : object_loc -> int -> descriptor -> bool -> bool -> ext_expr
  | spec_object_define_own_prop_array_3l_iii_2 : object_loc -> descriptor -> bool -> bool -> ext_expr
  | spec_object_define_own_prop_array_3l_iii_3 : object_loc -> descriptor -> bool -> ext_expr
  | spec_object_define_own_prop_array_3l_iii_4 : object_loc -> bool -> out -> ext_expr
  | spec_object_define_own_prop_array_3m_n : object_loc -> bool -> ext_expr

  (** Extended expressions for operations on references *)
  | spec_put_value : resvalue -> value -> ext_expr

  (** Extended expressions for operations on environment records *)

  | spec_env_record_has_binding : env_loc -> prop_name -> ext_expr
  | spec_env_record_has_binding_1 : env_loc -> prop_name -> env_record -> ext_expr
  | spec_env_record_get_binding_value : env_loc -> prop_name -> bool -> ext_expr
  | spec_env_record_get_binding_value_1 : env_loc -> prop_name -> bool -> env_record -> ext_expr
  | spec_env_record_get_binding_value_2 : prop_name -> bool -> object_loc -> out -> ext_expr

  | spec_env_record_create_immutable_binding : env_loc -> prop_name -> ext_expr
  | spec_env_record_initialize_immutable_binding : env_loc -> prop_name -> value -> ext_expr
  | spec_env_record_create_mutable_binding : env_loc -> prop_name -> option bool -> ext_expr
  | spec_env_record_create_mutable_binding_1 : env_loc -> prop_name -> bool -> env_record -> ext_expr
  | spec_env_record_create_mutable_binding_2 : env_loc -> prop_name -> bool -> object_loc -> out -> ext_expr
  | spec_env_record_create_mutable_binding_3 : out -> ext_expr
  | spec_env_record_set_mutable_binding : env_loc -> prop_name -> value -> bool -> ext_expr
  | spec_env_record_set_mutable_binding_1 : env_loc -> prop_name -> value -> bool -> env_record -> ext_expr
  | spec_env_record_delete_binding : env_loc -> prop_name -> ext_expr
  | spec_env_record_delete_binding_1 : env_loc -> prop_name -> env_record -> ext_expr

  | spec_env_record_create_set_mutable_binding : env_loc -> prop_name -> option bool -> value -> bool -> ext_expr
  | spec_env_record_create_set_mutable_binding_1 : out -> env_loc -> prop_name -> value -> bool -> ext_expr

  | spec_env_record_implicit_this_value : env_loc -> ext_expr
  | spec_env_record_implicit_this_value_1 : env_loc -> env_record -> ext_expr


  (** Extended expressions for operations on property descriptors (8.10) *)

  | spec_from_descriptor : (specret full_descriptor) -> ext_expr
  | spec_from_descriptor_1 : attributes -> out -> ext_expr
  | spec_from_descriptor_2 : object_loc -> attributes_data -> out -> ext_expr
  | spec_from_descriptor_3 : object_loc -> attributes_accessor -> out -> ext_expr
  | spec_from_descriptor_4 : object_loc -> attributes -> out -> ext_expr
  | spec_from_descriptor_5 : object_loc -> attributes -> out -> ext_expr
  | spec_from_descriptor_6 : object_loc -> out -> ext_expr

                       
                  
  (** Extented expressions for eval *)

  | spec_entering_eval_code : bool -> funcbody -> ext_expr -> ext_expr
  | spec_entering_eval_code_1 : funcbody -> ext_expr -> bool -> ext_expr
  | spec_entering_eval_code_2 : out -> ext_expr -> ext_expr

  | spec_call_global_eval : bool -> list value -> ext_expr
  | spec_call_global_eval_1 : bool -> value -> ext_expr
  | spec_call_global_eval_2 : prog -> ext_expr
  | spec_call_global_eval_3 : out -> ext_expr

  (** Extended expressions for function calls *)

  | spec_entering_func_code : object_loc -> value -> list value -> ext_expr -> ext_expr
  | spec_entering_func_code_1 : object_loc -> list value -> funcbody -> value -> strictness_flag -> ext_expr -> ext_expr
  | spec_entering_func_code_2 : object_loc -> list value -> funcbody -> out -> ext_expr -> ext_expr
  | spec_entering_func_code_3 : object_loc -> list value -> strictness_flag -> funcbody -> value -> ext_expr -> ext_expr
  | spec_entering_func_code_4 : out -> ext_expr -> ext_expr

  | spec_binding_inst_formal_params : list value -> env_loc -> list string -> strictness_flag -> ext_expr
  | spec_binding_inst_formal_params_1 : list value -> env_loc -> string -> list string -> strictness_flag -> value -> out -> ext_expr
  | spec_binding_inst_formal_params_2 : list value -> env_loc -> string -> list string -> strictness_flag -> value -> out -> ext_expr
  | spec_binding_inst_formal_params_3 : list value -> env_loc -> string -> list string -> strictness_flag -> value -> ext_expr
  | spec_binding_inst_formal_params_4 : list value -> env_loc -> list string -> strictness_flag -> out -> ext_expr
  | spec_binding_inst_function_decls :  list value -> env_loc -> list funcdecl -> strictness_flag -> bool -> ext_expr
  | spec_binding_inst_function_decls_1 : list value -> env_loc -> funcdecl -> list funcdecl -> strictness_flag -> bool -> out -> ext_expr
  | spec_binding_inst_function_decls_2 : list value -> env_loc -> funcdecl -> list funcdecl -> strictness_flag -> object_loc -> bool -> out -> ext_expr
  | spec_binding_inst_function_decls_3 : list value -> funcdecl -> list funcdecl -> strictness_flag -> object_loc -> bool -> specret full_descriptor -> ext_expr
  | spec_binding_inst_function_decls_3a : list value -> funcdecl -> list funcdecl -> strictness_flag -> object_loc -> bool -> full_descriptor -> ext_expr
  | spec_binding_inst_function_decls_4 : list value -> env_loc -> funcdecl -> list funcdecl -> strictness_flag -> object_loc -> bool -> out -> ext_expr
  | spec_binding_inst_function_decls_5 : list value -> env_loc -> funcdecl -> list funcdecl -> strictness_flag -> object_loc -> bool -> ext_expr
  | spec_binding_inst_function_decls_6 : list value -> env_loc -> list funcdecl -> strictness_flag -> bool -> out -> ext_expr
  | spec_binding_inst_arg_obj :   object_loc -> prog -> list string -> list value -> env_loc -> ext_expr
  | spec_binding_inst_arg_obj_1 : prog -> env_loc -> strictness_flag -> out -> ext_expr
  | spec_binding_inst_arg_obj_2 : prog -> env_loc -> object_loc -> out -> ext_expr
  | spec_binding_inst_var_decls : env_loc -> list string -> bool -> strictness_flag -> ext_expr
  | spec_binding_inst_var_decls_1 : env_loc -> string -> list string -> bool -> strictness_flag -> out -> ext_expr
  | spec_binding_inst_var_decls_2 : env_loc -> list string -> bool -> strictness_flag -> out -> ext_expr
  | spec_binding_inst : codetype -> option object_loc -> prog -> list value -> ext_expr
  | spec_binding_inst_1 : codetype -> option object_loc -> prog -> list value -> env_loc -> ext_expr
  | spec_binding_inst_2 : codetype -> object_loc -> prog -> list string -> list value -> env_loc -> out -> ext_expr
  | spec_binding_inst_3 : codetype -> option object_loc -> prog -> list string -> list value -> env_loc -> ext_expr
  | spec_binding_inst_4 : codetype -> option object_loc -> prog -> list string -> list value -> bool -> env_loc -> out -> ext_expr
  | spec_binding_inst_5 : codetype -> option object_loc -> prog -> list string -> list value -> bool -> env_loc -> ext_expr
  | spec_binding_inst_6 : codetype -> option object_loc -> prog -> list string -> list value -> bool -> env_loc -> out -> ext_expr
  | spec_binding_inst_7 : prog -> bool -> env_loc -> out -> ext_expr
  | spec_binding_inst_8 : prog -> bool -> env_loc -> ext_expr
  
  | spec_make_arg_getter : string -> lexical_env -> ext_expr
  | spec_make_arg_setter : string -> lexical_env -> ext_expr
  
  | spec_args_obj_get_1 : value -> object_loc -> prop_name -> object_loc -> (specret full_descriptor) -> ext_expr
  
  
  | spec_args_obj_define_own_prop_1 : object_loc -> prop_name -> descriptor -> bool -> object_loc -> specret full_descriptor -> ext_expr
  | spec_args_obj_define_own_prop_2 : object_loc -> prop_name -> descriptor -> bool -> object_loc -> full_descriptor -> out -> ext_expr
  | spec_args_obj_define_own_prop_3 : object_loc -> prop_name -> descriptor -> bool -> object_loc -> out -> ext_expr
  | spec_args_obj_define_own_prop_4 : object_loc -> prop_name -> descriptor -> bool -> object_loc -> ext_expr
  | spec_args_obj_define_own_prop_5 : out -> ext_expr
  | spec_args_obj_define_own_prop_6 : ext_expr
  
  | spec_args_obj_delete_1 : object_loc -> prop_name -> bool -> object_loc -> specret full_descriptor -> ext_expr
  | spec_args_obj_delete_2 : object_loc -> prop_name -> bool -> object_loc -> full_descriptor -> out -> ext_expr
  | spec_args_obj_delete_3 : out -> ext_expr
  | spec_args_obj_delete_4 : bool -> ext_expr
  
  | spec_arguments_object_map : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> ext_expr
  | spec_arguments_object_map_1 : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> out -> ext_expr
  | spec_arguments_object_map_2 : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> object_loc -> list string -> int -> ext_expr
  | spec_arguments_object_map_3 : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> object_loc -> list string -> int -> out -> ext_expr
  | spec_arguments_object_map_4 : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> object_loc -> list string -> int -> string -> ext_expr
  | spec_arguments_object_map_5 : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> object_loc -> list string -> int -> string -> out -> ext_expr
  | spec_arguments_object_map_6 : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> object_loc -> list string -> int -> object_loc -> out -> ext_expr
  | spec_arguments_object_map_7 : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> object_loc -> list string -> int -> out -> ext_expr
  | spec_arguments_object_map_8 : object_loc -> object_loc -> list string -> ext_expr

  | spec_create_arguments_object : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> ext_expr
  | spec_create_arguments_object_1 : object_loc -> list string -> list value -> lexical_env -> strictness_flag -> object_loc -> out -> ext_expr
  | spec_create_arguments_object_2 : object_loc -> strictness_flag -> object_loc -> out -> ext_expr
  | spec_create_arguments_object_3 : object_loc -> value -> attributes -> out -> ext_expr
  | spec_create_arguments_object_4 : object_loc -> out -> ext_expr

  (* Functions *)

  | spec_object_has_instance : object_loc -> value -> ext_expr
  | spec_object_has_instance_1 : builtin_has_instance -> object_loc -> value -> ext_expr
  | spec_function_has_instance_1 : object_loc -> out -> ext_expr
  | spec_function_has_instance_2 : object_loc -> object_loc -> ext_expr
  | spec_function_has_instance_3 : object_loc -> value -> ext_expr

  | spec_function_has_instance_after_bind_1 : object_loc -> value -> ext_expr
  | spec_function_has_instance_after_bind_2 : object_loc -> value  -> ext_expr

  | spec_function_get_1 : object_loc -> prop_name -> out -> ext_expr

  (* Function.prototype.apply *)

  | spec_function_proto_apply   : object_loc -> value -> value -> ext_expr
  | spec_function_proto_apply_1 : object_loc -> value -> object_loc -> out -> ext_expr   
  | spec_function_proto_apply_2 : object_loc -> value -> object_loc -> specret int -> ext_expr
  | spec_function_proto_apply_3 : object_loc -> value -> specret (list value) -> ext_expr 

  (* Function.prototype.bind *)

  | spec_function_proto_bind_1 : object_loc -> value -> list value -> ext_expr
  | spec_function_proto_bind_2 : object_loc -> value -> list value -> ext_expr
  | spec_function_proto_bind_3 : object_loc -> specret int -> ext_expr
  | spec_function_proto_bind_4 : object_loc -> int -> ext_expr
  | spec_function_proto_bind_5 : object_loc -> ext_expr
  | spec_function_proto_bind_6 : object_loc -> out -> ext_expr
  | spec_function_proto_bind_7 : object_loc -> out -> ext_expr

  (* Throwing of errors *)

  | spec_error : native_error -> ext_expr 
  | spec_error_1 : out -> ext_expr
  | spec_error_or_cst : bool -> native_error -> value -> ext_expr
  | spec_error_or_void : bool -> native_error -> ext_expr

  (* LATER: these are currently unused *)
  | spec_init_throw_type_error : ext_expr
  | spec_init_throw_type_error_1 : out -> ext_expr

  | spec_build_error : value -> value -> ext_expr
  | spec_build_error_1 : object_loc -> value -> ext_expr
  | spec_build_error_2 : object_loc -> out -> ext_expr

  (* Object creation and calling continuation with object address *)

  | spec_new_object : (object_loc -> ext_expr) -> ext_expr
  | spec_new_object_1 : out -> (object_loc -> ext_expr) -> ext_expr

  | spec_prim_new_object : prim -> ext_expr

  (* Auxiliary reduction for creating function object steps 16 - 18 *)
  | spec_creating_function_object_proto : object_loc -> ext_expr
  | spec_creating_function_object_proto_1 : object_loc -> out -> ext_expr
  | spec_creating_function_object_proto_2 : object_loc -> object_loc -> out -> ext_expr

  | spec_creating_function_object : list string -> funcbody -> lexical_env -> strictness_flag -> ext_expr
  | spec_creating_function_object_1 : strictness_flag -> object_loc -> out -> ext_expr
  | spec_creating_function_object_2 : strictness_flag -> object_loc -> out -> ext_expr
  | spec_creating_function_object_3 : object_loc -> out -> ext_expr
  | spec_creating_function_object_4 : object_loc -> out -> ext_expr

  (* Function creation in give execution context*)
  | spec_create_new_function_in :  execution_ctx -> list string -> funcbody -> ext_expr

  (* TODO: Check if object_loc or value could be None *)
  (* TODO: get rid of this: | spec_call : builtin -> option object_loc -> option value -> list value -> ext_expr *)
  | spec_call : object_loc -> value -> list value -> ext_expr (* object with the call method, this value, arguments *)
  | spec_call_1 : call -> object_loc -> value -> list value -> ext_expr

  | spec_call_prealloc : prealloc -> value -> list value -> ext_expr

  | spec_call_default : object_loc -> value -> list value -> ext_expr
  | spec_call_default_1 : object_loc -> ext_expr
  | spec_call_default_2 : option funcbody -> ext_expr
  | spec_call_default_3 : out -> ext_expr
 
  | spec_construct : object_loc -> list value -> ext_expr
  | spec_construct_1 : construct -> object_loc -> list value -> ext_expr

  | spec_construct_prealloc : prealloc -> list value -> ext_expr

  | spec_construct_default : object_loc -> list value -> ext_expr
  | spec_construct_default_1 : object_loc -> list value -> out -> ext_expr
  | spec_construct_default_2 : object_loc -> out -> ext_expr

  | spec_construct_1_after_bind : object_loc -> list value -> object_loc -> ext_expr

  (** Extended expressions for calling global object builtin functions *)
  (* LATER: rename all the spec_call into spec_builtin *)

  | spec_call_global_is_nan_1 : out -> ext_expr
  | spec_call_global_is_finite_1 : out -> ext_expr

  | spec_call_object_call_1 : value -> list value -> ext_expr
  | spec_call_object_new_1 : value -> ext_expr
  | spec_call_object_get_proto_of_1 : value -> ext_expr
  | spec_call_object_is_extensible_1 : value -> ext_expr

  | spec_call_object_create_1 : value -> value -> ext_expr
  | spec_call_object_create_2 : out -> value -> value -> ext_expr
  | spec_call_object_create_3 : object_loc -> value -> ext_expr

  | spec_call_object_define_props_1 : value -> value -> ext_expr
  | spec_call_object_define_props_2 : out -> object_loc -> ext_expr
  | spec_call_object_define_props_3 : object_loc -> object_loc -> list prop_name -> list (prop_name * attributes) -> ext_expr
  | spec_call_object_define_props_4 : out -> object_loc -> object_loc -> prop_name -> list prop_name -> list (prop_name * attributes) -> ext_expr
  | spec_call_object_define_props_5 : object_loc -> object_loc -> prop_name -> list prop_name -> list (prop_name * attributes) -> specret attributes -> ext_expr
  | spec_call_object_define_props_6 : object_loc -> list (prop_name * attributes) -> ext_expr
  | spec_call_object_define_props_7 : out -> object_loc -> list (prop_name * attributes) -> ext_expr

  | spec_call_object_seal_1 : value -> ext_expr
  | spec_call_object_seal_2 : object_loc -> list prop_name -> ext_expr
  | spec_call_object_seal_3 : object_loc -> prop_name -> list prop_name -> specret full_descriptor -> ext_expr
  | spec_call_object_seal_4 : object_loc -> list prop_name -> out -> ext_expr

  | spec_call_object_is_sealed_1 : value -> ext_expr
  | spec_call_object_is_sealed_2 : object_loc -> list prop_name -> ext_expr
  | spec_call_object_is_sealed_3 : object_loc -> list prop_name -> specret full_descriptor -> ext_expr

  | spec_call_object_freeze_1 : value -> ext_expr
  | spec_call_object_freeze_2 : object_loc -> list prop_name -> ext_expr
  | spec_call_object_freeze_3 : object_loc -> prop_name -> list prop_name -> specret full_descriptor -> ext_expr
  | spec_call_object_freeze_4 : object_loc -> prop_name -> list prop_name -> full_descriptor -> ext_expr
  | spec_call_object_freeze_5 : object_loc -> list prop_name -> out -> ext_expr

  | spec_call_object_is_frozen_1 : value -> ext_expr
  | spec_call_object_is_frozen_2 : object_loc -> list prop_name -> ext_expr
  | spec_call_object_is_frozen_3 : object_loc -> list prop_name -> specret full_descriptor -> ext_expr
  | spec_call_object_is_frozen_4 : object_loc -> list prop_name -> full_descriptor -> ext_expr
  | spec_call_object_is_frozen_5 : object_loc -> list prop_name -> full_descriptor -> ext_expr


  | spec_call_object_prevent_extensions_1 : value -> ext_expr

  | spec_call_object_define_prop_1 : value -> value -> value -> ext_expr
  | spec_call_object_define_prop_2 : object_loc -> out -> value -> ext_expr
  | spec_call_object_define_prop_3 : object_loc -> string -> specret descriptor -> ext_expr
  | spec_call_object_define_prop_4 : object_loc -> out -> ext_expr

  | spec_call_object_get_own_prop_descriptor_1: value -> value -> ext_expr
  | spec_call_object_get_own_prop_descriptor_2: object_loc -> out -> ext_expr


  | spec_call_object_proto_to_string_1 : value -> ext_expr
  | spec_call_object_proto_to_string_2 : out -> ext_expr

  | spec_call_object_proto_has_own_prop_1 : out -> value -> ext_expr
  | spec_call_object_proto_has_own_prop_2 : out -> prop_name -> ext_expr
  | spec_call_object_proto_has_own_prop_3 : specret full_descriptor -> ext_expr

  | spec_call_object_proto_is_prototype_of_2_1 : value -> value -> ext_expr
  | spec_call_object_proto_is_prototype_of_2_2 : out -> object_loc -> ext_expr
  | spec_call_object_proto_is_prototype_of_2_3 : object_loc -> object_loc -> ext_expr
  | spec_call_object_proto_is_prototype_of_2_4 : object_loc -> value -> ext_expr

  | spec_call_object_proto_prop_is_enumerable_1 : value -> value -> ext_expr
  | spec_call_object_proto_prop_is_enumerable_2 : value -> out -> ext_expr
  | spec_call_object_proto_prop_is_enumerable_3 : out -> string -> ext_expr
  | spec_call_object_proto_prop_is_enumerable_4 : specret full_descriptor -> ext_expr

  | spec_call_array_new_1 : list value -> ext_expr
  | spec_call_array_new_2 : object_loc -> list value -> ext_expr
  | spec_call_array_new_3 : object_loc -> list value -> int -> ext_expr

  | spec_call_array_new_single_1 : value -> ext_expr
  | spec_call_array_new_single_2 : object_loc -> value -> ext_expr
  | spec_call_array_new_single_3 : object_loc -> number -> specret int -> ext_expr
  | spec_call_array_new_single_4 : object_loc -> int -> ext_expr

  | spec_call_array_is_array_1 : value -> ext_expr
  | spec_call_array_is_array_2_3 : class_name -> ext_expr

  | spec_call_array_proto_to_string : out -> ext_expr
  | spec_call_array_proto_to_string_1 : object_loc -> out -> ext_expr

  | spec_call_array_proto_join   : out -> list value -> ext_expr 
  | spec_call_array_proto_join_1 : object_loc -> out -> list value -> ext_expr 
  | spec_call_array_proto_join_2 : object_loc -> specret int -> list value -> ext_expr
  | spec_call_array_proto_join_3 : object_loc -> int -> value -> ext_expr
  | spec_call_array_proto_join_4 : object_loc -> int -> out -> ext_expr
  | spec_call_array_proto_join_5 : object_loc -> int -> string -> specret string -> ext_expr

  | spec_call_array_proto_join_elements : object_loc -> int -> int -> string -> string -> ext_expr
  | spec_call_array_proto_join_elements_1 : object_loc -> int -> int -> string -> string -> ext_expr
  | spec_call_array_proto_join_elements_2 : object_loc -> int -> int -> string -> string -> specret string -> ext_expr

  | spec_call_array_proto_pop_1 : out -> ext_expr
  | spec_call_array_proto_pop_2 : object_loc -> out -> ext_expr
  | spec_call_array_proto_pop_3 : object_loc -> specret int -> ext_expr
  | spec_call_array_proto_pop_3_empty_1 : object_loc -> ext_expr
  | spec_call_array_proto_pop_3_empty_2 : out -> ext_expr
  | spec_call_array_proto_pop_3_nonempty_1 : object_loc -> int -> ext_expr
  | spec_call_array_proto_pop_3_nonempty_2 : object_loc -> out -> ext_expr
  | spec_call_array_proto_pop_3_nonempty_3 : object_loc -> value -> out -> ext_expr
  | spec_call_array_proto_pop_3_nonempty_4 : object_loc -> value -> value -> out -> ext_expr
  | spec_call_array_proto_pop_3_nonempty_5 : value -> out -> ext_expr

  | spec_call_array_proto_push_1 : out -> list value -> ext_expr
  | spec_call_array_proto_push_2 : object_loc -> list value -> out -> ext_expr
  | spec_call_array_proto_push_3 : object_loc -> list value -> specret int -> ext_expr
  | spec_call_array_proto_push_4 : object_loc -> list value -> int -> ext_expr
  | spec_call_array_proto_push_4_nonempty_1 : object_loc -> list value -> int -> value -> ext_expr
  | spec_call_array_proto_push_4_nonempty_2 : object_loc -> list value -> int -> value -> out -> ext_expr
  | spec_call_array_proto_push_4_nonempty_3 : object_loc -> list value -> int -> value -> out -> ext_expr
  | spec_call_array_proto_push_5 : object_loc -> value -> ext_expr
  | spec_call_array_proto_push_6 : value -> out -> ext_expr

  | spec_call_string_non_empty : out -> ext_expr 

  | spec_construct_string_1 : value -> ext_expr
  | spec_construct_string_2 : out -> ext_expr

  | spec_construct_bool_1 : out -> ext_expr
  | spec_call_bool_proto_to_string_1 : out -> ext_expr
  | spec_call_bool_proto_value_of_1 : value -> ext_expr
  | spec_call_bool_proto_value_of_2 : value -> ext_expr

  | spec_call_number_proto_to_string_1 : value -> list value -> ext_expr
  | spec_call_number_proto_to_string_2 : value -> out -> ext_expr
  | spec_construct_number_1 : out -> ext_expr
  | spec_call_number_proto_value_of_1 : value -> ext_expr

  | spec_call_error_proto_to_string_1 : value -> ext_expr
  | spec_call_error_proto_to_string_2 : object_loc -> out -> ext_expr
  | spec_call_error_proto_to_string_3 : object_loc -> out -> ext_expr
  | spec_call_error_proto_to_string_4 : object_loc -> string -> out -> ext_expr
  | spec_call_error_proto_to_string_5 : object_loc -> string -> out -> ext_expr
  | spec_call_error_proto_to_string_6 : object_loc -> string -> out -> ext_expr


  (** Special state for returning an outcome *)

  | spec_returns : out -> ext_expr

(** Grammar of extended statements *)

with ext_stat :=

  (** Extended expressions include statements *)

  | stat_basic : stat -> ext_stat

  (** Extended statements associated with primitive statements *)
  | stat_expr_1: (specret value) -> ext_stat

  | stat_block_1 : out -> stat -> ext_stat
  | stat_block_2 : resvalue -> out -> ext_stat

  | stat_label_1 : label -> out -> ext_stat

  | stat_var_decl_1 : out -> list (string * option expr) -> ext_stat
  | stat_var_decl_item : (string * option expr) -> ext_stat
  | stat_var_decl_item_1 : string -> specret ref -> expr -> ext_stat
  | stat_var_decl_item_2 : string -> ref -> (specret value) -> ext_stat
  | stat_var_decl_item_3 : string -> out -> ext_stat

  | stat_if_1 : specret value -> stat -> option stat -> ext_stat

  | stat_while_1 : label_set -> expr -> stat -> resvalue -> ext_stat
  | stat_while_2 : label_set -> expr -> stat -> resvalue -> specret value -> ext_stat
  | stat_while_3 : label_set -> expr -> stat -> resvalue -> out -> ext_stat
  | stat_while_4 : label_set -> expr -> stat -> resvalue -> res -> ext_stat
  | stat_while_5 : label_set -> expr -> stat -> resvalue -> res -> ext_stat
  | stat_while_6 : label_set -> expr -> stat -> resvalue -> res -> ext_stat


  | stat_do_while_1 : label_set -> stat -> expr -> resvalue -> ext_stat
  | stat_do_while_2 : label_set -> stat -> expr -> resvalue -> out -> ext_stat
  | stat_do_while_3 : label_set -> stat -> expr -> resvalue -> res -> ext_stat
  | stat_do_while_4 : label_set -> stat -> expr -> resvalue -> res -> ext_stat
  | stat_do_while_5 : label_set -> stat -> expr -> resvalue -> res -> ext_stat
  | stat_do_while_6 : label_set -> stat -> expr -> resvalue -> ext_stat
  | stat_do_while_7 : label_set -> stat -> expr -> resvalue -> specret value -> ext_stat

  | stat_for_1 : label_set -> specret value -> option expr -> option expr -> stat -> ext_stat
  | stat_for_2 : label_set -> resvalue -> option expr -> option expr -> stat -> ext_stat
  | stat_for_3 : label_set -> resvalue -> expr -> specret value -> option expr -> stat -> ext_stat
  | stat_for_4 : label_set -> resvalue -> option expr -> option expr -> stat -> ext_stat
  | stat_for_5 : label_set -> resvalue -> option expr -> out -> option expr -> stat -> ext_stat
  | stat_for_6 : label_set -> resvalue -> option expr -> option expr -> stat -> res -> ext_stat
  | stat_for_7 : label_set -> resvalue -> option expr -> option expr -> stat -> res -> ext_stat
  | stat_for_8 : label_set -> resvalue -> option expr -> option expr -> stat -> ext_stat
  | stat_for_9 : label_set -> resvalue -> option expr -> expr -> specret value -> stat -> ext_stat

  | stat_for_var_1 : out -> label_set -> option expr -> option expr -> stat -> ext_stat


(* LATER: define prop_names for [set prop_name] *)
(* LATER
  | stat_for_in_1 : expr -> stat -> out -> ext_stat
  | stat_for_in_2 : expr -> stat -> out -> ext_stat
  | stat_for_in_3 : expr -> stat -> out -> ext_stat
  | stat_for_in_4 : expr -> stat -> object_loc -> option res -> option out -> set prop_name -> set prop_name -> ext_stat 
  | stat_for_in_5 : expr -> stat -> object_loc -> option res -> option out -> set prop_name -> set prop_name -> prop_name -> ext_stat
  | stat_for_in_6 : expr -> stat -> object_loc -> option res -> option out -> set prop_name -> set prop_name -> prop_name -> ext_stat
  | stat_for_in_7 : expr -> stat -> object_loc -> option res -> option out -> set prop_name -> set prop_name -> out -> ext_stat
  | stat_for_in_8 : expr -> stat -> object_loc -> option res -> option out -> set prop_name -> set prop_name -> out -> ext_stat
  | stat_for_in_9 : expr -> stat -> object_loc -> option res -> option out -> set prop_name -> set prop_name -> res -> ext_stat
*)

  (* Extended statements for 'switch' *)
                                                                           
  | stat_switch_1: (specret value) -> label_set -> switchbody -> ext_stat
  | stat_switch_2: out -> label_set -> ext_stat
  | stat_switch_nodefault_1: value -> resvalue -> list switchclause -> ext_stat
  | stat_switch_nodefault_2: (specret value) -> value -> resvalue -> list stat -> list switchclause -> ext_stat
  | stat_switch_nodefault_3: bool -> value -> resvalue -> list stat -> list switchclause -> ext_stat
  | stat_switch_nodefault_4: out -> list switchclause -> ext_stat
  | stat_switch_nodefault_5: resvalue -> list switchclause -> ext_stat
  | stat_switch_nodefault_6: resvalue -> out -> list switchclause -> ext_stat

  | stat_switch_default_1: value -> resvalue -> list switchclause -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_A_1: bool -> value -> resvalue -> list switchclause -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_A_2: (specret value) -> value -> resvalue -> list stat -> list switchclause -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_A_3: bool -> value -> resvalue -> list stat -> list switchclause -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_A_4: resvalue -> value -> list stat -> list switchclause -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_A_5: resvalue -> out -> value -> list switchclause -> list stat -> list switchclause -> ext_stat

  | stat_switch_default_B_1: value -> resvalue -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_B_2: (specret value) -> value -> resvalue -> list stat -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_B_3: bool -> value -> resvalue -> list stat -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_B_4: out -> list stat -> list switchclause -> ext_stat

  | stat_switch_default_5: value -> resvalue -> list stat -> list switchclause -> ext_stat
  | stat_switch_default_6: out -> list switchclause -> ext_stat
  | stat_switch_default_7: resvalue -> list switchclause -> ext_stat
  | stat_switch_default_8: resvalue -> out -> list switchclause -> ext_stat

  | stat_with_1 : stat -> specret value -> ext_stat (* The expression have been executed. *)

  | stat_throw_1 : (specret value) -> ext_stat (* The expression have been executed. *)

  | stat_return_1 : (specret value) -> ext_stat (* The expression have been executed. *)

  | stat_try_1 : out -> option (string*stat) -> option stat -> ext_stat (* The try block has been executed. *)
  | stat_try_2 : out -> lexical_env -> stat -> option stat -> ext_stat (* The catch block is actived and will be executed. *)
  | stat_try_3 : out -> option stat -> ext_stat (* The try catch block has been executed:  there only stay an optional finally. *)
  | stat_try_4 : res -> option stat -> ext_stat (* The try catch block has been executed:  there only stay an optional finally. *)
  | stat_try_5 : res -> out -> ext_stat (* The finally has been executed. *)

(** Grammar of extended programs *)

with ext_prog :=

  | prog_basic : prog -> ext_prog
  | javascript_1 : out -> prog -> ext_prog
  | prog_1 : out -> element -> ext_prog
  | prog_2 : resvalue -> out -> ext_prog


(** Grammar of extended forms for specification functions *)

with ext_spec :=
  | spec_to_int32 : value -> ext_spec
  | spec_to_int32_1 : out -> ext_spec
  | spec_to_uint32 : value -> ext_spec
  | spec_to_uint32_1 : out -> ext_spec

  | spec_expr_get_value_conv : (value -> ext_expr) -> expr -> ext_spec
  | spec_expr_get_value_conv_1 : (value -> ext_expr) -> (specret value) -> ext_spec
  | spec_expr_get_value_conv_2 : out -> ext_spec

  | spec_convert_twice : ext_expr -> ext_expr -> ext_spec
  | spec_convert_twice_1 : out -> ext_expr -> ext_spec
  | spec_convert_twice_2 : value -> out -> ext_spec

  (** Extended expressions for lists of expressions *)
  | spec_list_expr : list expr -> ext_spec
  | spec_list_expr_1 : list value -> list expr -> ext_spec
  | spec_list_expr_2 : list value -> (specret value) -> list expr -> ext_spec 

  | spec_to_descriptor : value -> ext_spec
  | spec_to_descriptor_1a : object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_1b : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_1c : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_2a : object_loc -> descriptor -> ext_spec                            
  | spec_to_descriptor_2b : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_2c : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_3a : object_loc -> descriptor -> ext_spec                            
  | spec_to_descriptor_3b : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_3c : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_4a : object_loc -> descriptor -> ext_spec                            
  | spec_to_descriptor_4b : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_4c : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_5a : object_loc -> descriptor -> ext_spec                            
  | spec_to_descriptor_5b : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_5c : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_6a : object_loc -> descriptor -> ext_spec                            
  | spec_to_descriptor_6b : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_6c : out -> object_loc -> descriptor -> ext_spec
  | spec_to_descriptor_7 : object_loc -> descriptor -> ext_spec    

  | spec_object_get_own_prop : object_loc -> prop_name -> ext_spec
  | spec_object_get_own_prop_1 : builtin_get_own_prop -> object_loc -> prop_name -> ext_spec
  | spec_object_get_own_prop_2 : object_loc -> prop_name -> option attributes -> ext_spec

  | spec_object_get_prop : object_loc -> prop_name -> ext_spec
  | spec_object_get_prop_1 : builtin_get_prop -> object_loc -> prop_name -> ext_spec
  | spec_object_get_prop_2 : object_loc -> prop_name -> specret full_descriptor -> ext_spec
  | spec_object_get_prop_3 : object_loc -> prop_name -> value -> ext_spec

  | spec_get_value : resvalue -> ext_spec
  | spec_get_value_ref_b_1 : out -> ext_spec
  | spec_get_value_ref_c_1 : out -> ext_spec

  (** Shorthand for calling [red_expr] then [ref_get_value] *)

  | spec_expr_get_value : expr -> ext_spec
  | spec_expr_get_value_1 : out -> ext_spec

  (** Extended expressions for operations on lexical environments *)

  | spec_lexical_env_get_identifier_ref : lexical_env -> prop_name -> bool -> ext_spec
  | spec_lexical_env_get_identifier_ref_1 : env_loc -> lexical_env -> prop_name -> bool -> ext_spec
  | spec_lexical_env_get_identifier_ref_2 : env_loc -> lexical_env -> prop_name -> bool -> out -> ext_spec

  (** Errors in the grammar of spec *) (* LATER: merge *)
  | spec_error_spec : native_error -> ext_spec
  | spec_error_spec_1 : out -> ext_spec

  (* .. *)
  | spec_args_obj_get_own_prop_1 : object_loc -> prop_name -> (specret full_descriptor) -> ext_spec
  | spec_args_obj_get_own_prop_2 : object_loc -> prop_name -> object_loc -> full_descriptor -> (specret full_descriptor) -> ext_spec
  | spec_args_obj_get_own_prop_3 : full_descriptor -> out -> ext_spec
  | spec_args_obj_get_own_prop_4 : full_descriptor -> ext_spec

  | spec_string_get_own_prop_1 : object_loc -> prop_name -> (specret full_descriptor) -> ext_spec
  | spec_string_get_own_prop_2 : object_loc -> prop_name -> (specret int) -> ext_spec
  | spec_string_get_own_prop_3 : object_loc -> prop_name -> out -> ext_spec
  | spec_string_get_own_prop_4 : prop_name -> string -> ext_spec
  | spec_string_get_own_prop_5 : string -> (specret int) -> ext_spec
  | spec_string_get_own_prop_6 : string -> int -> int -> ext_spec

  (* Argumenst for Function.prototype.apply *)

  | spec_function_proto_apply_get_args   : object_loc -> int -> int -> ext_spec
  | spec_function_proto_apply_get_args_1 : object_loc -> int -> int -> out -> ext_spec
  | spec_function_proto_apply_get_args_2 : object_loc -> int -> int -> out -> ext_spec
  | spec_function_proto_apply_get_args_3 : value -> specret (list value) -> ext_spec 

  (* Length for Function.prototype.bind *)

  | spec_function_proto_bind_length   : object_loc -> list value -> ext_spec
  | spec_function_proto_bind_length_1 : object_loc -> list value -> ext_spec
  | spec_function_proto_bind_length_2 : list value -> out -> ext_spec
  | spec_function_proto_bind_length_3 : specret int -> list value -> ext_spec

  (* Conversion for Array.prototype.join *)

  | spec_call_array_proto_join_vtsfj : object_loc -> int -> ext_spec
  | spec_call_array_proto_join_vtsfj_1 : object_loc -> out -> ext_spec
  | spec_call_array_proto_join_vtsfj_2 : object_loc -> out -> ext_spec
  | spec_call_array_proto_join_vtsfj_3 : out -> ext_spec
.

(** Coercions *)

Coercion expr_basic : expr >-> ext_expr.
Coercion stat_basic : stat >-> ext_stat.
Coercion prog_basic : prog >-> ext_prog.


(** Shorthand for calling toPrimitive without prefered type *)

Definition spec_to_primitive_auto v :=
  spec_to_primitive v None.


(**************************************************************)
(** ** Extracting outcome from an extended expression. *)

(** Auxiliary definition for extracting [out] of [specret] *)

Definition out_of_specret T (y:specret T) :=
  match y with
  | specret_out o => Some o
  | specret_val _ _ => None
  end.

(** The [out_of_ext_*] family of definitions is used by
    the generic abort rule, which propagates exceptions,
    and divergence, break and continues. *)

Definition out_of_ext_expr (e : ext_expr) : option out :=
  match e with
  | expr_basic _ => None

  | expr_identifier_1 y => out_of_specret y   
  | expr_object_0 o _ => Some o
  | expr_object_1 _ _ => None
  | expr_object_2 _ _ _ _ => None
  | expr_object_3_val _ _ y _ => out_of_specret y
  | expr_object_3_get _ _ o _ => Some o
  | expr_object_3_set _ _ o _ => Some o
  | expr_object_4 _ _ _ _ => None
  | expr_object_5 _ _ o => Some o




  (* _ARRAYS_ : support for array intermediate forms - CHECK!*)
  | expr_array_0 o _ => Some o
  | expr_array_1 _ _ => None
  | expr_array_2 _ _ _ => None
  | expr_array_3 _ _ _ => None
  | expr_array_3_1 _ y _ _ => out_of_specret y
  | expr_array_3_2 _ _ o _ _ => Some o
  | expr_array_3_3 _ _ y _ _ => out_of_specret y
  | expr_array_3_4 _ _ o _ => Some o
  | expr_array_3_5 _ o _ => Some o
  

  | expr_array_add_length   _ _ o => Some o
  | expr_array_add_length_0 _ _ => None
  | expr_array_add_length_1 _ _ o => Some o 
  | expr_array_add_length_2 _ y _ => out_of_specret y
  | expr_array_add_length_3 _ y => out_of_specret y
  | expr_array_add_length_4 _ o => Some o





  | expr_function_1 _ _ _ _ _ o => Some o
  | expr_function_2 _ _ o => Some o
  | expr_function_3 _ o => Some o

  | expr_access_1 y _ => out_of_specret y
  | expr_access_2 _ y => out_of_specret y
  | expr_access_3 _ o _ => Some o
  | expr_access_4 _ o => Some o

  | expr_new_1 y _ => out_of_specret y
  | expr_new_2 _ y => out_of_specret y  

  | expr_call_1 o _ _ => Some o
  | expr_call_2 _ _ _ y => out_of_specret y
  | expr_call_3 _ _ _ y => out_of_specret y  
  | expr_call_4 _ _ _ _ => None
  | expr_call_5 _ _ _ o => Some o

  | spec_eval _ _ _ => None

  | expr_unary_op_1 _ y => out_of_specret y
  | expr_unary_op_2 _ _ => None
  | expr_delete_1 o => Some o
  | expr_delete_2 _ => None
  | expr_delete_3 _ o => Some o
  | expr_delete_4 _ _ => None
  | expr_typeof_1 o => Some o
  | expr_typeof_2 y => out_of_specret y
  | expr_prepost_1 _ o => Some o
(*  | expr_prepost_2 _ _ o => Some o *)

  | expr_prepost_2 _ _ y => out_of_specret y

  | expr_prepost_3 _ _ o => Some o
  | expr_prepost_4 _ o => Some o
  | expr_unary_op_neg_1 o => Some o
  | expr_unary_op_bitwise_not_1 y => out_of_specret y
  | expr_unary_op_not_1 o => Some o
  | expr_conditional_1 y _ _ => out_of_specret y
  | expr_conditional_1' o _ _ => None
  | expr_conditional_2 y => out_of_specret y

  | expr_binary_op_1 _ y _ => out_of_specret y
  | expr_binary_op_2 _ _ y => out_of_specret y
  | expr_binary_op_3 _ _ _ => None
  | expr_binary_op_add_1 y => out_of_specret y
  | expr_binary_op_add_string_1 y => out_of_specret y
  | expr_puremath_op_1 _ y => out_of_specret y
  | expr_shift_op_1 _ y _ => out_of_specret y
  | expr_shift_op_2 _ _ y => out_of_specret y
  | expr_inequality_op_1 _ _ _ _ => None
  | expr_inequality_op_2 _ _ y => out_of_specret y
  | expr_binary_op_in_1 _ o => Some o
  | expr_binary_op_disequal_1 o => Some o
  | spec_equal _ _ => None
  | spec_equal_1 _ _ _ _ => None
  | spec_equal_2 _ => None
  | spec_equal_3 _ _ _ => None
  | spec_equal_4 _ o => Some o
  | expr_bitwise_op_1 _ y _ => out_of_specret y
  | expr_bitwise_op_2 _ _ y => out_of_specret y
  | expr_lazy_op_1 _ y _ => out_of_specret y
  | expr_lazy_op_2 _ _ o _ => Some o
  | expr_lazy_op_2_1 y => out_of_specret y

  | expr_assign_1 o _ _ => Some o
  | expr_assign_2 _ y _ _ => out_of_specret y
  | expr_assign_3 _ _ _ y => out_of_specret y
  | expr_assign_3' _ o => Some o
  | expr_assign_4 _ y => out_of_specret y
  | expr_assign_5 _ o => Some o

  | spec_to_primitive _ _ => None
  | spec_to_boolean _ => None
  | spec_to_number _ => None
  | spec_to_number_1 o => Some o
  | spec_to_integer _ => None
  | spec_to_integer_1 o => Some o
  | spec_to_string _ => None
  | spec_to_string_1 o => Some o
  | spec_to_object _ => None

  | spec_check_object_coercible _ => None

  | spec_eq _ _ => None
  | spec_eq0 _ _ => None
  | spec_eq1 _ _ => None
  | spec_eq2 _ _ _ => None

  | spec_object_get _ _ => None
  | spec_object_get_1 _ _ _ _ => None
  | spec_object_get_2 _ y => out_of_specret y
  | spec_object_get_3 _ _ => None

  | spec_object_can_put _ _ => None
  | spec_object_can_put_1 _ _ _ => None
  | spec_object_can_put_2 _ _ y => out_of_specret y
  | spec_object_can_put_4 _ _ _ => None
  | spec_object_can_put_5 _ y => out_of_specret y
  | spec_object_can_put_6 _ _ => None

  | spec_object_put _ _ _ _ => None
  | spec_object_put_1 _ _ _ _ _ _ => None
  | spec_object_put_2 _ _ _ _ _ o => Some o
  | spec_object_put_3 _ _ _ _ _ y => out_of_specret y
  | spec_object_put_4 _ _ _ _ _ y => out_of_specret y
  | spec_object_put_5 o => Some o

  | spec_object_has_prop _ _ => None
  | spec_object_has_prop_1 _ _ _ => None
  | spec_object_has_prop_2 y => out_of_specret y

  | spec_object_delete _ _ _ => None
  | spec_object_delete_1 _ _ _ _ => None
  | spec_object_delete_2 _ _ _ y => out_of_specret y
  | spec_object_delete_3 _ _ _ _ => None

  | spec_object_default_value _ _ => None
  | spec_object_default_value_1 _ _ _ => None
  | spec_object_default_value_2 _ _ _ => None
  | spec_object_default_value_3 _ _ => None
  | spec_object_default_value_4 => None
  | spec_object_default_value_sub_1 _ _ _ => None
  | spec_object_default_value_sub_2 _ o _ => Some o
  | spec_object_default_value_sub_3 o _ => Some o

  | spec_object_define_own_prop _ _ _ _ => None
  | spec_object_define_own_prop_1 _ _ _ _ _ => None
  | spec_object_define_own_prop_2 _ _ _ _ y => out_of_specret y
  | spec_object_define_own_prop_3 _ _ _ _ _ _ => None
  | spec_object_define_own_prop_4 _ _ _ _ _ => None
  | spec_object_define_own_prop_5 _ _ _ _ _ => None
  | spec_object_define_own_prop_6a _ _ _ _ _ => None
  | spec_object_define_own_prop_6b _ _ _ _ _ => None
  | spec_object_define_own_prop_6c _ _ _ _ _ => None
  | spec_object_define_own_prop_reject _ => None
  | spec_object_define_own_prop_write _ _ _ _ _ => None



  (* ARRAYS *)
  | spec_object_define_own_prop_array_2 _ _ _ _ y => out_of_specret y
  | spec_object_define_own_prop_array_2_1 _ _ _ _ _ _ => None
  | spec_object_define_own_prop_array_branch_3_4 _ _ _ _ _ y => out_of_specret y
  | spec_object_define_own_prop_array_branch_4_5 _ _ _ _ _ _ => None
  | spec_object_define_own_prop_array_branch_4_5_a _ _ y _ _ _ _ => out_of_specret y
  | spec_object_define_own_prop_array_branch_4_5_b _ _ _ o _ _ _ _ => Some o
  | spec_object_define_own_prop_array_4a _ _ _ _ _ _ => None
  | spec_object_define_own_prop_array_4b _ _ y _ _ _ _ => out_of_specret y
  | spec_object_define_own_prop_array_4c _ _ _ _ _ _ o => Some o
  | spec_object_define_own_prop_array_5 _ _ _ _ => None
  | spec_object_define_own_prop_array_3 _ _ _ _ _ => None
  | spec_object_define_own_prop_array_3c _ _ y _ _ _ _ => out_of_specret y
  | spec_object_define_own_prop_array_3d_e _ o _ _ _ _ _ => Some o
  | spec_object_define_own_prop_array_3f_g _ _ _ _ _ _ => None
  | spec_object_define_own_prop_array_3h_i _ _ _ _ _ _ => None
  | spec_object_define_own_prop_array_3j _ _ _ _ _ _ _ => None
  | spec_object_define_own_prop_array_3k_l _ o _ _ _ _ _ _ => Some o
  | spec_object_define_own_prop_array_3l _ _ _ _ _ _ => None
  | spec_object_define_own_prop_array_3l_ii _ _ _ _ _ _ => None
  | spec_object_define_own_prop_array_3l_ii_1 _ _ _ _ _ _ o => Some o
  | spec_object_define_own_prop_array_3l_ii_2 _ _ _ _ _ _ o => Some o
  | spec_object_define_own_prop_array_3l_iii_1 _ _ _ _ _ => None
  | spec_object_define_own_prop_array_3l_iii_2 _ _ _ _ => None
  | spec_object_define_own_prop_array_3l_iii_3 _ _ _ => None
  | spec_object_define_own_prop_array_3l_iii_4 _ _ o => Some o
  | spec_object_define_own_prop_array_3m_n _ _ => None

  | spec_prim_value_get _ _ => None
  | spec_prim_value_get_1 _ _ o => Some o

  | spec_prim_value_put _ _ _ _ => None
  | spec_prim_value_put_1 _ _ _ _ o => Some o

  | spec_put_value _ _ => None


  | spec_env_record_has_binding _ _ => None
  | spec_env_record_has_binding_1 _ _ _ => None
  | spec_env_record_get_binding_value _ _ _ => None
  | spec_env_record_get_binding_value_1 _ _ _ _ => None
  | spec_env_record_get_binding_value_2 _ _ _ o => Some o

  | spec_env_record_create_immutable_binding _ _ => None
  | spec_env_record_initialize_immutable_binding _ _ _ => None
  | spec_env_record_create_mutable_binding _ _ _ => None
  | spec_env_record_create_mutable_binding_1 _ _ _ _ => None
  | spec_env_record_create_mutable_binding_2 _ _ _ _ o => Some o
  | spec_env_record_create_mutable_binding_3 o => Some o
  | spec_env_record_set_mutable_binding _ _ _ _ => None
  | spec_env_record_set_mutable_binding_1 _ _ _ _ _ => None
  | spec_env_record_delete_binding _ _ => None
  | spec_env_record_delete_binding_1 _ _ _ => None

  | spec_env_record_create_set_mutable_binding _ _ _ _ _ => None
  | spec_env_record_create_set_mutable_binding_1 o _ _ _ _ => Some o

  | spec_env_record_implicit_this_value _ => None
  | spec_env_record_implicit_this_value_1 _ _ => None

  | spec_from_descriptor y => out_of_specret y
  | spec_from_descriptor_1 _ o => Some o
  | spec_from_descriptor_2 _ _ o => Some o
  | spec_from_descriptor_3 _ _ o => Some o
  | spec_from_descriptor_4 _ _ o => Some o
  | spec_from_descriptor_5 _ _ o => Some o
  | spec_from_descriptor_6 _ o => Some o


  | spec_entering_eval_code _ _ _ => None
  | spec_entering_eval_code_1 _ _ _ => None
  | spec_entering_eval_code_2 o _ => Some o

  | spec_call_global_eval _ _ => None
  | spec_call_global_eval_1 _ _ => None
  | spec_call_global_eval_2 _ => None
  | spec_call_global_eval_3 o => Some o

  | spec_entering_func_code _ _ _ _ => None
  | spec_entering_func_code_1 _ _ _ _ _ _ => None
  | spec_entering_func_code_2 _ _ _ o _ => Some o
  | spec_entering_func_code_3 _ _ _ _ _ _ => None
  | spec_entering_func_code_4 o _ => Some o

  | spec_binding_inst_formal_params _ _ _ _ => None
  | spec_binding_inst_formal_params_1 _ _ _ _ _ _ o => Some o
  | spec_binding_inst_formal_params_2 _ _ _ _ _ _ o => Some o
  | spec_binding_inst_formal_params_3 _ _ _ _ _ _ => None
  | spec_binding_inst_formal_params_4 _ _ _ _ o => Some o
  | spec_binding_inst_function_decls _ _ _ _ _ => None
  | spec_binding_inst_function_decls_1 _ _ _ _ _ _ o => Some o
  | spec_binding_inst_function_decls_2 _ _ _ _ _ _ _ o => Some o
  | spec_binding_inst_function_decls_3 _ _ _ _ _ _ y => out_of_specret y
  | spec_binding_inst_function_decls_3a _ _ _ _ _ _ _ => None
  | spec_binding_inst_function_decls_4 _ _ _ _ _ _ _ o => Some o
  | spec_binding_inst_function_decls_5 _ _ _ _ _ _ _ => None
  | spec_binding_inst_function_decls_6 _ _ _ _ _ o => Some o
  | spec_binding_inst_arg_obj   object_loc _ _ _ _ => None
  | spec_binding_inst_arg_obj_1 _ _ _ o => Some o
  | spec_binding_inst_arg_obj_2 _ _ _ o => Some o
  | spec_binding_inst_var_decls _ _ _ _ => None
  | spec_binding_inst_var_decls_1 _ _ _ _ _ o => Some o
  | spec_binding_inst_var_decls_2 _ _ _ _ o => Some o
  | spec_binding_inst _ _ _ _ => None
  | spec_binding_inst_1 _ _ _ _ _ => None
  | spec_binding_inst_2 _ _ _ _ _ _ o => Some o
  | spec_binding_inst_3 _ _ _ _ _ _ => None
  | spec_binding_inst_4 _ _ _ _ _ _ _ o => Some o
  | spec_binding_inst_5 _ _ _ _ _ _ _ => None
  | spec_binding_inst_6 _ _ _ _ _ _ _ o => Some o
  | spec_binding_inst_7 _ _ _ o => Some o
  | spec_binding_inst_8 _ _ _ => None
  
  | spec_make_arg_getter _ _ => None
  | spec_make_arg_setter _ _ => None
  
  | spec_args_obj_get_1 _ _ _ _ y => out_of_specret y
  
  
  | spec_args_obj_define_own_prop_1 _ _ _ _ _ y => out_of_specret y
  | spec_args_obj_define_own_prop_2 _ _ _ _ _ _ o => Some o
  | spec_args_obj_define_own_prop_3 _ _ _ _ _ o => Some o
  | spec_args_obj_define_own_prop_4 _ _ _ _ _ => None
  | spec_args_obj_define_own_prop_5 o => Some o
  | spec_args_obj_define_own_prop_6 => None
  
  | spec_args_obj_delete_1 _ _ _ _ y => out_of_specret y
  | spec_args_obj_delete_2 _ _ _ _ _ o => Some o
  | spec_args_obj_delete_3 o => Some o
  | spec_args_obj_delete_4 _ => None
  
  | spec_arguments_object_map _ _ _ _ _ => None
  | spec_arguments_object_map_1 _ _ _ _ _ o => Some o
  | spec_arguments_object_map_2 _ _ _ _ _ _ _ _ => None
  | spec_arguments_object_map_3 _ _ _ _ _ _ _ _ o => Some o
  | spec_arguments_object_map_4 _ _ _ _ _ _ _ _ _ => None
  | spec_arguments_object_map_5 _ _ _ _ _ _ _ _ _ o => Some o
  | spec_arguments_object_map_6 _ _ _ _ _ _ _ _ _ o => Some o
  | spec_arguments_object_map_7 _ _ _ _ _ _ _ _ o => Some o
  | spec_arguments_object_map_8 _ _ _  => None


  | spec_create_arguments_object _ _ _ _ _ => None
  | spec_create_arguments_object_1 _ _ _ _ _ _ o => Some o
  | spec_create_arguments_object_2 _ _ _ o => Some o
  | spec_create_arguments_object_3 _ _ _ o => Some o
  | spec_create_arguments_object_4 _ o => Some o

  | spec_object_has_instance _ _ => None
  | spec_object_has_instance_1 _ _ _ => None
  | spec_function_has_instance_1 _ o => Some o
  | spec_function_has_instance_2 _ _ => None
  | spec_function_has_instance_3 _ _ => None
  | spec_function_has_instance_after_bind_1 _ _ => None
  | spec_function_has_instance_after_bind_2 _ _ => None 

  | spec_function_get_1 _ _ o => Some o

  | spec_error _ => None
  | spec_error_1 o => Some o 
  | spec_error_or_cst _ _ _ => None
  | spec_error_or_void _ _ => None

  | spec_init_throw_type_error => None
  | spec_init_throw_type_error_1 o => Some o

  | spec_build_error _ _ => None
  | spec_build_error_1 _ _ => None
  | spec_build_error_2 _ o => Some o

  | spec_new_object _ => None
  | spec_new_object_1 o _ => Some o

  | spec_prim_new_object _ => None

  | spec_creating_function_object_proto _ => None
  | spec_creating_function_object_proto_1 _ o => Some o
  | spec_creating_function_object_proto_2 _ _ o => Some o

  | spec_creating_function_object _ _ _ _ => None
  | spec_creating_function_object_1 _ _ o => Some o
  | spec_creating_function_object_2 _ _ o => Some o
  | spec_creating_function_object_3 _ o => Some o
  | spec_creating_function_object_4 _ o => Some o

  | spec_function_proto_apply _ _ _ => None
  | spec_function_proto_apply_1 _ _ _ o => Some o  
  | spec_function_proto_apply_2 _ _ _ y => out_of_specret y
  | spec_function_proto_apply_3 _ _ y => out_of_specret y

  | spec_function_proto_bind_1 _ _ _ => None 
  | spec_function_proto_bind_2 _ _ _ => None 
  | spec_function_proto_bind_3 _ y => out_of_specret y 
  | spec_function_proto_bind_4 _ _ => None 
  | spec_function_proto_bind_5 _ => None
  | spec_function_proto_bind_6 _ o => Some o 
  | spec_function_proto_bind_7 _ o => Some o

  | spec_create_new_function_in  execution_ctx _ _ => None

  | spec_call _ _ _ => None
  | spec_call_1 _ _ _ _ => None

  | spec_call_prealloc _ _ _ => None

  | spec_call_default _ _ _ => None
  | spec_call_default_1 _ => None
  | spec_call_default_2 _ => None
  | spec_call_default_3 o => Some o

  | spec_construct _ _ => None
  | spec_construct_1 _ _ _ => None

  | spec_construct_prealloc _ _ => None

  | spec_construct_default _ _ => None
  | spec_construct_default_1 _ _ o => Some o
  | spec_construct_default_2 _ o => Some o

  | spec_construct_1_after_bind _ _ _ => None

  | spec_construct_bool_1 o => Some o

  | spec_construct_number_1 o => Some o

  | spec_call_global_is_nan_1 o => Some o
  | spec_call_global_is_finite_1 o => Some o

  | spec_call_object_call_1 _ _ => None
  | spec_call_object_new_1 _ => None
  | spec_call_object_get_proto_of_1 _ => None
  | spec_call_object_is_extensible_1 _ => None

  | spec_call_object_define_props_1 _ _ => None
  | spec_call_object_define_props_2 o _ => Some o
  | spec_call_object_define_props_3 _ _ _ _ => None
  | spec_call_object_define_props_4 o _ _ _ _ _ => Some o
  | spec_call_object_define_props_5 _ _ _ _ _ y => out_of_specret y
  | spec_call_object_define_props_6 _ _ => None
  | spec_call_object_define_props_7 o _ _ => Some o

  | spec_call_object_create_1 _ _ => None
  | spec_call_object_create_2 o _ _ => Some o
  | spec_call_object_create_3 _ _ => None

  | spec_call_object_seal_1 _ => None
  | spec_call_object_seal_2 _ _ => None
  | spec_call_object_seal_3 _ _ _ y => out_of_specret y
  | spec_call_object_seal_4 _ _ o => Some o

  | spec_call_object_is_sealed_1 _ => None
  | spec_call_object_is_sealed_2 _ _ => None
  | spec_call_object_is_sealed_3 _ _ y => out_of_specret y

  | spec_call_object_freeze_1 _ => None
  | spec_call_object_freeze_2 _ _ => None
  | spec_call_object_freeze_3 _ _ _ y => out_of_specret y
  | spec_call_object_freeze_4 _ _ _ _ => None
  | spec_call_object_freeze_5 _ _ o => Some o

  | spec_call_object_is_frozen_1 _ => None
  | spec_call_object_is_frozen_2 _ _ => None
  | spec_call_object_is_frozen_3 _ _ y => out_of_specret y
  | spec_call_object_is_frozen_4 _ _ _ => None
  | spec_call_object_is_frozen_5 _ _ _ => None


  | spec_call_object_prevent_extensions_1 _ => None

  | spec_call_object_define_prop_1 _ _ _ => None
  | spec_call_object_define_prop_2 _ o _ => Some o
  | spec_call_object_define_prop_3 _ _ y => out_of_specret y
  | spec_call_object_define_prop_4 _ o => Some o

  | spec_call_object_get_own_prop_descriptor_1 _ _ => None
  | spec_call_object_get_own_prop_descriptor_2 _ o => Some o

  | spec_call_object_proto_to_string_1 _ => None
  | spec_call_object_proto_to_string_2 o => Some o

  | spec_call_object_proto_has_own_prop_1 o _ => Some o
  | spec_call_object_proto_has_own_prop_2 o _ => Some o
  | spec_call_object_proto_has_own_prop_3 y => out_of_specret y

  | spec_call_object_proto_is_prototype_of_2_1 _ _ => None
  | spec_call_object_proto_is_prototype_of_2_2 o _ => Some o
  | spec_call_object_proto_is_prototype_of_2_3 _ _ => None
  | spec_call_object_proto_is_prototype_of_2_4 _ _ => None

  | spec_call_object_proto_prop_is_enumerable_1 _ _ => None
  | spec_call_object_proto_prop_is_enumerable_2 _ o => Some o
  | spec_call_object_proto_prop_is_enumerable_3 o _ => Some o
  | spec_call_object_proto_prop_is_enumerable_4 y => out_of_specret y

  | spec_call_array_new_1 _ => None
  | spec_call_array_new_2 _ _ => None
  | spec_call_array_new_3 _ _ _ => None

  | spec_call_array_new_single_1 _ => None
  | spec_call_array_new_single_2 _ _ => None
  | spec_call_array_new_single_3 _ _ y => out_of_specret y
  | spec_call_array_new_single_4 _ _ => None
  
  | spec_call_array_is_array_1 _ => None
  | spec_call_array_is_array_2_3 _ => None

  | spec_call_array_proto_join   o _ => Some o 
  | spec_call_array_proto_join_1 _ o _ => Some o 
  | spec_call_array_proto_join_2 _ y _ => out_of_specret y
  | spec_call_array_proto_join_3 _ _ _ => None
  | spec_call_array_proto_join_4 _ _ o => Some o
  | spec_call_array_proto_join_5 _ _ _ y => out_of_specret y

  | spec_call_array_proto_join_elements _ _ _ _ _ => None
  | spec_call_array_proto_join_elements_1 _ _ _ _ _ => None
  | spec_call_array_proto_join_elements_2 _ _ _ _ _ y => out_of_specret y

  | spec_call_array_proto_to_string o => Some o
  | spec_call_array_proto_to_string_1 _ o => Some o

  | spec_call_array_proto_pop_1 o => Some o
  | spec_call_array_proto_pop_2 _ o => Some o
  | spec_call_array_proto_pop_3 _ y  => out_of_specret y
  | spec_call_array_proto_pop_3_empty_1 _ => None
  | spec_call_array_proto_pop_3_empty_2 o => Some o
  | spec_call_array_proto_pop_3_nonempty_1 _ _ => None
  | spec_call_array_proto_pop_3_nonempty_2 _ o => Some o
  | spec_call_array_proto_pop_3_nonempty_3 _ _ o => Some o
  | spec_call_array_proto_pop_3_nonempty_4 _ _ _ o => Some o
  | spec_call_array_proto_pop_3_nonempty_5 _ o => Some o

  | spec_call_array_proto_push_1 o _ => Some o
  | spec_call_array_proto_push_2 _ _ o => Some o
  | spec_call_array_proto_push_3 _ _ y => out_of_specret y
  | spec_call_array_proto_push_4 _ _ _ => None
  | spec_call_array_proto_push_4_nonempty_1 _ _ _ _ => None
  | spec_call_array_proto_push_4_nonempty_2 _ _ _ _ o => Some o
  | spec_call_array_proto_push_4_nonempty_3 _ _ _ _ o => Some o
  | spec_call_array_proto_push_5 _ _ => None
  | spec_call_array_proto_push_6 _ o => Some o

  | spec_call_string_non_empty o => Some o 

  | spec_construct_string_1 _ => None
  | spec_construct_string_2 o => Some o

  | spec_call_bool_proto_to_string_1 o => Some o
  | spec_call_bool_proto_value_of_1 _ => None
  | spec_call_bool_proto_value_of_2 _ => None

  | spec_call_number_proto_to_string_1 _ _ => None
  | spec_call_number_proto_to_string_2 _ o => Some o
  | spec_call_number_proto_value_of_1 _ => None

  | spec_call_error_proto_to_string_1 _ => None
  | spec_call_error_proto_to_string_2 _ o => Some o
  | spec_call_error_proto_to_string_3 _ o => Some o
  | spec_call_error_proto_to_string_4 _ _ o => Some o
  | spec_call_error_proto_to_string_5 _ _ o => Some o
  | spec_call_error_proto_to_string_6 _ _ o => Some o

  | spec_returns o => Some o
  end.

Definition out_of_ext_stat (p : ext_stat) : option out :=
  match p with
  | stat_expr_1 (specret_out o) => Some o
  | stat_expr_1 (specret_val _ _) => None
  | stat_basic _ => None

  | stat_block_1 o _ => Some o
  | stat_block_2 _ o => Some o

  | stat_label_1 _ o => Some o

  | stat_var_decl_1 o _ => Some o
  | stat_var_decl_item _ => None
  | stat_var_decl_item_1 _ y _ => out_of_specret y
  | stat_var_decl_item_2 _ _ y => out_of_specret y
  | stat_var_decl_item_3 _ o => Some o

  | stat_if_1 y _ _ => out_of_specret y

  | stat_while_1 _ _ _ _ => None
  | stat_while_2 _ _ _ _ y => out_of_specret y
  | stat_while_3 _ _ _ _ o => Some o
  | stat_while_4 _ _ _ _ _ => None
  | stat_while_5 _ _ _ _ _ => None
  | stat_while_6 _ _ _ _ _ => None

  | stat_do_while_1 _ _ _ _ => None
  | stat_do_while_2 _ _ _ _ o => Some o
  | stat_do_while_3 _ _ _ _ _ => None
  | stat_do_while_4 _ _ _ _ _ => None
  | stat_do_while_5 _ _ _ _ _ => None
  | stat_do_while_6 _ _ _ _ => None
  | stat_do_while_7 _ _ _ _ y => out_of_specret y

  | stat_for_1 _ y _ _ _ => out_of_specret y
  | stat_for_2 _ _ _ _ _ => None
  | stat_for_3 _ _ _ y _ _ => out_of_specret y
  | stat_for_4 _ _ _ _ _ => None
  | stat_for_5 _ _ _ o _ _ => Some o
  | stat_for_6 _ _ _ _ _ _ => None
  | stat_for_7 _ _ _ _ _ _ => None
  | stat_for_8 _ _ _ _ _ => None
  | stat_for_9 _ _ _ _ y _ => out_of_specret y
  | stat_for_var_1 o _ _ _ _ => Some o

  | stat_with_1 _ y => out_of_specret y

  | stat_throw_1 y => out_of_specret y

  | stat_return_1 y => out_of_specret y

  | stat_try_1 o _ _ => Some o
  | stat_try_2 o _ _ _ => Some o
  | stat_try_3 o _ => Some o
  | stat_try_4 _ _ => None
  | stat_try_5 _ o => Some o

  | stat_switch_1 y _ _ => out_of_specret y
  | stat_switch_2 o _ => Some o
  | stat_switch_nodefault_1 _ _ _=> None
  | stat_switch_nodefault_2 y _ _ _ _  => out_of_specret y
  | stat_switch_nodefault_3 _ _ _ _ _ => None
  | stat_switch_nodefault_4 o _ => Some o
  | stat_switch_nodefault_5 _ _ => None
  | stat_switch_nodefault_6 _ o _ => Some o

  | stat_switch_default_1 _ _ _ _ _ => None
  | stat_switch_default_A_1 _ _ _ _ _ _ => None 
  | stat_switch_default_A_2 y _ _ _ _ _ _ => out_of_specret y
  | stat_switch_default_A_3 _ _ _ _ _ _ _  => None
  | stat_switch_default_A_4 _ _ _ _ _ _ => None
  | stat_switch_default_A_5 _ o _ _ _ _ => Some o
  | stat_switch_default_B_1 _ _ _ _ => None
  | stat_switch_default_B_2 y _ _ _ _ _ => out_of_specret y
  | stat_switch_default_B_3 _ _ _ _ _ _ => None
  | stat_switch_default_B_4 o _ _ => Some o

  | stat_switch_default_5 _ _ _ _ => None
  | stat_switch_default_6 o _ => Some o 
  | stat_switch_default_7 _ _  => None
  | stat_switch_default_8 _ o _ => Some o

  end.

Definition out_of_ext_prog (p : ext_prog) : option out :=
  match p with
  | prog_basic _ => None
  | javascript_1 o _ => Some o
  | prog_1 o _ => Some o
  | prog_2 _ o => Some o

  end.

Definition out_of_ext_spec (es : ext_spec) : option out :=
  match es with
  | spec_to_int32 _ => None
  | spec_to_int32_1 o => Some o
  | spec_to_uint32 _ => None
  | spec_to_uint32_1 o => Some o
  | spec_expr_get_value_conv _ _ => None
  | spec_expr_get_value_conv_1 _ y => out_of_specret y
  | spec_expr_get_value_conv_2 o => Some o
  | spec_convert_twice _ _ => None
  | spec_convert_twice_1 o _ => Some o
  | spec_convert_twice_2 _ o => Some o
  | spec_list_expr _ => None
  | spec_list_expr_1 _ _ => None
  | spec_list_expr_2 _ y _ => out_of_specret y
  | spec_to_descriptor _ => None
  | spec_to_descriptor_1a _ _ => None
  | spec_to_descriptor_1b o _ _ => Some o
  | spec_to_descriptor_1c o _ _ => Some o
  | spec_to_descriptor_2a _ _ => None
  | spec_to_descriptor_2b o _ _ => Some o
  | spec_to_descriptor_2c o _ _ => Some o
  | spec_to_descriptor_3a _ _ => None
  | spec_to_descriptor_3b o _ _ => Some o
  | spec_to_descriptor_3c o _ _ => Some o
  | spec_to_descriptor_4a _ _ => None
  | spec_to_descriptor_4b o _ _ => Some o
  | spec_to_descriptor_4c o _ _ => Some o
  | spec_to_descriptor_5a _ _ => None
  | spec_to_descriptor_5b o _ _ => Some o
  | spec_to_descriptor_5c o _ _ => Some o
  | spec_to_descriptor_6a _ _ => None
  | spec_to_descriptor_6b o _ _=> Some o
  | spec_to_descriptor_6c o _ _ => Some o
  | spec_to_descriptor_7 _ _ => None
  | spec_object_get_own_prop _ _ => None
  | spec_object_get_own_prop_1 _ _ _ => None
  | spec_object_get_own_prop_2 _ _ _ => None
  | spec_object_get_prop _ _ => None
  | spec_object_get_prop_1 _ _ _ => None
  | spec_object_get_prop_2 _ _ y => out_of_specret y
  | spec_object_get_prop_3 _ _ _ => None
  | spec_get_value _ => None
  | spec_get_value_ref_b_1 o => Some o
  | spec_get_value_ref_c_1 o => Some o
  | spec_expr_get_value _ => None
  | spec_expr_get_value_1 o => Some o
  | spec_lexical_env_get_identifier_ref _ _ _ => None
  | spec_lexical_env_get_identifier_ref_1 _ _ _ _ => None
  | spec_lexical_env_get_identifier_ref_2 _ _ _ _ o => Some o
  | spec_error_spec _ => None
  | spec_error_spec_1 o => Some o
  | spec_args_obj_get_own_prop_1 _ _ y => out_of_specret y
  | spec_args_obj_get_own_prop_2 _ _ _ _ y => out_of_specret y
  | spec_args_obj_get_own_prop_3 _ o => Some o
  | spec_args_obj_get_own_prop_4 _ => None
  | spec_string_get_own_prop_1 _ _ y => out_of_specret y
  | spec_string_get_own_prop_2 _ _ y => out_of_specret y
  | spec_string_get_own_prop_3 _ _ o => Some o
  | spec_string_get_own_prop_4 _ _ => None
  | spec_string_get_own_prop_5 _ y => out_of_specret y
  | spec_string_get_own_prop_6 _ _ _ => None
  | spec_function_proto_apply_get_args _ _ _ => None
  | spec_function_proto_apply_get_args_1 _ _ _ o => Some o 
  | spec_function_proto_apply_get_args_2 _ _ _ o => Some o 
  | spec_function_proto_apply_get_args_3 _ y => out_of_specret y
  | spec_function_proto_bind_length _ _ => None
  | spec_function_proto_bind_length_1 _ _ => None
  | spec_function_proto_bind_length_2 _ o => Some o
  | spec_function_proto_bind_length_3 y _ => out_of_specret y
  | spec_call_array_proto_join_vtsfj _ _ => None
  | spec_call_array_proto_join_vtsfj_1 _ o => Some o 
  | spec_call_array_proto_join_vtsfj_2 _ o => Some o 
  | spec_call_array_proto_join_vtsfj_3 o => Some o 
  end.


(**************************************************************)
(** ** Rules for propagating aborting expressions *)

(** Definition of a result of type normal *)

Definition res_is_normal R :=
  res_type R = restype_normal.

(** Definition of aborting outcomes: diverging outcomes,
    and terminating outcomes that are not of type "normal". *)

Inductive abort : out -> Prop :=
  | abort_div :
      abort out_div
  | abort_not_normal : forall S R,
      abrupt_res R ->
      abort (out_ter S R).

(** Definition of the behaviors caught by an exception handler,
    and thus not propagated by the generic abort rule *)

Inductive abort_intercepted_prog : ext_prog -> Prop :=
  | abort_intercepted_prog_block_2 : forall S R rv,
      abort_intercepted_prog (prog_2 rv (out_ter S R)).

Inductive abort_intercepted_stat : ext_stat -> Prop :=

  | abort_intercepted_stat_block_2 : forall S R rv,
      abort_intercepted_stat (stat_block_2 rv (out_ter S R))
  | abort_intercepted_stat_label_1 : forall lab rv S R,
      R = res_intro restype_break rv lab ->
      abort_intercepted_stat (stat_label_1 lab (out_ter S R))
  | abort_intercepted_do_while_2 : forall labs e1 t2 rv S R,
      res_label_in R labs ->
      (res_type R = restype_continue \/ res_type R = restype_break) ->
      abort_intercepted_stat (stat_do_while_2 labs t2 e1 rv (out_ter S R))
  | abort_intercepted_while_3 : forall labs e1 t2 rv S R,
      res_label_in R labs ->
      (res_type R = restype_continue \/ res_type R = restype_break) ->
      abort_intercepted_stat (stat_while_3 labs e1 t2 rv (out_ter S R))
  | abort_intercepted_stat_try_1 : forall S R cb fo,
      res_type R = restype_throw ->
      abort_intercepted_stat (stat_try_1 (out_ter S R) (Some cb) fo)
  | abort_intercepted_stat_try_3 : forall S R fo,
      abort_intercepted_stat (stat_try_3 (out_ter S R) fo)
  | abort_intercepted_stat_switch_2 : forall S R labs,
      res_type R = restype_break ->
      res_label_in R labs ->  
      abort_intercepted_stat (stat_switch_2 (out_ter S R) labs)
  | abort_intercepted_stat_switch_nodefault_6 : forall S rv R scs,
      abrupt_res R ->
      res_type R <> restype_throw -> 
      abort_intercepted_stat (stat_switch_nodefault_6 rv (out_ter S R) scs)
  | abort_intercepted_stat_switch_default_8 : forall S rv R scs,
      abrupt_res R ->
      res_type R <> restype_throw ->
      abort_intercepted_stat (stat_switch_default_8 rv (out_ter S R) scs)
  | abort_intercepted_stat_switch_default_A_5 : forall S rv R vi scs ts1 scs2,
      abrupt_res R ->
      res_type R <> restype_throw ->
      abort_intercepted_stat (stat_switch_default_A_5 rv (out_ter S R) vi scs ts1 scs2)
   | abort_intercepted_stat_for_6 : forall S0 S C labs rv R eo2 eo3 t,
      abort_intercepted_stat (stat_for_6 labs rv eo2 eo3 t R)
   | abort_intercepted_stat_for_7 : forall S0 S C labs rv R eo2 eo3 t,
      abort_intercepted_stat (stat_for_7 labs rv eo2 eo3 t R)
.

Inductive abort_intercepted_expr : ext_expr -> Prop :=
  | abort_intercepted_expr_call_default_2 : forall S R,
      res_type R = restype_return ->
      abort_intercepted_expr (spec_call_default_3 (out_ter S R))
  | abort_intercepted_expr_call_global_eval_3 : forall S R,
      res_type R = restype_throw ->
      abort_intercepted_expr (spec_call_global_eval_3 (out_ter S R)).

Inductive abort_intercepted_spec : ext_spec -> Prop :=
  .

(**************************************************************)
(** ** Auxiliary definition used in identifier resolution *)

(** [spec_identifier_resolution C x] returns the extended expression
    which needs to be evaluated in order to perform the lookup
    of name [x] in the execution context [C]. Typically, a
    reduction rule that performs such a lookup would have a
    premise of the form [red_expr S C (identifier_resolution C x) o1]. *)

Definition spec_identifier_resolution C x :=
  let lex := execution_ctx_lexical_env C in
  let strict := execution_ctx_strict C in
  spec_lexical_env_get_identifier_ref lex x strict.


(**************************************************************)
(** ** Instantiation of arguments in function calls *)

Inductive arguments_from : list value -> list value -> Prop :=
 | arguments_from_nil : forall Vs,
      arguments_from Vs nil
 | arguments_from_undef : forall Vs: list value,
      arguments_from nil Vs ->
      arguments_from nil (undef::Vs)
 | arguments_from_cons : forall Vs1 Vs2 v,
      arguments_from Vs1 Vs2 ->
      arguments_from (v::Vs1) (v::Vs2).

Inductive arguments_first_and_rest : list value -> (value * list value) -> Prop :=
 | arguments_f_a_r_from_nil  : arguments_first_and_rest nil (undef, nil)
 | arguments_f_a_r_from_cons : forall v lv, 
     arguments_first_and_rest (v :: lv) (v, lv).

Hint Constructors arguments_first_and_rest.

(**************************************************************)
(** ** Rules for delete_events. *)

(** [search_proto_chain S l x] returns the location l' of the first object 
    in the prototype chain of l which contains property x. *)

Inductive search_proto_chain : state -> object_loc -> prop_name -> option object_loc -> Prop :=
  | search_proto_chain_found : forall S l x,
                                 object_has_property S l x ->
                                 search_proto_chain S l x (Some l)
  | search_proto_chain_not_found : forall S l x,
                                     not (object_has_property S l x) ->
                                     object_proto S l prim_null ->
                                     search_proto_chain S l x None
  | search_proto_chain_inductive : forall S l x v l' res,
                                     not (object_has_property S l x) ->
                                     object_proto S l (value_object l') ->
                                     search_proto_chain S l' x res ->
                                     search_proto_chain S l x res.


(** [make_delete_event S l x ev] constructs a delete_event "ev" which
records the deletion of the property (l,x) in the state S. *)

Inductive make_delete_event : state -> object_loc -> prop_name -> event -> Prop :=
  | make_delete_event_intro : forall S l x res ev,
                                search_proto_chain S l x res ->
                                ev = delete_event l x res ->
                                make_delete_event S l x ev.

(**************************************************************)
(** ** Auxiliary definitions for the semantics of for-in. *)

(* LATER *)


(**************************************************************)
(** ** Implementation Defined Object *)

(** As stated in Section 2 of the ECMAScript specification, an
  implementation can provide additionnal properties not described in
  the specification. **)

(** As we are only describing the core of JavaScrip here, this
  inductive shall be empty.  But one can found in the other branches
  of this developpment some examples of non-empty instantiation of
  this predicate. **)

Inductive implementation_prealloc : prealloc -> Prop :=
  .


(**************************************************************)
(** Shorthand **)

Definition vret : state -> value -> specret value := ret (T:=value).
Definition dret : state -> full_descriptor -> specret full_descriptor := ret (T:=full_descriptor).

