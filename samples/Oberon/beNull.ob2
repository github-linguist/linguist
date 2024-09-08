(** Головной модуль сбора статистики *)

MODULE beNull;  (* Empty back-end *)

IMPORT 	pc :=pcK,
	xfs:=xiFiles,
	env:=xiEnv,
	SYSTEM;

TYPE
  CODE = POINTER TO code_rec;
  code_rec = RECORD (pc.code_rec)
	     END;

PROCEDURE val(x: LONGINT): pc.VALUE;
  VAR v: pc.VALUE;
BEGIN
  v:=pc.value.new(env.null_pos,pc.ZZ_type);
  v.set_integer(x);
  RETURN v;
END val;

(*PROCEDURE rval(x: LONGREAL): pc.VALUE;
  VAR v: pc.VALUE;
BEGIN
  v:=pc.value.new(env.null_pos,pc.RR_type);
  v.set_real(x);
  RETURN v;
END rval;
*)
PROCEDURE (c: CODE) ini;
BEGIN
  c.bits_per_loc :=8;
  c.locs_per_word:=4;

  c.FRETs:=pc.REALs+pc.WHOLEs+pc.CPLXs+
		pc.TY_SET{pc.ty_boolean,pc.ty_enum,pc.ty_range,pc.ty_char}+
		pc.TY_SET{pc.ty_array,pc.ty_loc}+
		pc.TY_SET{pc.ty_opaque,pc.ty_pointer}+
		pc.TY_SET{pc.ty_protection,pc.ty_proctype,pc.ty_record};

(*  c.max_loc :=val(ASH(1,c.bits_per_loc)-1);
  c.max_sint:=val(MAX(SHORTINT));
  c.max_int :=val(MAX(INTEGER));
  c.max_lint:=val(MAX(LONGINT));
  c.min_sint:=val(MIN(SHORTINT));
  c.min_int :=val(MIN(INTEGER));
  c.min_lint:=val(MIN(LONGINT));

  c.max_scard:=val(0);
  c.max_scard.binary(pc.sb_minus,c.max_sint,c.min_sint);
  c.max_card :=val(0);
  c.max_card .binary(pc.sb_minus,c.max_int,c.min_int);
  c.max_lcard:=val(0);
  c.max_lcard.binary(pc.sb_minus,c.max_lint,c.min_lint);

  c.max_char :=val(255);
  c.max_real :=rval(MAX(REAL));
  c.max_lreal:=rval(MAX(LONGREAL));
  c.min_real :=rval(MIN(REAL));
  c.min_lreal:=rval(MIN(LONGREAL));
*)
  c.max_dim:=10;
  c.max_ext_lev:=25;
  c.def_storage:=TRUE;
  c.max_sysflag:=pc.flag_c;

  c.int16:=FALSE;
  c.index16:=FALSE;
  c.address16:=TRUE;
  c.max_index:=val(MAX(LONGINT));
END ini;

PROCEDURE (c: CODE) exi;
BEGIN
END exi;

PROCEDURE (c: CODE) gen_code(cu: pc.Mno; main: BOOLEAN);
BEGIN
END gen_code;

PROCEDURE (c: CODE) allocate(cu: pc.Mno; main: BOOLEAN; src_time: SYSTEM.CARD32);
BEGIN
END allocate;

PROCEDURE (c: CODE) get_size(op: pc.SUB_MODE; t: pc.STRUCT): LONGINT;
BEGIN
  RETURN -1;
END get_size;

PROCEDURE (c: CODE) inp_object(file: xfs.SymFile; o: pc.OBJECT; id: LONGINT);
BEGIN
END inp_object;

PROCEDURE (c: CODE) skip_object(file: xfs.SymFile; id: LONGINT);
BEGIN
END skip_object;

PROCEDURE (c: CODE) inp_struct(file: xfs.SymFile; s: pc.STRUCT; id: LONGINT);
BEGIN
END inp_struct;

PROCEDURE (c: CODE) skip_struct(file: xfs.SymFile; id: LONGINT);
BEGIN
END skip_struct;

PROCEDURE Set*;
  VAR code: CODE;
BEGIN
  env.config.NewOption("GENSTAT",FALSE,SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewOption("GENSTATONLY",FALSE,SYSTEM.VAL(env.CompilerOption,-1));
  env.config.NewEquation("STATEXT");
  env.config.SetEquation("STATEXT","0st");
  NEW(code);
  pc.code:=code;
  code.sym_ident:=pc.sym_C;
  code.valid_idents := { pc.sym_C          - pc.sym_base
                       , pc.sym_native     - pc.sym_base
                       , pc.sym_native + 1 - pc.sym_base };
  code.vers:="NULL";
  env.config.Equation("OBJEXT",code.code_ext);
END Set;

END beNull.
