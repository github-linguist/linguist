'' intermediate representation - core module
''
'' chng: dec/2006 written [v1ctor]

#include once "fb.bi"
#include once "fbint.bi"
#include once "ir.bi"
#include once "emit.bi"
#include once "ir-private.bi"

dim shared ir as IRCTX

sub irInit( )
	select case( env.clopt.backend )
	case FB_BACKEND_GCC
		ir.vtbl = irhlc_vtbl
	case FB_BACKEND_LLVM
		ir.vtbl = irllvm_vtbl
	case FB_BACKEND_GAS64
		ir.vtbl = irgas64_vtbl
	case else
		assert( env.clopt.backend = FB_BACKEND_GAS )
		ir.vtbl = irtac_vtbl
	end select

	'' reset ir.options becasue irSetOption() will merge (OR) values
	ir.options = 0

	ir.vtbl.init( )
end sub

sub irEnd( )
	ir.vtbl.end( )

	ir.options = 0

	#if __FB_DEBUG__
		'' debugging - reset the vtable - shouldn't matter in production
		'' because ir.vtbl calls should never be called outside irInit()/irEnd()
		dim null_vtbl as IR_VTBL
		ir.vtbl = null_vtbl
	#endif

end sub

dim shared irhl as IRHLCONTEXT

sub irhlInit( )
	flistInit( @irhl.vregs, IR_INITVREGNODES, sizeof( IRVREG ) )
	listInit( @irhl.callargs, 32, sizeof( IRCALLARG ), LIST_FLAGS_NOCLEAR )
end sub

sub irhlEnd( )
	listEnd( @irhl.callargs )
	flistEnd( @irhl.vregs )
end sub

sub irhlEmitProcBegin( )
	irhl.regcount = 0
end sub

sub irhlEmitProcEnd( )
	flistReset( @irhl.vregs )
end sub

sub irhlEmitPushArg _
	( _
		byval param as FBSYMBOL ptr, _
		byval vr as IRVREG ptr, _
		byval udtlen as longint, _
		byval level as integer, _
		byval lreg as IRVREG ptr _ _
	)

	'' Remember for later, so during _emitCall[Ptr] we can emit the whole
	'' call in one go
	dim as IRCALLARG ptr arg = listNewNode( @irhl.callargs )
	arg->param = param
	arg->vr = vr
	arg->level = level

	'' ignore udtlen, it's only used by ir-tac.bas:_emitPushArg()
	'' ignore lreg, it's only used by ir-tac.bas:_emitPushArg()

end sub

function irhlNewVreg _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval vtype as integer _
	) as IRVREG ptr

	dim as IRVREG ptr v = any

	v = flistNewItem( @irhl.vregs )

	v->typ = vtype
	v->dtype = dtype
	v->subtype = subtype
	if( vtype = IR_VREGTYPE_REG ) then
		v->reg = irhl.regcount
		irhl.regcount += 1
	else
		v->reg = INVALID
	end if
	v->regFamily = 0
	v->vector = 0
	v->sym = NULL
	v->ofs = 0
	v->mult = 0
	v->vidx = NULL
	v->vaux = NULL

	function = v
end function

function irhlAllocVreg _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr _
	) as IRVREG ptr
	function = irhlNewVreg( dtype, subtype, IR_VREGTYPE_REG )
end function

function irhlAllocVrImm _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval value as longint _
	) as IRVREG ptr

	dim as IRVREG ptr vr = any

	vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_IMM )
	vr->value.i = value

	function = vr
end function

function irhlAllocVrImmF _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval value as double _
	) as IRVREG ptr

	dim as IRVREG ptr vr = any

	vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_IMM )
	vr->value.f = value

	function = vr
end function

function irhlAllocVrVar _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval symbol as FBSYMBOL ptr, _
		byval ofs as longint _
	) as IRVREG ptr

	dim as IRVREG ptr vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_VAR )

	vr->sym = symbol
	vr->ofs = ofs

	function = vr
end function

function irhlAllocVrIdx _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval symbol as FBSYMBOL ptr, _
		byval ofs as longint, _
		byval mult as integer, _
		byval vidx as IRVREG ptr _
	) as IRVREG ptr

	dim as IRVREG ptr vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_IDX )

	vr->sym = symbol
	vr->ofs = ofs
	vr->vidx = vidx

	function = vr
end function

function irhlAllocVrPtr _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval ofs as longint, _
		byval vidx as IRVREG ptr _
	) as IRVREG ptr

	dim as IRVREG ptr vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_PTR )

	vr->ofs = ofs
	vr->vidx = vidx

	function = vr
end function

function irhlAllocVrOfs _
	( _
		byval dtype as integer, _
		byval subtype as FBSYMBOL ptr, _
		byval symbol as FBSYMBOL ptr, _
		byval ofs as longint _
	) as IRVREG ptr

	dim as IRVREG ptr vr = irhlNewVreg( dtype, subtype, IR_VREGTYPE_OFS )

	vr->sym = symbol
	vr->ofs = ofs

	function = vr
end function

'' DATA descriptor arrays must be emitted based on the order indicated by the
'' FBSYMBOL.var_.data.prev linked list, not in the symtb order.
sub irForEachDataStmt( byval callback as sub( byval as FBSYMBOL ptr ) )
	var sym = astGetLastDataStmtSymbol( )
	while( sym )
		callback( sym )
		sym = sym->var_.data.prev
	wend
end sub

sub irhlFlushStaticInitializer( byval sym as FBSYMBOL ptr )
	astLoadStaticInitializer( symbGetTypeIniTree( sym ), sym )
	symbSetTypeIniTree( sym, NULL )
end sub

#if __FB_DEBUG__
function vregDumpToStr( byval v as IRVREG ptr ) as string
	dim as string s
	dim as string regname

	if( v = NULL ) then
		return "<NULL>"
	end if

	static as zstring ptr vregtypes(IR_VREGTYPE_IMM to IR_VREGTYPE_OFS) = _
	{ _
		@"imm", @"var", @"idx", @"ptr", @"reg", @"ofs" _
	}

	#if 0
		s += "[" + hex( v, 8 ) + "] "
	#endif

	s += *vregtypes(v->typ)

	select case( v->typ )
	case IR_VREGTYPE_IMM
		s += " "
		if( typeGetClass( v->dtype ) = FB_DATACLASS_FPOINT ) then
			s += str( v->value.f )
		else
			s += str( v->value.i )
		end if

	case IR_VREGTYPE_REG
		if( env.clopt.backend = FB_BACKEND_GAS ) then
			regname = emitDumpRegName( v->dtype, v->reg )
			if( len( regname ) > 0 ) then
				s += " " + ucase( regname )
			else
				s += " " + str( v->reg )
			end if
		else
			''s += " reg="
			s += " " + str( v->reg )
		end if
	end select

	if( v->sym ) then
		s += " " + *symbGetName( v->sym )
	end if

	if( v->typ <> IR_VREGTYPE_REG ) then
		if( v->ofs ) then
			if( (env.clopt.backend = FB_BACKEND_GAS) and (v->sym <> NULL) ) then
				s += " [" + *symbGetMangledName( v->sym )
				if( v->ofs >= 0 ) then
					s += "+"
				end if
				s += str( v->ofs )
				s += "]"
			else
				s += " ofs=" + str( v->ofs )
			end if
		end if
		if( v->mult ) then
			s += " mult=" + str( v->mult )
		end if
	end if

	s += " " + typeDumpToStr( v->dtype, v->subtype )

	if( v->typ <> IR_VREGTYPE_REG ) then
		if( v->vidx ) then
			s += " vidx=<" + vregDumpToStr( v->vidx ) + ">"
		end if
	end if

	'' If it's a longint vreg, show vaux
	'' ASM backend: uses vaux, so always show it
	'' C/LLVM backends: don't use vaux, so only show it if it's set
	if( ISLONGINT( v->dtype ) and _
	    ((env.clopt.backend = FB_BACKEND_GAS) or (v->vaux <> NULL)) ) then
		s += " vaux=<" + vregDumpToStr( v->vaux ) + ">"
	end if

	function = s
end function

sub vregDump( byval v as IRVREG ptr )
	print vregDumpToStr( v )
end sub

#endif
