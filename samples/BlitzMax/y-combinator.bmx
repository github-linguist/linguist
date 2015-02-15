SuperStrict

'Boxed type so we can just use object arrays for argument lists
Type Integer
	Field val:Int
	Function Make:Integer(_val:Int)
		Local i:Integer = New Integer
		i.val = _val
		Return i
	End Function
End Type


'Higher-order function type - just a procedure attached to a scope
Type Func Abstract
	Method apply:Object(args:Object[]) Abstract
End Type

'Function definitions - extend with fields as locals and implement apply as body
Type Scope Extends Func Abstract
	Field env:Scope

	'Constructor - bind an environment to a procedure
	Function lambda:Scope(env:Scope) Abstract

	Method _init:Scope(_env:Scope)	'Helper to keep constructors small
		env = _env ; Return Self
	End Method
End Type


'Based on the following definition:
'(define (Y f)
'    (let ((_r (lambda (r) (f (lambda a (apply (r r) a))))))
'      (_r _r)))

'Y (outer)
Type Y Extends Scope
	Field f:Func	'Parameter - gets closed over

	Function lambda:Scope(env:Scope)	'Necessary due to highly limited constructor syntax
		Return (New Y)._init(env)
	End Function

	Method apply:Func(args:Object[])
		f = Func(args[0])
		Local _r:Func = YInner1.lambda(Self)
		Return Func(_r.apply([_r]))
	End Method
End Type

'First lambda within Y
Type YInner1 Extends Scope
	Field r:Func	'Parameter - gets closed over

	Function lambda:Scope(env:Scope)
		Return (New YInner1)._init(env)
	End Function

	Method apply:Func(args:Object[])
		r = Func(args[0])
		Return Func(Y(env).f.apply([YInner2.lambda(Self)]))
	End Method
End Type

'Second lambda within Y
Type YInner2 Extends Scope
	Field a:Object[]	'Parameter - not really needed, but good for clarity

	Function lambda:Scope(env:Scope)
		Return (New YInner2)._init(env)
	End Function

	Method apply:Object(args:Object[])
		a = args
		Local r:Func = YInner1(env).r
		Return Func(r.apply([r])).apply(a)
	End Method
End Type


'Based on the following definition:
'(define fac (Y (lambda (f)
'                 (lambda (x)
'                   (if (<= x 0) 1 (* x (f (- x 1)))))))

Type FacL1 Extends Scope
	Field f:Func	'Parameter - gets closed over

	Function lambda:Scope(env:Scope)
		Return (New FacL1)._init(env)
	End Function

	Method apply:Object(args:Object[])
		f = Func(args[0])
		Return FacL2.lambda(Self)
	End Method
End Type

Type FacL2 Extends Scope
	Function lambda:Scope(env:Scope)
		Return (New FacL2)._init(env)
	End Function

	Method apply:Object(args:Object[])
		Local x:Int = Integer(args[0]).val
		If x <= 0 Then Return Integer.Make(1) ; Else Return Integer.Make(x * Integer(FacL1(env).f.apply([Integer.Make(x - 1)])).val)
	End Method
End Type


'Based on the following definition:
'(define fib (Y (lambda (f)
'                 (lambda (x)
'                   (if (< x 2) x (+ (f (- x 1)) (f (- x 2)))))))

Type FibL1 Extends Scope
	Field f:Func	'Parameter - gets closed over

	Function lambda:Scope(env:Scope)
		Return (New FibL1)._init(env)
	End Function

	Method apply:Object(args:Object[])
		f = Func(args[0])
		Return FibL2.lambda(Self)
	End Method
End Type

Type FibL2 Extends Scope
	Function lambda:Scope(env:Scope)
		Return (New FibL2)._init(env)
	End Function

	Method apply:Object(args:Object[])
		Local x:Int = Integer(args[0]).val
		If x < 2
			Return Integer.Make(x)
		Else
			Local f:Func = FibL1(env).f
			Local x1:Int = Integer(f.apply([Integer.Make(x - 1)])).val
			Local x2:Int = Integer(f.apply([Integer.Make(x - 2)])).val
			Return Integer.Make(x1 + x2)
		EndIf
	End Method
End Type


'Now test
Local _Y:Func = Y.lambda(Null)

Local fac:Func = Func(_Y.apply([FacL1.lambda(Null)]))
Print Integer(fac.apply([Integer.Make(10)])).val

Local fib:Func = Func(_Y.apply([FibL1.lambda(Null)]))
Print Integer(fib.apply([Integer.Make(10)])).val
