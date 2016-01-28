
struct V {
	entry : {&A} -> {}
} and
struct A {
	vtable : &V
}

terra what(a : &A) : {}
end

what:compile()

terra bar()
	var v : V
	v.entry = what
end

bar:compile()

--[[
{&A}->{}
&A
A
&V
V
&{&A}->{}
{&A}->{}

]]