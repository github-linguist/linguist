local C = terralib.includecstring [[

struct teststruct {
  int idata;
  float fdata;
};

void makeitlive(struct teststruct * s) {}

]]


terra foo()
	var a : C.teststruct
	a.idata = 3
	a.fdata = 3.5
	return a.idata + a.fdata
end

assert(foo() == 6.5)