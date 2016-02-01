local C = terralib.includecstring [[

union testunion {
  int idata;
  float fdata;
};

typedef 
	union {
		int a;
		float b;
	} S;

void setUnion(union testunion * u, S * s){
  u->idata = 3;
}
int getUnionInt(union testunion * u){
  return u->idata;
}

]]

terra foo() : int
  var u : C.testunion
  C.setUnion(&u,nil)
  var s : C.S
  return C.getUnionInt(&u)
end


terralib.tree.printraw(C.S)

local test = require("test")
test.eq(foo(),3)