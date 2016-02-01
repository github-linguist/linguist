local C = terralib.includecstring [[

union testunion {
  int idata;
  float fdata;
};

void setUnionI(union testunion * u){
  u->idata = 3;
}
void setUnionF(union testunion * u){
  u->fdata = 4.f;
}

]]

terra foo() : int
  var u : C.testunion
  C.setUnionI(&u)
  var f = u.idata
  C.setUnionF(&u)
  var f2 = u.fdata
  return f + f2
end

terra foo2()
  var a : C.testunion
  a.fdata = -3.0
  a.idata = a.idata and not (1 << 31)
  return a.fdata
end

local test = require("test")
test.eq(foo(),7)
test.eq(foo2(),3)