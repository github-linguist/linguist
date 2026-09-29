// Copyright (c) 2010-2026 Zhang Weidong <zwd@funlang.org>
// SPDX-License-Identifier: MIT

########################################
# lib-base
########################################

fun ifval (condition, vtrue, vfalse)
  if condition then
    return vtrue;
  else
    return vfalse;
  end if;
end fun;
var ifv    = ifval;
var ifb(c) = ifv(c, true, false);
var iff(c, t, f) = ifv(c(), t, f);
var iffa(c, t, f) = ifa(c(), t, f);

fun ifa (condition, atrue, afalse)
  if condition then
    return atrue ();
  else
    return afalse();
  end if;
end fun;

fun forloop (start, end1, step1, action, set)
  result = set;
  for i = start to end1 step step1 do
    result = action(i, result) or result;
  end do;
end fun;
var forto  = forloop;
var forE(num, action, set) = forto(1, num, 1, action, set);

var filter = (start, end1, step1, cond, action){
  result = [];
  forto(start, end1, step1, (i, ret){
    if cond(i, ret) then
      action(i, ret);
    end if;
    result = ret;
  }, result);
};
var filter1(s, e, c, a) = filter(s, e, 1, c, a);

fun foreach (set1, action)
  for k: v in set1 do
    action(v, k);
  end do;
end fun;
var forin  = foreach;
