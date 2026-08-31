#*======================================
    FFFFFFFF   UU    UU   NN    NN
    FF         UU    UU   NNN   NN
    FFFFFFF    UU    UU   NN N  NN
    FF         UU    UU   NN  N NN
    FF         UU    UU   NN   NNN
    FF          UUUUUU    NN    NN
##======================================

var out = output();
class output()
  var head = (s){?. s & '.'.x(20-s.length())};
  var body = (s){?, ' '; ?. s};
  fun close()
    head('end');
    ? '\r\n'.escape();
  end fun;
end class;

var idx;
fun closure(o)
  var i = 0;
  return {
    o.body('index $i'.eval());
    result = i;
    i += 1;
  };
end fun;

fun test(n, f)
  out.head(n);
  idx = closure(out);
  f();
  out.close();
end fun;

test('if', {
  if    idx() > idx() and idx() <= idx() then # short-circuit
    out.body('wrong');
  elsif idx() < idx()  or idx() >= idx() then # short-circuit
    out.body('right');
  elsif idx() = idx() xor idx() <> idx() then
    out.body('error');
  else
    out.body('error');
  end if;
});

test('case', {
  case idx() is # @ <- idx()
    when idx() do
      out.body('wrong x()');
    when [idx(), idx()] do // or when new ...
      out.body('wrong [x]');
    when '^(%s)$'.format(idx()).toRegex() do
      out.body('wrong /x/');
    when '^($@)$'.eval().toRegex() do
      out.body('right /x/');
    when idx() do
      out.body('error');
    else
      out.body('error');
  end case;
});

test('loop', {
  loop
    exit when idx() < idx();
  end loop;

  for i = idx() to idx() step idx() loop
    out.body((i = 2) & ' - ' & i);
  end loop;

  for k: i in [a: idx(), b: idx(), c: idx()] do // or in new ...
    out.body('$k: $i'.eval());
  end do;

  while idx() < idx() do
    next when idx() mod 2 = 0;
    exit when idx() < idx();
  end do;

  out.body(idx() = 16);
});

test('try', {
  try
    var error() = 1 / 0;
    idx(); error(); idx();
  except # @ <- exception
    out.body('$@ at $@@()'.eval()); # eval $@@ as the last error
  finally
    out.body('final'.x(-1)); # reverse: a.x(-1)
  end try;
});
