to |Y|(f)
  script x
    to funcall(y)
      script
        to funcall(arg)
          y's funcall(y)'s funcall(arg)
        end funcall
      end script
      f's funcall(result)
    end funcall
  end script
  x's funcall(x)
end |Y|

script
  to funcall(f)
    script
      to funcall(n)
        if n = 0 then return 1
        n * (f's funcall(n - 1))
      end funcall
    end script
  end funcall
end script
set fact to |Y|(result)

script
  to funcall(f)
    script
      to funcall(n)
        if n = 0 then return 0
        if n = 1 then return 1
        (f's funcall(n - 2)) + (f's funcall(n - 1))
      end funcall
    end script
  end funcall
end script
set fib to |Y|(result)

set facts to {}
repeat with i from 0 to 11
  set end of facts to fact's funcall(i)
end repeat

set fibs to {}
repeat with i from 0 to 20
  set end of fibs to fib's funcall(i)
end repeat

{facts:facts, fibs:fibs}
(*
{facts:{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800},
 fibs:{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765}}
*)
