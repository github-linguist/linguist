define function fib (n)
  when (n < 0)
    error("Can't take fibonacci of negative integer: %d\n", n)
  end;
  local method fib1 (n, a, b)
    if (n = 0)
      a
    else
      fib1(n - 1, b, a + b)
    end
  end;
  fib1(n, 0, 1)
end
