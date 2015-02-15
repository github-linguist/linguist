define method primes(n)
  let limit = floor(n ^ 0.5) + 1;
  let sieve = make(limited(<simple-vector>, of: <boolean>), size: n + 1, fill: #t);
  let last-prime = 2;

  while (last-prime < limit)
    for (x from last-prime ^ 2 to n by last-prime)
      sieve[x] := #f;
    end for;
    block (found-prime)
      for (n from last-prime + 1 below limit)
        if (sieve[n] = #f)
          last-prime := n;
          found-prime()
        end;
      end;
      last-prime := limit;
    end block;
  end while;

  for (x from 2 to n)
    if (sieve[x]) format-out("Prime: %d\n", x); end;
  end;
end;
