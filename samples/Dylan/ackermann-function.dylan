define method ack(m == 0, n :: <integer>)
   n + 1
end;
define method ack(m :: <integer>, n :: <integer>)
   ack(m - 1, if (n == 0) 1 else ack(m, n - 1) end)
end;
