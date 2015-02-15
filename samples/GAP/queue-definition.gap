Enqueue := function(v, x)
   Add(v[1], x);
end;

Dequeue := function(v)
   local n, x;
   n := Size(v[2]);
   if n = 0 then
      v[2] := Reversed(v[1]);
      v[1] := [ ];
      n := Size(v[2]);
      if n = 0 then
         return fail;
      fi;
   fi;
   return Remove(v[2], n);
end;

# a new queue
v := [[], []];

Enqueue(v, 3);
Enqueue(v, 4);
Enqueue(v, 5);
Dequeue(v);
# 3
Enqueue(v, 6);
Dequeue(v);
# 4
Dequeue(v);
# 5
Dequeue(v);
# 6
Dequeue(v);
# fail
