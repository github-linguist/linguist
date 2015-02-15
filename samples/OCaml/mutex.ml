let m = Mutex.create() in
Mutex.lock m;  (* locks in blocking mode *)

if (Mutex.try_lock m)
then ...  (* did the lock *)
else ...  (* already locked, do not block *)

Mutex.unlock m;
