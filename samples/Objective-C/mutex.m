NSLock *m = [[NSLock alloc] init];

[m lock]; // locks in blocking mode

if ([m tryLock]) { // acquire a lock -- does not block if not acquired
  // lock acquired
} else {
  // already locked, does not block
}

[m unlock];
