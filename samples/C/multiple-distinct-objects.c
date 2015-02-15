foo *foos = malloc(n * sizeof(*foos));
for (int i = 0; i < n; i++)
  init_foo(&foos[i]);
