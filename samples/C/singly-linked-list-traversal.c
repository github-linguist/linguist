struct link *first;
// ...
struct link *iter;
for(iter = first; iter != NULL; iter = iter->next) {
  // access data, e.g. with iter->data
}
