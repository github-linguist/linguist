#include <list>

template <typename T>
std::list<T> strandSort(std::list<T> lst) {
  if (lst.size() <= 1)
    return lst;
  std::list<T> result;
  std::list<T> sorted;
  while (!lst.empty()) {
    sorted.push_back(lst.front());
    lst.pop_front();
    for (typename std::list<T>::iterator it = lst.begin(); it != lst.end(); ) {
      if (sorted.back() <= *it) {
        sorted.push_back(*it);
        it = lst.erase(it);
      } else
        it++;
    }
    result.merge(sorted);
  }
  return result;
}
