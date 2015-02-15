#include <vector>
#include <algorithm>
#include <string>

// helper comparator that is passed to std::sort()
template <class T>
struct sort_table_functor {
  typedef bool (*CompFun)(const T &, const T &);
  const CompFun ordering;
  const int column;
  const bool reverse;
  sort_table_functor(CompFun o, int c, bool r) :
    ordering(o), column(c), reverse(r) { }
  bool operator()(const std::vector<T> &x, const std::vector<T> &y) const {
    const T &a = x[column],
            &b = y[column];
    return reverse ? ordering(b, a)
                   : ordering(a, b);
  }
};

// natural-order less-than comparator
template <class T>
bool myLess(const T &x, const T &y) { return x < y; }

// this is the function we call, which takes optional parameters
template <class T>
void sort_table(std::vector<std::vector<T> > &table,
                int column = 0, bool reverse = false,
                bool (*ordering)(const T &, const T &) = myLess) {
  std::sort(table.begin(), table.end(),
            sort_table_functor<T>(ordering, column, reverse));
}

#include <iostream>

// helper function to print our 3x3 matrix
template <class T>
void print_matrix(std::vector<std::vector<T> > &data) {
  for () {
    for (int j = 0; j < 3; j++)
      std::cout << data[i][j] << "\t";
    std::cout << std::endl;
  }
}

// order in descending length
bool desc_len_comparator(const std::string &x, const std::string &y) {
  return x.length() > y.length();
}

int main() {

  std::string data_array[3][3] =
    {
      {"a", "b", "c"},
      {"", "q", "z"},
      {"zap", "zip", "Zot"}
    };

  std::vector<std::vector<std::string> > data_orig;
  for (int i = 0; i < 3; i++) {
    std::vector<std::string> row;
    for (int j = 0; j < 3; j++)
      row.push_back(data_array[i][j]);
    data_orig.push_back(row);
  }
  print_matrix(data_orig);

  std::vector<std::vector<std::string> > data = data_orig;
  sort_table(data);
  print_matrix(data);

  data = data_orig;
  sort_table(data, 2);
  print_matrix(data);

  data = data_orig;
  sort_table(data, 1);
  print_matrix(data);

  data = data_orig;
  sort_table(data, 1, true);
  print_matrix(data);

  data = data_orig;
  sort_table(data, 0, false, desc_len_comparator);
  print_matrix(data);

  return 0;
}
