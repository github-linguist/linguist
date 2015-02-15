#include <iostream>
#include <algorithm>
#include <vector>
#include <iterator>

void genPyrN(int rows) {
  if (rows < 0) return;
  // save the last row here
  std::vector<int> last(1, 1);
  std::cout << last[0] << std::endl;

  for (int i = 1; i <= rows; i++) {
    // work on the next row
    std::vector<int> thisRow;
    thisRow.reserve(i+1);
    thisRow.push_back(last.front()); // beginning of row
    std::transform(last.begin(), last.end()-1, last.begin()+1, std::back_inserter(thisRow), std::plus<int>()); // middle of row
    thisRow.push_back(last.back()); // end of row

    for (int j = 0; j <= i; j++)
      std::cout << thisRow[j] << " ";
    std::cout << std::endl;

    last.swap(thisRow);
  }
}
