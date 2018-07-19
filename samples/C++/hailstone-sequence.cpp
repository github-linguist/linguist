#include <iostream>
#include <vector>
#include <utility>

std::vector<int> hailstone(int i)
{
    std::vector<int> v;
    while(true){
        v.push_back(i);
        if (1 == i) break;
        i = (i % 2) ? (3 * i + 1) : (i / 2);
    }
    return v;
}

std::pair<int,int> find_longest_hailstone_seq(int n)
{
    std::pair<int, int> maxseq(0, 0);
    int l;
    for(int i = 1; i < n; ++i){
        l = hailstone(i).size();
        if (l > maxseq.second) maxseq = std::make_pair(i, l);
    }
    return maxseq;
}

int main () {

// Use the routine to show that the hailstone sequence for the number 27
    std::vector<int> h27;
    h27 = hailstone(27);
// has 112 elements
    int l = h27.size();
    std::cout << "length of hailstone(27) is " << l;
// starting with 27, 82, 41, 124 and
    std::cout << " first four elements of hailstone(27) are ";
    std::cout << h27[0] << " " << h27[1] << " "
              << h27[2] << " " << h27[3] << std::endl;
// ending with 8, 4, 2, 1
    std::cout << " last four elements of hailstone(27) are "
              << h27[l-4] << " " << h27[l-3] << " "
              << h27[l-2] << " " << h27[l-1] << std::endl;

    std::pair<int,int> m = find_longest_hailstone_seq(100000);

    std::cout << "the longest hailstone sequence under 100,000 is " << m.first
              << " with " << m.second << " elements." <<std::endl;

    return 0;
}
