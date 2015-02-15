#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

// pancake sort template (calls predicate to determine order)
template <typename BidIt, typename Pred>
void pancake_sort(BidIt first, BidIt last, Pred order)
{
    if (std::distance(first, last) < 2) return; // no sort needed

    for (; first != last; --last)
    {
        BidIt mid = std::max_element(first, last, order);
        if (mid == last - 1)
        {
            continue; // no flips needed
        }
        if (first != mid)
        {
            std::reverse(first, mid + 1); // flip element to front
        }
        std::reverse(first, last); // flip front to final position
    }
}

// pancake sort template (ascending order)
template <typename BidIt>
void pancake_sort(BidIt first, BidIt last)
{
    pancake_sort(first, last, std::less<typename std::iterator_traits<BidIt>::value_type>());
}

int main()
{
    std::vector<int> data;
    for (int i = 0; i < 20; ++i)
    {
        data.push_back(i); // generate test data
    }
    std::random_shuffle(data.begin(), data.end()); // scramble data

    std::copy(data.begin(), data.end(), std::ostream_iterator<int>(std::cout, " "));
    std::cout << "\n";

    pancake_sort(data.begin(), data.end()); // ascending pancake sort

    std::copy(data.begin(), data.end(), std::ostream_iterator<int>(std::cout, " "));
    std::cout << "\n";
}
