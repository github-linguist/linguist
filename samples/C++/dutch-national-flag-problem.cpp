#include <algorithm>
#include <iostream>

// Dutch national flag problem
template <typename BidIt, typename T>
void dnf_partition(BidIt first, BidIt last, const T& low, const T& high)
{
    for (BidIt next = first; next != last; ) {
        if (*next < low) {
            std::iter_swap(first++, next++);
        } else if (!(*next < high)) {
            std::iter_swap(next, --last);
        } else {
            ++next;
        }
    }
}

enum Colors { RED, WHITE, BLUE };

void print(const Colors *balls, size_t size)
{
    static const char *label[] = { "red", "white", "blue" };

    std::cout << "Balls:";
    for (size_t i = 0; i < size; ++i) {
        std::cout << ' ' << label[balls[i]];
    }
    std::cout << "\nSorted: " << std::boolalpha << std::is_sorted(balls, balls + size) << '\n';
}

int main()
{
    Colors balls[] = { RED, WHITE, BLUE, RED, WHITE, BLUE, RED, WHITE, BLUE };

    std::random_shuffle(balls, balls + 9);
    print(balls, 9);

    dnf_partition(balls, balls + 9, WHITE, BLUE);
    print(balls, 9);
}
