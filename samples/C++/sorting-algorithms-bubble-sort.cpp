#include <iostream>
#include <algorithm>

template< typename ARRAY_TYPE, typename INDEX_TYPE >
void bubble_sort(ARRAY_TYPE array[], INDEX_TYPE size)
{
    bool done = false;

    while ( ! done)
    {
        done = true;
        for (INDEX_TYPE i = 0 ; i < size-1 ; i++)
        {
            if (array[i] > array[i+1])
            {
                done = false;
                std::swap(array[i], array[i+1]);
            }
        }
        size--;
    }
}

template< typename TYPE >
void print(TYPE val)
{
    std::cout << val << " ";
}

int main()
{
    int array[] = { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };
    bubble_sort(array, 10);
    std::for_each (&array[0], &array[10], print<int>);
    std::cout << std::endl;

    //But in real life...
    int data[] = { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };
    std::sort(data, data+10);
    std::for_each (data, data+10, print<int>);
    std::cout << std::endl;
}
