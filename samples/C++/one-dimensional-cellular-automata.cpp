#include <iostream>
#include <bitset>
#include <string>

const int ArraySize = 20;
const int NumGenerations = 10;
const std::string Initial = "0011101101010101001000";

int main()
{
    // + 2 for the fixed ends of the array
    std::bitset<ArraySize + 2> array(Initial);

    for(int j = 0; j < NumGenerations; ++j)
    {
        std::bitset<ArraySize + 2> tmpArray(array);
        for(int i = ArraySize; i >= 1 ; --i)
        {
            if(array[i])
                std::cout << "#";
            else
                std::cout << "_";
            int val = (int)array[i-1] << 2 | (int)array[i] << 1 | (int)array[i+1];
            tmpArray[i] = (val == 3 || val == 5 || val == 6);
        }
        array = tmpArray;
        std::cout << std::endl;
    }
}
