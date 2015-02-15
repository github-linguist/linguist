#include <iostream>
#include <iomanip>
#include <cmath> // for log10()
#include <algorithm> // for max()

size_t get_table_column_width(const int min, const int max)
{
    unsigned int abs_max = std::max(max*max, min*min);

    // abs_max is the largest absolute value we might see.
    // If we take the log10 and add one, we get the string width
    // of the largest possible absolute value.
    // Add one for a little whitespace guarantee.
    size_t colwidth = 1 + std::log10(abs_max) + 1;

    // If only one of them is less than 0, then some will
    // be negative.
    bool has_negative_result = (min < 0) && (max > 0);

    // If some values may be negative, then we need to add some space
    // for a sign indicator (-)
    if(has_negative_result)
        colwidth++;

    return colwidth;
}

void print_table_header(const int min, const int max)
{
    size_t colwidth = get_table_column_width(min, max);

    // table corner
    std::cout << std::setw(colwidth) << " ";

    for(int col = min; col <= max; ++col)
    {
        std::cout << std::setw(colwidth) << col;
    }

    // End header with a newline and blank line.
    std::cout << std::endl << std::endl;
}

void print_table_row(const int num, const int min, const int max)
{
    size_t colwidth = get_table_column_width(min, max);

    // Header column
    std::cout << std::setw(colwidth) << num;

    // Spacing to ensure only the top half is printed
    for(int multiplicand = min; multiplicand < num; ++multiplicand)
    {
        std::cout << std::setw(colwidth) << " ";
    }

    // Remaining multiplicands for the row.
    for(int multiplicand = num; multiplicand <= max; ++multiplicand)
    {
        std::cout << std::setw(colwidth) << num * multiplicand;
    }

    // End row with a newline and blank line.
    std::cout << std::endl << std::endl;
}

void print_table(const int min, const int max)
{
    // Header row
    print_table_header(min, max);

    // Table body
    for(int row = min; row <= max; ++row)
    {
        print_table_row(row, min, max);
    }
}

int main()
{
    print_table(1, 12);
    return 0;
}
