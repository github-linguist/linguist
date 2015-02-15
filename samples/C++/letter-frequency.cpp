#include <fstream>
#include <iostream>

int main()
{
	std::ifstream input("filename.txt", std::ios_base::binary);
	if (!input)
	{
		std::cerr << "error: can't open file\n";
		return -1;
	}

	size_t count[256];
	std::fill_n(count, 256, 0);

	for (char c; input.get(c); ++count[uint8_t(c)]) // process input file
		; // empty loop body

	for (size_t i = 0; i < 256; ++i)
	{
		if (count[i] && isgraph(i)) // non-zero counts of printable characters
		{
			std::cout << char(i) << " = " << count[i] << '\n';
		}
	}
}
