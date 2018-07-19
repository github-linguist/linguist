#include <iostream>
#include <sstream>
#include <string>

const char *text =
{
    "In olden times when wishing still helped one, there lived a king "
    "whose daughters were all beautiful, but the youngest was so beautiful "
    "that the sun itself, which has seen so much, was astonished whenever "
    "it shone in her face.  Close by the king's castle lay a great dark "
    "forest, and under an old lime tree in the forest was a well, and when "
    "the day was very warm, the king's child went out into the forest and "
    "sat down by the side of the cool fountain, and when she was bored she "
    "took a golden ball, and threw it up on high and caught it, and this "
    "ball was her favorite plaything."
};

std::string wrap(const char *text, size_t line_length = 72)
{
    std::istringstream words(text);
    std::ostringstream wrapped;
    std::string word;

    if (words >> word) {
        wrapped << word;
        size_t space_left = line_length - word.length();
        while (words >> word) {
            if (space_left < word.length() + 1) {
                wrapped << '\n' << word;
                space_left = line_length - word.length();
            } else {
                wrapped << ' ' << word;
                space_left -= word.length() + 1;
            }
        }
    }
    return wrapped.str();
}

int main()
{
    std::cout << "Wrapped at 72:\n" << wrap(text) << "\n\n";
    std::cout << "Wrapped at 80:\n" << wrap(text, 80) << "\n";
}
