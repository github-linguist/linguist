#ifndef CARDS_H_INC
#define CARDS_H_INC

#include <deque>
#include <algorithm>
#include <ostream>
#include <iterator>

namespace cards
{
  class card
  {
  public:
    enum pip_type { two, three, four, five, six, seven, eight, nine, ten,
                    jack, queen, king, ace };
    enum suite_type { hearts, spades, diamonds, clubs };

    // construct a card of a given suite and pip
    card(suite_type s, pip_type p): value(s + 4*p) {}

    // construct a card directly from its value
    card(unsigned char v = 0): value(v) {}

    // return the pip of the card
    pip_type pip() { return pip_type(value/4); }

    // return the suit of the card
    suite_type suite() { return suite_type(value%4); }

  private:
    // there are only 52 cards, therefore unsigned char suffices
    unsigned char value;
  };

  char const* const pip_names[] =
    { "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
      "jack", "queen", "king", "ace" };

  // output a pip
  std::ostream& operator<<(std::ostream& os, card::pip_type pip)
  {
    return os << pip_names[pip];
  };

  char const* const suite_names[] =
    { "hearts", "spades", "diamonds", "clubs" };

  // output a suite
  std::ostream& operator<<(std::ostream& os, card::suite_type suite)
  {
    return os << suite_names[suite];
  }

  // output a card
  std::ostream& operator<<(std::ostream& os, card c)
  {
    return os << c.pip() << " of " << c.suite();
  }

  class deck
  {
  public:
    // default constructor: construct a default-ordered deck
    deck()
    {
      for (int i = 0; i < 52; ++i)
        cards.push_back(card(i));
    }

    // shuffle the deck
    void shuffle() { std::random_shuffle(cards.begin(), cards.end()); }

    // deal a card from the top
    card deal() { card c = cards.front(); cards.pop_front(); return c; }

    // iterators (only reading access is allowed)
    typedef std::deque<card>::const_iterator const_iterator;
    const_iterator begin() const { return cards.begin(); }
    const_iterator end() const { return cards.end(); }
  private:
    // the cards
    std::deque<card> cards;
  };

  // output the deck
  inline std::ostream& operator<<(std::ostream& os, deck const& d)
  {
    std::copy(d.begin(), d.end(), std::ostream_iterator<card>(os, "\n"));
    return os;
  }
}

#endif
