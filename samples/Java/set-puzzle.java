import java.util.*;
import org.apache.commons.lang3.ArrayUtils;

public class SetPuzzle {

    enum Color {
        GREEN(0), PURPLE(1), RED(2);

        private Color(int v) {
            val = v;
        }
        public final int val;
    }

    enum Number {
        ONE(0), TWO(1), THREE(2);

        private Number(int v) {
            val = v;
        }
        public final int val;
    }

    enum Symbol {
        OVAL(0), DIAMOND(1), SQUIGGLE(2);

        private Symbol(int v) {
            val = v;
        }
        public final int val;
    }

    enum Fill {
        OPEN(0), STRIPED(1), SOLID(2);

        private Fill(int v) {
            val = v;
        }
        public final int val;
    }

    private static class Card implements Comparable<Card> {
        Color c;
        Number n;
        Symbol s;
        Fill f;

        public String toString() {
            return String.format("[Card: %s, %s, %s, %s]", c, n, s, f);
        }

        public int compareTo(Card o) {
            return (c.val - o.c.val) * 10 + (n.val - o.n.val);
        }
    }

    private static Card[] deck;

    public static void main(String[] args) {
        deck = new Card[81];
        Color[] colors = Color.values();
        Number[] numbers = Number.values();
        Symbol[] symbols = Symbol.values();
        Fill[] fillmodes = Fill.values();
        for (int i = 0; i < deck.length; i++) {
            deck[i] = new Card();
            deck[i].c = colors[i / 27];
            deck[i].n = numbers[(i / 9) % 3];
            deck[i].s = symbols[(i / 3) % 3];
            deck[i].f = fillmodes[i % 3];
        }
        findSets(12);
    }

    private static void findSets(int numCards) {
        int target = numCards / 2;
        Card[] cards;
        Card[][] sets = new Card[target][3];
        int cnt;
        do {
            Collections.shuffle(Arrays.asList(deck));
            cards = ArrayUtils.subarray(deck, 0, numCards);
            cnt = 0;

            outer:
            for (int i = 0; i < cards.length - 2; i++) {
                for (int j = i + 1; j < cards.length - 1; j++) {
                    for (int k = j + 1; k < cards.length; k++) {
                        if (validSet(cards[i], cards[j], cards[k])) {
                            if (cnt < target)
                                sets[cnt] = new Card[]{cards[i], cards[j], cards[k]};
                            if (++cnt > target) {
                                break outer;
                            }
                        }
                    }
                }
            }
        } while (cnt != target);

        Arrays.sort(cards);

        System.out.printf("GIVEN %d CARDS:\n\n", numCards);
        for (Card c : cards) {
            System.out.println(c);
        }
        System.out.println();

        System.out.println("FOUND " + target + " SETS:\n");
        for (Card[] set : sets) {
            for (Card c : set) {
                System.out.println(c);
            }
            System.out.println();
        }
    }

    private static boolean validSet(Card c1, Card c2, Card c3) {
        int tot = 0;
        tot += (c1.c.val + c2.c.val + c3.c.val) % 3;
        tot += (c1.n.val + c2.n.val + c3.n.val) % 3;
        tot += (c1.s.val + c2.s.val + c3.s.val) % 3;
        tot += (c1.f.val + c2.f.val + c3.f.val) % 3;
        return tot == 0;
    }
}
