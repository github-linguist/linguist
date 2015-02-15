pips <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")
suit <- c("Clubs", "Diamonds", "Hearts", "Spades")
# Create a deck
deck <- data.frame(pips=rep(pips, 4), suit=rep(suit, each=13))

shuffle <- function(deck)
{
   n <- nrow(deck)
   ord <- sample(seq_len(n), size=n)
   deck[ord,]
}

deal <- function(deck, fromtop=TRUE)
{
   index <- ifelse(fromtop, 1, nrow(deck))
   print(paste("Dealt the", deck[index, "pips"], "of", deck[index, "suit"]))
   deck[-index,]
}

# Usage
deck <- shuffle(deck)
deck
deck <- deal(deck)
# While no-one is looking, sneakily deal a card from the bottom of the pack
deck <- deal(deck, FALSE)
