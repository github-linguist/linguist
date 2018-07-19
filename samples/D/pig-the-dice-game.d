void main() {
    import std.stdio, std.string, std.algorithm, std.random;
    enum maxScore = 100;
    enum playerCount = 2;
    immutable confirmations = ["yes", "y", ""];

    int[playerCount] safeScore;
    int player, score;

    while (true) {
        writef(" Player %d: (%d, %d). Rolling? (y/n) ", player,
               safeScore[player], score);
        if (safeScore[player] + score < maxScore &&
            confirmations.canFind(readln.strip.toLower)) {
            immutable rolled = uniform(1, 7);
            writefln(" Rolled %d", rolled);
            if (rolled == 1) {
                writefln(" Bust! You lose %d but keep %d\n",
                         score, safeScore[player]);
            } else {
                score += rolled;
                continue;
            }
        } else {
            safeScore[player] += score;
            if (safeScore[player] >= maxScore)
                break;
            writefln(" Sticking with %d\n", safeScore[player]);
        }

        score = 0;
        player = (player + 1) % playerCount;
    }

    writefln("\n\nPlayer %d wins with a score of %d",
             player, safeScore[player]);
}
