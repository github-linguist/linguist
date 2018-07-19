import std.stdio, std.random;

enum nPlayers = 4, maxPoints = 100;

enum Moves { roll, hold }

abstract class Player {
  public:
    final void addCurrScore() pure nothrow {
        current_score += round_score;
    }
    final int getCurrScore() const pure nothrow {
        return current_score;
    }
    final int getRoundScore() const pure nothrow {
        return round_score;
    }
    final void addRoundScore(in int rs) pure nothrow {
        round_score += rs;
    }
    final void zeroRoundScore() pure nothrow {
        round_score = 0;
    }
    Moves getMove();

  protected int current_score, round_score;
}

final class PlayerRand: Player {
    override Moves getMove() {
        if (round_score + current_score >= maxPoints)
            return Moves.hold;

        if (uniform(0, 2) == 0)
            return Moves.roll;
        if (round_score > 0)
            return Moves.hold;
        return Moves.roll;
    }
}

final class PlayerQ2Win: Player {
    override Moves getMove() {
        if (round_score + current_score >= maxPoints)
            return Moves.hold;

        int q = maxPoints - current_score;
        if (q < 6)
            return Moves.roll;
        q /= 4;
        if (round_score < q)
            return Moves.roll;
        return Moves.hold;
    }
}

final class PlayerAL20: Player {
    override Moves getMove() {
        if (round_score + current_score >= maxPoints)
            return Moves.hold;
        if (round_score < 20)
            return Moves.roll;
        return Moves.hold;
    }
}

final class PlayerAL20T: Player {
    override Moves getMove() {
        if (round_score + current_score >= maxPoints)
            return Moves.hold;

        immutable d = (100 * round_score) / 20;
        if (round_score < 20 && d < uniform(0, 100))
            return Moves.roll;
        return Moves.hold;
    }
}

void main() {
    auto players = [new PlayerRand, new PlayerQ2Win,
                    new PlayerAL20, new PlayerAL20T];

    void nextTurn(ref uint p) nothrow {
        players[p].zeroRoundScore;
        p = (p + 1) % nPlayers;
    }

    uint p = 0;
    bool endGame = false;

    while (!endGame) {
        final switch (players[p].getMove) {
            case Moves.roll:
                immutable die = uniform(1, 7);

                if (die == 1) {
                    writeln("Player ", p + 1, " rolled ", die,
                            " - current score: ",
                            players[p].getCurrScore, "\n");
                    nextTurn(p);
                    continue;
                }
                players[p].addRoundScore(die);
                writeln("Player ", p + 1, " rolled ", die,
                        " - round score: ",
                        players[p].getRoundScore);
                break;

            case Moves.hold:
                players[p].addCurrScore;
                writeln("Player ", p + 1,
                        " holds - current score: ",
                        players[p].getCurrScore, "\n");
                if (players[p].getCurrScore >= maxPoints)
                    endGame = true;
                else
                    nextTurn(p);

        }
    }

    writeln;
    writeln("Player   I (Rand):  ", players[0].getCurrScore);
    writeln("Player  II (Q2Win): ", players[1].getCurrScore);
    writeln("Player III (AL20):  ", players[2].getCurrScore);
    writeln("Player  IV (AL20T): ", players[3].getCurrScore, "\n\n");
}
