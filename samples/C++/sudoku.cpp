#include <iostream>
using namespace std;

class SudokuSolver {
private:
    int grid[81];

public:

    SudokuSolver(string s) {
        for (unsigned int i = 0; i < s.length(); i++) {
            grid[i] = (int) (s[i] - '0');
        }
    }

    void solve() {
        try {
            placeNumber(0);
            cout << "Unsolvable!" << endl;
        } catch (char* ex) {
            cout << ex << endl;
            cout << this->toString() << endl;
        }
    }

    void placeNumber(int pos) {
        if (pos == 81) {
            throw (char*) "Finished!";
        }
        if (grid[pos] > 0) {
            placeNumber(pos + 1);
            return;
        }
        for (int n = 1; n <= 9; n++) {
            if (checkValidity(n, pos % 9, pos / 9)) {
                grid[pos] = n;
                placeNumber(pos + 1);
                grid[pos] = 0;
            }
        }
    }

    bool checkValidity(int val, int x, int y) {
        for (int i = 0; i < 9; i++) {
            if (grid[y * 9 + i] == val || grid[i * 9 + x] == val)
                return false;
        }
        int startX = (x / 3) * 3;
        int startY = (y / 3) * 3;
        for (int i = startY; i < startY + 3; i++) {
            for (int j = startX; j < startX + 3; j++) {
                if (grid[i * 9 + j] == val)
                    return false;
            }
        }
        return true;
    }

    string toString() {
        string sb;
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                char c[2];
                c[0] = grid[i * 9 + j] + '0';
                c[1] = '\0';
                sb.append(c);
                sb.append(" ");
                if (j == 2 || j == 5)
                    sb.append("| ");
            }
            sb.append("\n");
            if (i == 2 || i == 5)
                sb.append("------+-------+------\n");
        }
        return sb;
    }

};

int main() {
    SudokuSolver ss(
            (string) "850002400" +
            (string) "720000009" +
            (string) "004000000" +
            (string) "000107002" +
            (string) "305000900" +
            (string) "040000000" +
            (string) "000080070" +
            (string) "017000000" +
            (string) "000036040"
            );
    ss.solve();
}
