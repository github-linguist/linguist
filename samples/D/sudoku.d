import std.stdio, std.range, std.string, std.algorithm, std.array,
       std.typetuple, std.ascii, std.typecons;

template Range(size_t stop) { // For loop unrolling.
    static if (stop == 0)
        alias TypeTuple!() Range;
    else
        alias TypeTuple!(Range!(stop - 1), stop - 1) Range;
}

struct Digit {
    immutable char d;

    this(in char d_) pure nothrow
    in { assert(d_ >= '0' && d_ <= '9'); }
    body { this.d = d_; }

    this(in int d_) pure nothrow
    in { assert(d_ >= '0' && d_ <= '9'); }
    body { this.d = cast(char)d_; } // Required cast.

    alias d this;
}

enum size_t sudokuUnitSide = 3;
enum size_t sudokuSide = sudokuUnitSide ^^ 2; // Sudoku grid side.
alias SudokuTable = Digit[sudokuSide ^^ 2];


Nullable!SudokuTable sudokuSolver(in ref SudokuTable problem)
pure nothrow {
    alias Tgrid = uint;
    Tgrid[SudokuTable.length] grid = void;
    problem[].map!(c => c - '0').copy(grid[]);

    // DMD doesn't inline this function. Performance loss.
    Tgrid access(in size_t x, in size_t y) nothrow {
        return grid[y * sudokuSide + x];
    }

    // DMD doesn't inline this function. If you want to retain
    // the same performance as the C++ entry and you use the DMD
    // compiler then this function must be manually inlined.
    bool checkValidity(in Tgrid val, in size_t x, in size_t y) nothrow {
        /*static*/ foreach (immutable i; Range!sudokuSide)
            if (access(i, y) == val || access(x, i) == val)
                return false;

        immutable startX = (x / sudokuUnitSide) * sudokuUnitSide;
        immutable startY = (y / sudokuUnitSide) * sudokuUnitSide;

        /*static*/ foreach (immutable i; Range!sudokuUnitSide)
            /*static*/ foreach (immutable j; Range!sudokuUnitSide)
                if (access(startX + j, startY + i) == val)
                    return false;

        return true;
    }

    bool canPlaceNumbers(in size_t pos=0) nothrow {
        if (pos == SudokuTable.length)
            return true;
        if (grid[pos] > 0)
            return canPlaceNumbers(pos + 1);

        foreach (immutable n; 1 .. sudokuSide + 1)
            if (checkValidity(n, pos % sudokuSide, pos / sudokuSide)) {
                grid[pos] = n;
                if (canPlaceNumbers(pos + 1))
                    return true;
                grid[pos] = 0;
            }

        return false;
    }

    if (canPlaceNumbers) {
        //return typeof(return)(grid[]
        //                      .map!(c => Digit(c + '0'))
        //                      .array);
        immutable SudokuTable result = grid[]
                                       .map!(c => Digit(c + '0'))
                                       .array;
        return typeof(return)(result);
    } else
        return typeof(return)();
}

string representSudoku(in ref SudokuTable sudo)
pure nothrow out(result) {
    assert(result.countchars("1-9") == sudo[].count!q{a != '0'});
    assert(result.countchars(".") == sudo[].count!q{a == '0'});
} body {
    static assert(sudo.length == 81,
        "representSudoku works only with a 9x9 Sudoku.");
    string result;

    foreach (immutable i; 0 .. sudokuSide) {
        foreach (immutable j; 0 .. sudokuSide) {
            result ~= sudo[i * sudokuSide + j];
            result ~= ' ';
            if (j == 2 || j == 5)
                result ~= "| ";
        }
        result ~= "\n";
        if (i == 2 || i == 5)
            result ~= "------+-------+------\n";
    }

    return result.replace("0", ".");
}

void main() {
    enum ValidateCells(string s) = s.map!Digit.array;

    immutable SudokuTable problem = ValidateCells!("
        850002400
        720000009
        004000000
        000107002
        305000900
        040000000
        000080070
        017000000
        000036040".removechars(whitespace));
    problem.representSudoku.writeln;

    immutable solution = problem.sudokuSolver;
    if (solution.isNull)
        writeln("Unsolvable!");
    else
        solution.get.representSudoku.writeln;
}
