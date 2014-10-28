// Copyright (c) 2011, David J. Pearce (djp@ecs.vuw.ac.nz)
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//    * Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimer in the
//      documentation and/or other materials provided with the distribution.
//    * Neither the name of the <organization> nor the
//      names of its contributors may be used to endorse or promote products
//      derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL DAVID J. PEARCE BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

type nat is (int x) where x >= 0

// ==================================================================
// A square on the board is either blank, or holds either a circle or
// cross.
// ==================================================================
constant BLANK is 0
constant CIRCLE is 1
constant CROSS is 2

type Square is (int x) where x == BLANK || x == CIRCLE || x == CROSS

// ==================================================================
// A board consists of 9 squares, and a move counter
// ==================================================================
type Board is {
    nat move,
    [Square] pieces // 3 x 3
} where |pieces| == 9 && move <= 9 && 
    countOf(pieces,BLANK) == (9 - move) &&
    (countOf(pieces,CIRCLE) == countOf(pieces,CROSS) ||    
     countOf(pieces,CIRCLE) == countOf(pieces,CROSS)+1)

// ==================================================================
// An empty board is one where all pieces are blank
// ==================================================================
function EmptyBoard() => (Board r)
// Empty board has no moves yet
ensures r.move == 0:
    //
    return {
        move: 0,
        pieces: [BLANK,BLANK,BLANK,
                 BLANK,BLANK,BLANK,
                 BLANK,BLANK,BLANK]
    }

// ===============================================================
// Playing a piece requires an blank square, and returns the board
// updated with the piece at that position and an incremented the move
// counter.
// ===============================================================
function play(Board b, nat pos) => (Board r)
// Board position to place onto must be valid
requires pos < 9 && b.move < 9 && b.pieces[pos] == BLANK
// Ensures move count is incremented
ensures r.move == r.move + 1:
    // decide who's moving
    if b.move % 2 == 0:
        // circle on even moves
        b.pieces[pos] = CIRCLE
    else:
        // cross on odd moves
        b.pieces[pos] = CROSS
    // update the move counter
    b.move = b.move + 1
    // done
    return b

// ===============================================================
// Helper Method
// ===============================================================
function countOf([Square] pieces, Square s) => (int r)
ensures r == |{ i | i in 0..|pieces|, pieces[i] == s }|:
    //
    {int} matches = { i | i in 0..|pieces|, pieces[i] == s }
    return |matches|

// ===============================================================
// Test Game
// ===============================================================
constant GAME is [0,1,2,3,4,5,6,7,8]

method main(System.Console console):
    Board b = EmptyBoard()
    for p in GAME:
        console.out.println("BOARD: " ++ b)
        console.out.println("MOVE: " ++ p)
        if p < 0 || p > 9 || b.pieces[p] != BLANK || b.move == 9:
            console.out.println("INVALID MOVE!")
            break
        else:
            b = play(b,p)





    
    
    
