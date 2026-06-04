/*
 * Copyright 2024 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestDisplay {
    use Flixball.Core.{Strategy, PlayerId};
    use Flixball.Core.Board.Board.Board;
    use Flixball.Core.Board.Board;
    use Flixball.Core.Direction.{East, North, South, West};
    use Flixball.Core.Rotation.{Clockwise, Counterclockwise};
    use Flixball.Core.Tile.{Person, Wall};
    use Flixball.BoardLibrary.{automataBoard, randomBoard};
    use Flixball.Core.GameState.mkGameState;
    use Flixball.AiLibrary.{
        rotate, shoot, walk, spinAndShoot, goAndSpin, naiveSearchAndDestroy,
        wander, smartSearchAndDestroy
    };
    use Flixball.Display.{runGameState, displayBoard}

    def testBoard01(): (Board, Map[PlayerId, Strategy]) =
        let board = match Flixball.BoardLibrary.fromString(String.unlines(
            "     " ::
            "     " ::
            "  v  " ::
            "   < " ::
            Nil
        )) {
            case Ok(b) => b
            case Err(msg) => bug!(msg)
        };
        let moveLogics = Map#{
            0 => walk(),
            1 => shoot()
        };
        (board, moveLogics)
    
    def testBoard02(): (Board, Map[PlayerId, Strategy]) =
        let board = match Flixball.BoardLibrary.fromString(String.unlines(
            "                  " ::
            " >  ^^^^^^^#  <   " ::
            "                  " ::
            Nil
        )) {
            case Ok(b) => b
            case Err(msg) => bug!(msg)
        };
        let moveLogics = Map#{
            0 => shoot(),
            1 => rotate(Clockwise),
            2 => rotate(Counterclockwise),
            3 => rotate(Clockwise),
            4 => rotate(Counterclockwise),
            5 => rotate(Clockwise),
            6 => rotate(Counterclockwise),
            7 => rotate(Clockwise),
            8 => spinAndShoot(Clockwise)
        };
        (board, moveLogics)

    def testBoard03(): (Board, Map[PlayerId, Strategy]) =
        let board = match Flixball.BoardLibrary.fromString(String.unlines(
            "^ #       " ::
            "  #       " ::
            "  #       " ::
            "  #  #    " ::
            "     #    " ::
            "########< " ::
            "          " ::
            "        ##" ::
            "          " ::
            "          " ::
            Nil
        )) {
            case Ok(b) => b
            case Err(msg) => bug!(msg)
        };
        let moveLogics = Map#{
            0 => goAndSpin((10, 10)),
            1 => walk()};
        (board, moveLogics)

    def testBoard04(): Board =
        randomBoard(719_657i64, rows=15, 10, 0.3)
    
    def testBoard05(): Board =
        automataBoard(285_041_125i64, rows=25, 40, 0.38, 3)

    def testBoard06(): (Board, Map[PlayerId, Strategy]) =
        let board = match Flixball.BoardLibrary.fromString(String.unlines(
            "^         " ::
            " v        " ::
            "  <       " ::
            "   >      " ::
            "    <     " ::
            "     v    " ::
            "      ^   " ::
            "       <  " ::
            "        v " ::
            "         >" ::
            Nil
        )) {
            case Ok(b) => b
            case Err(msg) => bug!(msg)
        };
        let moveLogics = Map#{
            0 => naiveSearchAndDestroy(), 
            1 => wander(), 
            2 => wander(),
            3 => wander(),
            4 => wander(),
            5 => wander(),
            6 => wander(),
            7 => wander(),
            8 => wander(),
            9 => wander()
        };
        (board, moveLogics)

    def testBoard07(): (Board, Map[PlayerId, Strategy]) =
        let board = match Flixball.BoardLibrary.fromString(String.unlines(
            "^   #    <" ::
            "    #  ###" ::
            "#   #  #  " ::
            "v      >  " ::
            "   #### ##" ::
            "     ^    " ::
            "###  #  # " ::
            "  <###  ##" ::
            "       >  " ::
            "    ##    " ::
            Nil
        )) {
            case Ok(b) => b
            case Err(msg) => bug!(msg)
        };
        let moveLogics = Map#{
            0 => smartSearchAndDestroy(), 
            1 => wander(), 
            2 => wander(),
            3 => wander(),
            4 => wander(),
            5 => wander(),
            6 => wander()
        };
        (board, moveLogics)

    // Test Scenarios

    pub def testDisplayBoard01(): Unit \ IO =
        displayBoard(fst(testBoard01()))

    pub def testRunBoard01(): Unit \ IO =
        // OBS letbinding is avoided because of bug
        // https://github.com/flix/flix/issues/4440
        testBoard01() |> match (board, strategies) ->
        runGameState(500i64, mkGameState(42i64, strategies, board))

    pub def testDisplayBoard02(): Unit \ IO =
        displayBoard(fst(testBoard02()))

    pub def testRunBoard02(): Unit \ IO =
        testBoard02() |> match (board, strategies) ->
        runGameState(500i64, mkGameState(42i64, strategies, board))

    pub def testDisplayBoard03(): Unit \ IO =
        displayBoard(fst(testBoard03()))

    pub def testRunBoard03(): Unit \ IO =
        testBoard03() |> match (board, strategies) ->
        runGameState(500i64, mkGameState(42i64, strategies, board))

    pub def testDisplayBoard04(): Unit \ IO =
        displayBoard(testBoard04())

    pub def testDisplayBoard05(): Unit \ IO =
        displayBoard(testBoard05())

    pub def testRunBoard06(): Unit \ IO =
        testBoard06() |> match (board, strategies) ->
        runGameState(500i64, mkGameState(42i64, strategies, board))

    pub def testRunBoard07(): Unit \ IO =
        testBoard07() |> match (board, strategies) ->
        runGameState(500i64, mkGameState(42i64, strategies, board))

}