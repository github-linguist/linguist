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

mod Flixball.Core.GameState {
    use Flixball.Core.AiState
    use Flixball.Core.Coordinates
    use Flixball.Core.Move
    use Flixball.Core.MoveLogic
    use Flixball.Core.PlayerId
    use Flixball.Core.Position
    use Flixball.Core.Position.Position
    use Flixball.Core.rotateDir
    use Flixball.Core.Rotation
    use Flixball.Core.Strategy
    use Flixball.Core.Tile.Person
    use Flixball.Core.Board.{adjust => boardAdjust}
    use Flixball.Core.Board.Board
    use Flixball.Core.Board.get_
    use Flixball.Core.Board.set_
    use Flixball.Core.Position.{coordinates => posCoords}
    use Flixball.Core.Position.{direction => posDir}

    use GameState.GameState

    type alias PlayerInfo = {
        state = AiState,
        ml = MoveLogic,
        pos = Position
    }

    pub enum GameState(
        Map[PlayerId, PlayerInfo],
        Board
    )

    //law gamestatea =

    /// Returns the board of the state.
    pub def board(s: GameState): Board = {
        let GameState(_, b) = s;
        b
    }

    ///
    /// Gets the position of the given player.
    ///
    pub def playerPosition(id: PlayerId, s: GameState): Option[Position] = {
        let GameState(players, _) = s;
        players |> Map.get(id) |> Option.map(info -> info#pos)
    }

    /// Position updates are naively done without care for collisions or inconsistensies.
    pub def map(f: PlayerInfo -> PlayerInfo, s: GameState): GameState =
        mapWithId((_, info) -> f(info), s)

    /// Position updates are naively done without care for collisions or inconsistensies.
    pub def mapWithId(f: (PlayerId, PlayerInfo) -> PlayerInfo, s: GameState): GameState = region r {
        let GameState(players0, board0) = s;
        let board = Ref.fresh(r, board0);
        let players = players0 |> Map.mapWithKey((id, info0) -> {
            let info = f(id, info0);
            if (info#pos != info0#pos) {
                // remove previous
                Ref.put(set_(posCoords(info0#pos), None, Ref.get(board)), board);
                // insert new
                Ref.put(set_(posCoords(info#pos), Some(Person(id, posDir(info#pos))), Ref.get(board)), board)
            } else ();
            info
        });
        GameState(players, Ref.get(board))
    }

    /// Adjusts the playerinfo of player `id`. Position updates are not verified.
    pub def adjust(id: PlayerId, f: PlayerInfo -> PlayerInfo, s: GameState): GameState = region r {
        let GameState(players0, board0) = s;
        let board = Ref.fresh(r, board0);
        let players = players0 |> Map.adjust(info0 -> {
            let info = f(info0);
            if (info#pos != info0#pos) {
                Ref.put(set_(posCoords(info0#pos), None, Ref.get(board)), board);
                Ref.put(set_(posCoords(info#pos), Some(Person(id, posDir(info#pos))), Ref.get(board)), board)
            } else ();
            info
        }, id);
        GameState(players, Ref.get(board))
    }

    /// Computes the logics of all players, returning their map of moves.
    pub def computeMoveLogics(s: GameState): (Map[PlayerId, Move], GameState) = {
        let GameState(players0, b0) = s;
        let (moves, players) = players0 |> Map.mapWithKey((_, info) -> {
            let (move, state) = (info#ml)(Flixball.Core.AiInfo.fromBoard(b0), info#state);
            (move, {state = state | info})
        }) |> Utils.Map.unzip;
        (moves, GameState(players, b0))
    }

    /// Removes a player from the game.
    /// Does nothing if the player does not exist in the game.
    pub def removePlayer(id: PlayerId, state0: GameState): GameState = {
        match (playerPosition(id, state0)) {
            // Case 1: No such player. Do nothing.
            case None => state0

            // Case 2: Found the player. Remove from the map, the board, and the states.
            case Some(Position(coords, _)) => {
                let GameState(players0, board0) = state0;
                let board = board0 |> set_(coords, None);
                let players = players0 |> Map.remove(id);
                GameState(players, board)
            }
        }
    }

    /// Rotates a player with the given rotation.
    /// Does nothing if the player does not exist in the game.
    pub def rotatePlayer(id: PlayerId, rot: Rotation, gs: GameState): GameState =
        gs |> adjust(id, info -> {
            let pos = Position(posCoords(info#pos), posDir(info#pos) |> rotateDir(rot));
            {pos = pos | info}
        })

    /// Moves a player to a new position.
    /// Does not change the direction of the player.
    pub def movePlayer(id: PlayerId, coords: Coordinates, gs: GameState): GameState =
        gs |> adjust(id, info -> {
            let pos = Position(coords, posDir(info#pos));
            {pos = pos | info}
        })

    ///
    /// Create a gamestate based on a seed (for ais), strategies and a board.
    /// Precondition: The domain of `strategies` must be `[0,n]` where `n` is
    /// the number of players on the board.
    ///
    pub def mkGameState(seed: Int64, strategies: Map[PlayerId, Strategy], b: Board): GameState = {
        let playerMap = Flixball.Core.Board.players(b);
        let ais = strategies |> Map.mapWithKey((id, strat) -> {
            let (stateGen, ml) = strat(id);
            // unsafe get is safe per precondition.
            let pos = Map.get(id, playerMap) |> Utils.Option.unsafeGet;
            {state = stateGen(seed), ml = ml, pos = pos}
        });
        GameState(ais, b)
    }
}