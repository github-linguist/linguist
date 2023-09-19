import board.aleo;
import move.aleo;
import verify.aleo;
program battleship.aleo;




function initialize_board:
    input r0 as u64.private;
    input r1 as u64.private;
    input r2 as u64.private;
    input r3 as u64.private;
    input r4 as address.private;
    call verify.aleo/validate_ship r0 5u64 31u64 4311810305u64 into r5;
    assert.eq r5 true;
    call verify.aleo/validate_ship r1 4u64 15u64 16843009u64 into r6;
    assert.eq r6 true;
    call verify.aleo/validate_ship r2 3u64 7u64 65793u64 into r7;
    assert.eq r7 true;
    call verify.aleo/validate_ship r3 2u64 3u64 257u64 into r8;
    assert.eq r8 true;
    call verify.aleo/create_board r0 r1 r2 r3 into r9;
    call board.aleo/new_board_state r9 r4 into r10;
    output r10 as board.aleo/board_state.record;


function offer_battleship:
    input r0 as board.aleo/board_state.record;
    call board.aleo/start_board r0 into r1;
    call move.aleo/start_game r0.player_2 into r2;
    output r1 as board.aleo/board_state.record;
    output r2 as move.aleo/move.record;


function start_battleship:
    input r0 as board.aleo/board_state.record;
    input r1 as move.aleo/move.record;
    assert.eq r0.player_1 r1.player_2;
    assert.eq r0.player_2 r1.player_1;
    call board.aleo/start_board r0 into r2;
    call move.aleo/start_game r0.player_2 into r3;
    output r2 as board.aleo/board_state.record;
    output r3 as move.aleo/move.record;


function play:
    input r0 as board.aleo/board_state.record;
    input r1 as move.aleo/move.record;
    input r2 as u64.private;
    assert.eq r0.game_started true;
    assert.eq r0.player_1 r1.player_2;
    assert.eq r0.player_2 r1.player_1;
    call board.aleo/update_played_tiles r0 r2 into r3;
    call board.aleo/update_hits_and_misses r3 r1.prev_hit_or_miss into r4;
    and r1.incoming_fire_coordinate r0.ships into r5;
    call move.aleo/create_move r1 r2 r5 into r6;
    output r4 as board.aleo/board_state.record;
    output r6 as move.aleo/move.record;
