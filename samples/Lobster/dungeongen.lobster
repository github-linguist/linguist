// based on: https://gist.github.com/munificent/b1bcd969063da3e6c298be070a22b604
// via: https://gist.github.com/Joker-vD/cc5372a349559b9d1a3b220d5eaf2b01

import std
import vec
import color
import gl

let TILE_VOID        = ' '
let TILE_FLOOR       = '.'
let TILE_WALL        = '#'
let TILE_CORNER      = '!'
let TILE_OPEN_DOOR   = '\''
let TILE_CLOSED_DOOR = '+'
let TILE_PLAYER      = '@'

let fsize = int2 { 80, 40 }
let field = mapxy(fsize): TILE_VOID

def cave(with_player):
    // csize/start are all inner dimensions/coordinates (w/o walls)
    let csize = rnd_int2(int2 { 10, 6 }) + int2 { 5, 3 }
    let start = rnd_int2(fsize - csize - 2) + 1

    // Cave iterator function that supplies wall/corner type.
    def in_box(f):
        forxy(csize + 2) v:
            v += start - 1
            let at_wall = (v < start) + (v >= start + csize)
            // We need to somehow record corners of all caves to check
            // for intersections later, so we use a special tile for it
            f(v, [ TILE_FLOOR, TILE_WALL, TILE_CORNER ][manhattan(at_wall)])

    // Check if the new cave (with walls) intersects with the interior of
    // any already existing cave. Touching walls/corners are okay
    in_box() v:
        if field[v] == TILE_FLOOR: return

    // Find a suitable place for a door
    var door_counter = 0
    var door_pos = int2_0
    if not with_player:
        in_box() v, tile:
            // The door should not be created in the cave's corner or over
            // another door, or in another cave's corner. It's impossible
            // to make a cave without a door, because rnd always
            // returns 0.
            if tile == TILE_WALL and field[v] == TILE_WALL:
                door_counter++
                if rnd(door_counter) == 0: door_pos = v

        // If the cave's walls were made completely out of corners
        // and doors, don't make such a cave
        if door_counter == 0: return

    // The cave looks okay, let's draw it. First, draw the walls and the floor
    in_box() v, tile:
        field[v] = tile

    // Now draw the door.
    if door_counter > 0:
        field[door_pos] = if rnd(2): TILE_OPEN_DOOR else: TILE_CLOSED_DOOR

    if with_player:
        // A cave with the player has only the player inside it
        field[rnd_int2(csize) + start] = TILE_PLAYER
    else:
        // A cave without the player has some random mobs and/or gold in it;
        // 1d6 of entities total, 25% chance of gold, 75% of a mob.
        // Mob letters range from 'A' to '~', inclusive
        for rnd(6) + 1:
            field[rnd_int2(csize) + start] =
                if rnd(4) == 0: '$' else: 'A' + rnd('~' - 'A' + 1)

rnd_seed(int(seconds_elapsed() * 1000000.0))

// A call to cave() is not guaranteed to actually make a new cave,
// so call it many times
for(1000) j:
    cave(j == 0)

// Remove special corner type.
forxy(fsize) v:
    if field[v] ==  TILE_CORNER:
        field[v] = TILE_WALL

// Print the generated field
for(field) row:
    print unicode_to_string(row)
