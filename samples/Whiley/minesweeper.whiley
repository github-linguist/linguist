
import uint from std::integer
import uinteger from js::core
import random from js::math
import Document, Window, CanvasRenderingContext2D from w3c::dom
import Element,HTMLCanvasElement, HTMLImageElement, MouseEvent from w3c::dom

import model
import view

/**
 * Add a given number of bombs to the board.
 */
method add_random_bombs(model::Board board, uint n) -> model::Board:
    uinteger remaining = |board.squares|   
    // Use Knuth's algorithm S
    for x in 0..board.width:
        for y in 0..board.height:
            // Flip a coin (so-to-speak)
            if random(remaining) < n:
                // create bomb square
                model::Square s = model::HiddenSquare(true,false)
                // Update board
                board = model::set_square(board,(uint) x, (uint) y,s)
                // Reduce number of bombs to place
                n = n - 1
            // Reduce remaining options
            remaining = remaining - 1
    // return updated board
    return board

/**
 * Handle a mouse event on the canvas
 */
method onclick_handler(MouseEvent e, &view::State state, Window window):
    // Convert from view to world coordinates
    uint x = e->offsetX / state->gridsize
    uint y = e->offsetY / state->gridsize
    // Update board
    if e->shiftKey:
        state->board = model::flag_square(state->board,x,y)
    else:
        state->board = model::expose_square(state->board,x,y)
    // Render initial board
    view::draw_board(*state)
    // Finally determine game status
    (bool gameOver, bool winner) = model::is_gameover(state->board)
    // Check whether game over
    if gameOver:
        // Yes, but win or lose?
        if winner:
            window->alert("Well done --- You Found all the Mines!")
        else:
            window->alert("Game Over --- You Lost!")
    // Done

/**
 * Create a new game of Minesweeper
 */
public export method main(uint width, uint height, uint bombs, Window window, HTMLCanvasElement canvas, HTMLImageElement[] images)
// Requires at least 9 images
requires |images| == 13:
    Document document = window->document
    // NOTE: following should not be required!
    Element c = document->getElementById("myCanvas")
    // Create a standard sized board
    model::Board board = model::Board(width,height)
    // Add bombs
    board = add_random_bombs(board,bombs)
    // Initialise the view state
    &view::State state = new view::init(document,canvas,board,images)
    // Render initial board
    view::draw_board(*state)
    // Configure mouse click listener
    c->addEventListener("click",&(MouseEvent e -> onclick_handler(e,state,window)))
    
