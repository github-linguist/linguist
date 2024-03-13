import uint from std::integer
import Window,Document,Element from w3c::dom
import HTMLCanvasElement,MouseEvent,EventListener from w3c::dom

import model
import view

/**
 * Controller state.  This just simplifies the task of passing around
 * the information necessary to manage the game and user interactions.
 */
public type State is {
    // Enclosing Window (for animation)
    Window window,
    // Canvas for drawing
    HTMLCanvasElement canvas,    
    // State of game
    model::State game,
    // Flag when executing
    bool running
}

method State(Window window, HTMLCanvasElement canvas, model::State game) -> State:
    // This method should be deprecated in the future!
    return { window: window, canvas: canvas, game: game, running: false }

/**
 * Main entrypoint for the game.
 */
public export method main(Window window, HTMLCanvasElement canvas, uint width, uint height):
    Document document = window->document
    // Get items
    Element c = document->getElementById("mycanvas")
    Element b = document->getElementById("startstop")
    model::State m = model::init(width,height)
    // Create peristent state object
    &State st = new State(window, canvas, m)
    // Configure mouse click listener
    EventListener el1 = &(MouseEvent e -> onclick_canvas(e,st))
    EventListener el2 = &(MouseEvent e -> onclick_button(b,st))
    c->addEventListener("click", el1)
    b->addEventListener("click", el2)
    // Begin game loop
    loop(st)

/**
 * Click handler for board.  This determines the game cell on which
 * the user clicked, and updates the game state accordingly.
 */
method onclick_canvas(MouseEvent e, &State st):
   int x = e->offsetX / 20
   int y = e->offsetY / 20
   // Apply click to model state
   model::State g = model::click(x,y,st->game)
   // Update state object
   st->game.cells = g.cells

/**
 * Click start / stop button.  This starts or stops execution
 * accordingly, and updates the text for the button as well.
 */
method onclick_button(Element b, &State st):
    //
    if st->running:
        st->running = false
        b->textContent = "START"
    else:
        st->running = true    
        b->textContent = "STOP"        

/**
 * Game loop to be called on every frame.  If the game is executing,
 * then this updates the game state.  Eitherway, the board is redrawn
 * to the canvas completely.
 */
method loop(&State st):
    // Update game state (if running)
    if st->running:
        // Execute one step
        model::State g = model::update(st->game)    
        // Update controller state
        st->game.cells = g.cells        
    // Render current game state
    view::draw(st->canvas,st->game)
    // Request next frame
    st->window->requestAnimationFrame(&(int ms -> loop(st)))
