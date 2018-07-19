import Graphics.UI.Gtk
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

maximumWindowDimensions :: IO ()
maximumWindowDimensions = do
    -- initialize the internal state of the GTK toolkit
    initGUI
    -- create a window
    window <- windowNew
    -- quit the application when the window is closed
    on window objectDestroy mainQuit
    -- query the size of the window when its dimensions change
    on window configureEvent printSize
    -- get the screen the window will be drawn upon
    screen <- windowGetScreen window
    -- get the size of the screen
    x <- screenGetWidth screen
    y <- screenGetHeight screen
    -- print the dimensions of the screen
    putStrLn ("The screen is " ++ show x ++ " pixels wide and " ++
        show y ++ " pixels tall for an undecorated fullscreen window.")
    -- maximize the window and show it. printSize will then be called
    windowMaximize window
    widgetShowAll window
    -- run the main GTK loop.
    -- close the window manually.
    mainGUI

-- On my Xfce4 desktop, the configure_event is called three times when a
-- top level window is maximized. The first time, the window size
-- returned is the size prior to maximizing, and the last two times
-- it is the size after maximizing.
-- If the window is (un)maximized manually, the size returned is always
-- the size of the unmaximized window.
-- That means: either GTK or Xfce4 does not handle window maximization
-- correctly, or the GTK bindings for Haskell are buggy, or there is an
-- error in this program.

printSize :: EventM EConfigure Bool
printSize = do
    -- get the window that has been resized
    w <- eventWindow
    -- is the window maximized?
    s <- liftIO $ drawWindowGetState w
    when (WindowStateMaximized `elem` s) $ do
        -- get the size of the window that has been resized
        (x, y) <- eventSize
        -- print the dimensions out
        liftIO $ putStrLn ("The inner window region is now " ++ show x ++
            " pixels wide and " ++ show y ++ " pixels tall.")
    return True
