import Graphics.UI.Gtk
import Data.IORef

main :: IO ()
main = do
  initGUI
  window <- windowNew
  window `onDestroy` mainQuit
  windowSetTitle window "Simple Windowed App"
  set window [ containerBorderWidth := 10 ]

  hbox <- hBoxNew True 5

  set window [ containerChild := hbox ]

  lab <- labelNew (Just "There have been no clicks yet")
  button <- buttonNewWithLabel "Click me"
  set hbox [ containerChild := lab ]
  set hbox [ containerChild := button ]

  m <- newIORef 0

  onClicked button $ do
    v <- readIORef m
    writeIORef m (v+1)
    set lab [ labelText := "There have been " ++ show (v+1) ++ " clicks" ]

  widgetShowAll window

  mainGUI
