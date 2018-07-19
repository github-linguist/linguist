import Graphics.X11.Xlib
import Control.Concurrent (threadDelay)

main = do
  display <- openDisplay ""
  let defScr = defaultScreen display
  rw <- rootWindow display defScr

  xwin <- createSimpleWindow display rw
      0 0 400 200 1
      (blackPixel display defScr)
      (whitePixel display defScr)

  setTextProperty display xwin "Rosetta Code: X11 simple window" wM_NAME

  mapWindow display xwin

  sync display False
  threadDelay (5000000)

  destroyWindow display xwin
  closeDisplay display
