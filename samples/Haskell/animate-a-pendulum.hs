import Graphics.HGL.Draw.Monad (Graphic, )
import Graphics.HGL.Draw.Picture
import Graphics.HGL.Utils
import Graphics.HGL.Window
import Graphics.HGL.Run

import Control.Exception (bracket, )
import Control.Arrow

toInt = fromIntegral.round

pendulum = runGraphics $
  bracket
    (openWindowEx "Pendulum animation task" Nothing (600,400) DoubleBuffered (Just 30))
    closeWindow
    (\w -> mapM_ ((\ g -> setGraphic w g >> getWindowTick w).
		    (\ (x, y) -> overGraphic (line (300, 0) (x, y))
				  (ellipse (x - 12, y + 12) (x + 12, y - 12)) )) pts)
 where
    dt = 1/30
    t = - pi/4
    l = 1
    g = 9.812
    nextAVT (a,v,t) = (a', v', t + v' * dt) where
	a' = - (g / l) * sin t
	v' = v + a' * dt
    pts = map (\(_,t,_) -> (toInt.(300+).(300*).cos &&& toInt. (300*).sin) (pi/2+0.6*t) )
	$ iterate nextAVT (- (g / l) * sin t, t, 0)
