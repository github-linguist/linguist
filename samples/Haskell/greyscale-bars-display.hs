import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Control.Monad.Trans (liftIO)

-- click on the window to exit.

main = do
    initGUI

    window <- windowNew

    buf <- pixbufNewFromXPMData bars

    widgetAddEvents window [ButtonPressMask]
    on window objectDestroy mainQuit
    on window exposeEvent (paint buf)
    on window buttonPressEvent $
        liftIO $ do { widgetDestroy window; return True }

    windowFullscreen window
    widgetShowAll window

    mainGUI

paint :: Pixbuf -> EventM EExpose Bool
paint buf = do
    pix <- eventWindow
    liftIO $ do
        (sx, sy) <- drawableGetSize pix
        newBuf <- pixbufScaleSimple buf sx sy InterpNearest
        gc <- gcNewWithValues pix newGCValues
        drawPixbuf pix gc newBuf 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
        return True

bars :: [String]
bars = [
    "64 4 65 1 1 1","  c None","A c #000000",
    "C c #080808","D c #0C0C0C","E c #101010","F c #141414",
    "G c #181818","H c #1C1C1C","I c #202020","J c #242424",
    "K c #282828","L c #2C2C2C","M c #303030","N c #343434",
    "O c #383838","P c #3C3C3C","Q c #404040","R c #444444",
    "S c #484848","T c #4C4C4C","U c #505050","V c #545454",
    "W c #585858","X c #5C5C5C","Y c #606060","Z c #646464",
    "a c #686868","b c #6C6C6C","c c #707070","d c #747474",
    "e c #787878","f c #7C7C7C","g c #808080","h c #848484",
    "i c #888888","j c #8C8C8C","k c #909090","l c #949494",
    "m c #989898","n c #9C9C9C","o c #A0A0A0","p c #A4A4A4",
    "q c #A8A8A8","r c #ACACAC","s c #B0B0B0","t c #B4B4B4",
    "u c #B8B8B8","v c #BCBCBC","w c #C0C0C0","x c #C4C4C4",
    "y c #C8C8C8","z c #CCCCCC","0 c #D0D0D0","1 c #D4D4D4",
    "2 c #D8D8D8","3 c #DCDCDC","4 c #E0E0E0","5 c #E4E4E4",
    "6 c #E8E8E8","7 c #ECECEC","8 c #F0F0F0","9 c #F4F4F4",
    ". c #F8F8F8","+ c #FCFCFC","* c #FFFFFF",
    "AAAAAAAAJJJJJJJJRRRRRRRRZZZZZZZZhhhhhhhhppppppppxxxxxxxx********",
    "****88881111xxxxttttppppllllhhhhddddZZZZVVVVRRRRNNNNJJJJFFFFAAAA",
    "AADDFFHHJJLLNNPPRRTTVVXXZZbbddffhhjjllnnpprrttvvxxzz11336688..**",
    "*+.9876543210zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCA"
     ]
