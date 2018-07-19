import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- Draw a cuboid.  Its vertices are those of a unit cube, which is then scaled
-- to the required dimensions.  We only specify the visible faces, each of
-- which is composed of two triangles.  The faces are rotated into position and
-- rendered with a perspective transformation.

type Fl = GLfloat

cuboid :: IO ()
cuboid = do
  color red   ; render front
  color green ; render side
  color blue  ; render top

red,green,blue :: Color4 GLfloat
red   = Color4 1 0 0 1
green = Color4 0 1 0 1
blue  = Color4 0 0 1 1

render :: [(Fl, Fl, Fl)] -> IO ()
render = renderPrimitive TriangleStrip . mapM_ toVertex
  where toVertex (x,y,z) = vertex $ Vertex3 x y z

front,side,top :: [(Fl,Fl,Fl)]
front = vertices [0,1,2,3]
side  = vertices [4,1,5,3]
top   = vertices [3,2,5,6]

vertices :: [Int] -> [(Fl,Fl,Fl)]
vertices = map (verts !!)

verts :: [(Fl,Fl,Fl)]
verts = [(0,0,1), (1,0,1), (0,1,1), (1,1,1), (1,0,0), (1,1,0), (0,1,0)]

transform :: IO ()
transform = do
  translate $ Vector3 0 0 (-10 :: Fl)
  rotate (-14) $ Vector3 0 0 (1 :: Fl)
  rotate (-30) $ Vector3 0 1 (0 :: Fl)
  rotate   25  $ Vector3 1 0 (0 :: Fl)
  scale 2 3 (4 :: Fl)
  translate $ Vector3 (-0.5) (-0.5) (-0.5 :: Fl)

display :: IO ()
display = do
  clear [ColorBuffer]
  perspective 40 1 1 (15 :: GLdouble)
  transform
  cuboid
  flush

main :: IO ()
main = do
  let name = "Cuboid"
  initialize name []
  createWindow name
  displayCallback $= display
  mainLoop
