import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do
  getArgsAndInitialize
  createWindow "Triangle"
  displayCallback $= display

  matrixMode $= Projection
  loadIdentity
  ortho2D 0 30 0 30
  matrixMode $= Modelview 0

  mainLoop

display = do
  clear [ColorBuffer]
  renderPrimitive Triangles $ do
    corner 1 0 0 5 5
    corner 0 1 0 25 5
    corner 0 0 1 5 25
  swapBuffers

corner r g b x y = do color  (Color3  r g b :: Color3  GLfloat)
                      vertex (Vertex2 x y   :: Vertex2 GLfloat)
