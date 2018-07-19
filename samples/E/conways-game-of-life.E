def gridWidth := 3
def gridHeight := 3
def X := 0..!gridWidth
def Y := 0..!gridHeight

def makeFlexList := <elib:tables.makeFlexList>
def makeGrid() {
  def storage := makeFlexList.fromType(<type:java.lang.Boolean>, gridWidth * gridHeight)
  storage.setSize(gridWidth * gridHeight)

  def grid {
    to __printOn(out) {
      for y in Y {
        out.print("[")
        for x in X {
          out.print(grid[x, y].pick("#", " "))
        }
        out.println("]")
      }
    }
    to get(xb :int, yb :int) {
      return if (xb =~ x :X && yb =~ y :Y) {
        storage[y * gridWidth + x]
      } else {
        false
      }
    }
    to put(x :X, y :Y, c :boolean) {
      storage[y * gridWidth + x] := c
    }
  }
  return grid
}

def mooreNeighborhood := [[-1,-1],[0,-1],[1,-1],[-1,0],[1,0],[-1,1],[0,1],[1,1]]
def computeNextLife(prevGrid, nextGrid) {
  for y in Y {
    for x in X {
      var neighbors := 0
      for [nx, ny] ? (prevGrid[x+nx, y+ny]) in mooreNeighborhood {
        neighbors += 1
      }
      def self := prevGrid[x, y]
      nextGrid[x, y] := (self && neighbors == 2 || neighbors == 3)
    }
  }
}

var currentFrame := makeGrid()
var nextFrame := makeGrid()
currentFrame[1, 0] := true
currentFrame[1, 1] := true
currentFrame[1, 2] := true

for _ in 1..3 {
  def frame := nextFrame
  computeNextLife(currentFrame, frame)
  nextFrame := currentFrame
  currentFrame := frame
  println(currentFrame)
}
