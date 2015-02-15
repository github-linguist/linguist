package main
import "os"

func main() {
  os.Remove("input.txt")
  os.Remove("/input.txt")
  os.Remove("docs")
  os.Remove("/docs")
  // recursively removes contents:
  os.RemoveAll("docs")
  os.RemoveAll("/docs")
}
