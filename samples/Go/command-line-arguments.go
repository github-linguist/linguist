package main
import (
  "fmt"
  "os"
)

func main() {
  for i, x := range os.Args {
    if i == 0 {
      fmt.Printf("This program is named %s.\n", x)
    } else {
      fmt.Printf("the argument #%d is %s\n", i, x)
    }
  }
}
