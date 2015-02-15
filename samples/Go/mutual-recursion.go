package main
import "fmt"

func F(n int) int {
  if n == 0 { return 1 }
  return n - M(F(n-1))
}

func M(n int) int {
  if n == 0 { return 0 }
  return n - F(M(n-1))
}

func main() {
  for i := 0; i < 20; i++ {
    fmt.Printf("%2d ", F(i))
  }
  fmt.Println()
  for i := 0; i < 20; i++ {
    fmt.Printf("%2d ", M(i))
  }
  fmt.Println()
}
