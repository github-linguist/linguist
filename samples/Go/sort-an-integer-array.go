package main
import "fmt"
import "sort"

func main() {
  nums := []int {2, 4, 3, 1, 2}
  sort.Ints(nums)
  fmt.Println(nums)
}
