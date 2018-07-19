package main
import "fmt"

func func1(f func(string) string) string { return f("a string") }
func func2(s string) string { return "func2 called with " + s }
func main() { fmt.Println(func1(func2)) }
