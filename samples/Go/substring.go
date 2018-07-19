package main
import "fmt"
import "strings"

func main() {
  s := "ABCDEFGH"
  n, m := 2, 3

  fmt.Println(s[n:n+m]) // "CDE"
  fmt.Println(s[n:]) // "CDEFGH"
  fmt.Println(s[0:len(s)-1]) // "ABCDEFG"
  fmt.Println(s[strings.Index(s, "D"):strings.Index(s, "D")+m]) // "DEF"
  fmt.Println(s[strings.Index(s, "DE"):strings.Index(s, "DE")+m]) // "DEF"
}
