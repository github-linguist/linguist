package main
import "fmt"
import "regexp"

func main() {
  str := "I am the original string"

  // Test
  matched, _ := regexp.MatchString(".*string$", str)
  if matched { fmt.Println("ends with 'string'") }

  // Substitute
  pattern := regexp.MustCompile("original")
  result := pattern.ReplaceAllString(str, "modified")
  fmt.Println(result)
}
