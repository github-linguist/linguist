package main
import "fmt"
import "os/exec"

func main() {
  cmd := exec.Command("ls", "-l")
  output, err := cmd.Output()
  if (err != nil) {
    fmt.Println(err)
    return
  }
  fmt.Print(string(output))
}
