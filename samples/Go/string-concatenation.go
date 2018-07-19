package main

import "fmt"

func main() {
    // text assigned to a string variable
    s := "hello"

    // output string variable
    fmt.Println(s)

    // this output requested by original task descrption, although
    // not really required by current wording of task description.
    fmt.Println(s + " literal")

    // concatenate variable and literal, assign result to another string variable
    s2 := s + " literal"

    // output second string variable
    fmt.Println(s2)
}
