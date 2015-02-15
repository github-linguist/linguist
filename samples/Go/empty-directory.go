package main

import (
	"fmt"
	"io/ioutil"
	"log"
)

func main() {
	empty, err := IsEmptyDir("/tmp")
	if err != nil {
		log.Fatalln(err)
	}
	if empty {
		fmt.Printf("/tmp is empty\n")
	} else {
		fmt.Printf("/tmp is not empty\n")
	}
}

func IsEmptyDir(name string) (bool, error) {
	entries, err := ioutil.ReadDir(name)
	if err != nil {
		return false, err
	}
	return len(entries) == 0, nil
}
