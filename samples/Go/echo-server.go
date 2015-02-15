package main

import (
	"fmt"
	"net"
	"bufio"
)

func echo(s net.Conn, i int) {
	defer s.Close();

	fmt.Printf("%d: %v <-> %v\n", i, s.LocalAddr(), s.RemoteAddr())
	b := bufio.NewReader(s)
	for {
		line, e := b.ReadBytes('\n')
		if e != nil {
			break
		}
		s.Write(line)
	}
	fmt.Printf("%d: closed\n", i)
}

func main() {
	l, e := net.Listen("tcp", ":12321")
	for i := 0; e == nil; i++ {
		var s net.Conn
		s, e = l.Accept()
		go echo(s, i)
	}
}
