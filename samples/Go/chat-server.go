package main

import (
	"os"
	"fmt"
	"net"
	"flag"
	"bufio"
	"bytes"
)

// Quick and dirty error handling.
func error_(err error, r int) {
	fmt.Printf("Error: %v\n", err)

	if r >= 0 {
		os.Exit(r)
	}
}

// A type for storing the connections.
type clientMap map[string]net.Conn

// A method that makes clientMap compatible with io.Writer, allowing it to be
// used with fmt.Fprintf().
func (cm clientMap) Write(buf []byte) (n int, err error) {
	for _, c := range cm {
		// Write to each client in a seperate goroutine.
		go c.Write(buf)
	}

	n = len(buf)

	return
}

// Check if a name exists; if it doesn't, add it.
func (cm clientMap) Add(name string, c net.Conn) bool {
	for k := range cm {
		if name == k {
			return false
		}
	}

	cm[name] = c

	return true
}

// A clientMap variable.
var clients clientMap

func init() {
	// Initialize the map.
	clients = make(clientMap)
}

func client(c net.Conn) {
	// Close the connection when this function returns.
	defer c.Close()

	br := bufio.NewReader(c)

	fmt.Fprintf(c, "Please enter your name: ")

	buf, err := br.ReadBytes('\n')
	if err != nil {
		error_(err, -1)
		return
	}
	name := string(bytes.Trim(buf, " \t\n\r\x00"))

	if name == "" {
		fmt.Fprintf(c, "!!! %v is invalid !!!\n", name)
	}

	// Try to add the connection to the map.
	if !clients.Add(name, c) {
		fmt.Fprintf(c, "!!! %v is not available !!!\n", name)
		return
	}

	// Send a message telling the clients who connected.
	fmt.Fprintf(clients, "+++ %v connected +++\n", name)
	// Send a disconnected message when the function returns.
	defer fmt.Fprintf(clients, "--- %v disconnected ---\n", name)
	// Remove the client from the list.
	defer delete(clients, name)

	for {
		buf, err = br.ReadBytes('\n')
		if err != nil {
			break
		}
		buf = bytes.Trim(buf, " \t\n\r\x00")

		// Ignore empty messages.
		if len(buf) == 0 {
			continue
		}

		switch {
		// Support for '/me' type messages.
		case string(buf[0:3]) == "/me":
			buf = append([]byte(name), buf[3:]...)
		default:
			// Prepend the user-name and '> '.
			buf = append([]byte(name+"> "), buf...)
		}

		// Send the message to all the clients.
		fmt.Fprintf(clients, "%v\n", string(buf))
	}
}

func main() {
	// Flags. Use -help for usage info.
	var (
		port int
		help bool
	)
	flag.IntVar(&port, "port", 23, "Port to listen on")
	flag.BoolVar(&help, "help", false, "Display this")
	flag.Parse()

	if help {
		flag.Usage()
		return
	}

	// Initialize a new listener.
	lis, err := net.Listen("tcp", fmt.Sprintf(":%v", port))
	if err != nil {
		error_(err, 1)
	}

	// Begin the main loop.
	for {
		c, err := lis.Accept()
		if err != nil {
			error_(err, -1)
			continue
		}

		// Launch a new goroutine to handle the connection.
		go client(c)
	}
}
