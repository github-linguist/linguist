package main

import (
	"bufio"
	"bytes"
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"net/smtp"
	"os"
	"strings"
)

type Message struct {
	From    string
	To      []string
	Cc      []string
	Subject string
	Content string
}

func (m Message) Bytes() (r []byte) {
	to := strings.Join(m.To, ",")
	cc := strings.Join(m.Cc, ",")

	r = append(r, []byte("From: "+m.From+"\n")...)
	r = append(r, []byte("To: "+to+"\n")...)
	r = append(r, []byte("Cc: "+cc+"\n")...)
	r = append(r, []byte("Subject: "+m.Subject+"\n\n")...)
	r = append(r, []byte(m.Content)...)

	return
}

func (m Message) Send(host string, port int, user, pass string) (err error) {
	err = check(host, user, pass)
	if err != nil {
		return
	}

	err = smtp.SendMail(fmt.Sprintf("%v:%v", host, port),
		smtp.PlainAuth("", user, pass, host),
		m.From,
		m.To,
		m.Bytes(),
	)

	return
}

func check(host, user, pass string) error {
	if host == "" {
		return errors.New("Bad host")
	}
	if user == "" {
		return errors.New("Bad username")
	}
	if pass == "" {
		return errors.New("Bad password")
	}

	return nil
}

func main() {
	var flags struct {
		host string
		port int
		user string
		pass string
	}
	flag.StringVar(&flags.host, "host", "", "SMTP server to connect to")
	flag.IntVar(&flags.port, "port", 587, "Port to connect to SMTP server on")
	flag.StringVar(&flags.user, "user", "", "Username to authenticate with")
	flag.StringVar(&flags.pass, "pass", "", "Password to authenticate with")
	flag.Parse()

	err := check(flags.host, flags.user, flags.pass)
	if err != nil {
		flag.Usage()
		os.Exit(1)
	}

	bufin := bufio.NewReader(os.Stdin)

	fmt.Printf("From: ")
	from, err := bufin.ReadString('\n')
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}
	from = strings.Trim(from, " \t\n\r")

	var to []string
	for {
		fmt.Printf("To (Blank to finish): ")
		tmp, err := bufin.ReadString('\n')
		if err != nil {
			fmt.Printf("Error: %v\n", err)
			os.Exit(1)
		}
		tmp = strings.Trim(tmp, " \t\n\r")

		if tmp == "" {
			break
		}

		to = append(to, tmp)
	}

	var cc []string
	for {
		fmt.Printf("Cc (Blank to finish): ")
		tmp, err := bufin.ReadString('\n')
		if err != nil {
			fmt.Printf("Error: %v\n", err)
			os.Exit(1)
		}
		tmp = strings.Trim(tmp, " \t\n\r")

		if tmp == "" {
			break
		}

		cc = append(cc, tmp)
	}

	fmt.Printf("Subject: ")
	subject, err := bufin.ReadString('\n')
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}
	subject = strings.Trim(subject, " \t\n\r")

	fmt.Printf("Content (Until EOF):\n")
	content, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}
	content = bytes.Trim(content, " \t\n\r")

	m := Message{
		From:    from,
		To:      to,
		Cc:      cc,
		Subject: subject,
		Content: string(content),
	}

	fmt.Printf("\nSending message...\n")
	err = m.Send(flags.host, flags.port, flags.user, flags.pass)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Message sent.\n")
}
