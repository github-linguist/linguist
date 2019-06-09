package compiler

import (
	"bytes"
	"os/exec"
)

func ConvertCSON(data []byte) ([]byte, error) {
	stdin := bytes.NewBuffer(data)
	stdout := &bytes.Buffer{}

	cmd := exec.Command("csonc")
	cmd.Stdin = stdin
	cmd.Stdout = stdout

	if err := cmd.Run(); err != nil {
		return nil, err
	}

	return stdout.Bytes(), nil
}
