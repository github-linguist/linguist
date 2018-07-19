package main

import(
	"fmt"
	"path"
	"strings"
)

func CommonPrefix(sep string, paths ...string) (string) {
	// Handle special cases.
	switch len(paths) {
		case 0:
			return ""
		case 1:
			return path.Clean(paths[0])
	}

	c := []byte(path.Clean(paths[0]))

	// Ignore the first path since it's already in c.
	for _, v := range(paths[1:]) {
		// Clean up each path before testing it.
		v = path.Clean(v)

		// Get the length of the shorter slice.
		shorter := len(v)
		if len(v) > len(c) {
			shorter = len(c)
		}

		// Find the first non-common character and copy up to it into c.
		for i := 0; i < shorter; i++ {
			if v[i] != c[i] {
				c = c[0:i]
				break
			}
		}
	}

	// Correct for problem caused by prepending the actual common path to the
	// list of paths searched through.
	for _, v := range(paths) {
		if len(v) > len(c) {
			if strings.HasPrefix(v, string(c)) {
				if len(v) > len(c) + len(sep) {
					if v[len(c):len(c)+len(sep)] == sep {
						c = append(c, []byte(sep)...)
						break
					}
				}
			}
		}
	}

	// Remove trailing non-seperator characters.
	for i := len(c)-1; i >= 0; i-- {
		if i + len(sep) > len(c) {
			continue
		}

		if string(c[i:i+len(sep)]) == sep {
			c = c[0:i]
			break
		}
	}

	return string(c)
}

func main() {
	c := CommonPrefix("/",
		"/home/user1/tmp/coverage/test",
		"/home/user1/tmp/covert/operator",
		"/home/user1/tmp/coven/members",
	)
	fmt.Printf("%v\n", c)
}
