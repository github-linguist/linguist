package main
import (
	"fmt"
	"io/ioutil"
	"strings"
	"sort"
)

func deranged(a, b string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range(a) {
		if a[i] == b[i] { return false }
	}
	return true
}

func main() {
	/* read the whole thing in. how big can it be? */
	buf, _ := ioutil.ReadFile("unixdict.txt")
	words := strings.Split(string(buf), "\n")

	m := make(map[string] []string)
	best_len, w1, w2 := 0, "", ""

	for _, w := range(words) {
		// don't bother: too short to beat current record
		if len(w) <= best_len { continue }

		// save strings in map, with sorted string as key
		letters := strings.Split(w, "")
		sort.Strings(letters)
		k := strings.Join(letters, "")

		if _, ok := m[k]; !ok {
			m[k] = []string { w }
			continue
		}

		for _, c := range(m[k]) {
			if deranged(w, c) {
				best_len, w1, w2 = len(w), c, w
				break
			}
		}

		m[k] = append(m[k], w)
	}

	fmt.Println(w1, w2, ": Length", best_len)
}
