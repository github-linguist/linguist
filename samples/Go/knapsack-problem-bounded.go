package main
import "fmt"

type Item struct {
	name string
	weight, value, qty int
}

type Solution struct {
	v, w int
	qty []int
}

var items = []Item {
	{"map",			9,	150,	1},
	{"compass",		13,	35,	1},
	{"water",		153,	200,	2},
	{"sandwich",		50,	60,	2},
	{"glucose",		15,	60,	2},
	{"tin",			68,	45,	3},
	{"banana",		27,	60,	3},
	{"apple",		39,	40,	3},
	{"cheese",		23,	30,	1},
	{"beer",		52,	10,	3},
	{"suntancream",		11,	70,	1},
	{"camera",		32,	30,	1},
	{"T-shirt",		24,	15,	2},
	{"trousers",		48,	10,	2},
	{"umbrella",		73,	40,	1},
	{"w-trousers",		42,	70,	1},
	{"w-overclothes",	43,	75,	1},
	{"note-case",		22,	80,	1},
	{"sunglasses",		7,      20,	1},
	{"towel",		18,	12,	2},
	{"socks",		4,      50,	1},
	{"book",		30,	10,	2},
}

func choose(weight, pos int, cache map[string]*Solution) (int, int, []int) {
	if pos < 0 || weight <= 0 {
		return 0, 0, make([]int, len(items))
	}

	str := fmt.Sprintf("%d,%d", weight, pos)
	if s, ok := cache[str]; ok {
		return s.v, s.w, s.qty
	}

	best_v, best_i, best_w, best_sol := 0, 0, 0, []int(nil)

	for i := 0; i * items[pos].weight <= weight && i <= items[pos].qty; i++ {
		v, w, sol := choose(weight - i * items[pos].weight, pos - 1, cache)
		v += i * items[pos].value
		if v > best_v {
			best_i, best_v, best_w, best_sol = i, v, w, sol
		}
	}

	taken := make([]int, len(items))
	copy(taken, best_sol)
	taken[pos] = best_i
	v, w := best_v, best_w + best_i * items[pos].weight

	cache[str] = &Solution{v, w, taken}
	return v, w, taken
}

func main() {
	v, w, s := choose(400, len(items) - 1, make(map[string]*Solution))

	fmt.Println("Taking:")
	for i, t := range s {
		if t > 0 {
			fmt.Printf("  %d of %d %s\n", t, items[i].qty, items[i].name)
		}
	}
	fmt.Printf("Value: %d; weight: %d\n", v, w)
}
