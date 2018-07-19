package main
import "fmt"

// Compress a string to a list of output symbols.
func compress(uncompressed string) []int {
    // Build the dictionary.
    dictSize := 256
    dictionary := make(map[string]int)
    for i := 0; i < 256; i++ {
        dictionary[string(i)] = i
    }

    w := ""
    result := make([]int, 0)
    for _, c := range []byte(uncompressed) {
        wc := w + string(c)
        if _, ok := dictionary[wc]; ok {
            w = wc
        } else {
            result = append(result, dictionary[w])
            // Add wc to the dictionary.
            dictionary[wc] = dictSize
            dictSize++
            w = string(c)
        }
    }

    // Output the code for w.
    if w != "" {
        result = append(result, dictionary[w])
    }
    return result
}

// Decompress a list of output ks to a string.
func decompress(compressed []int) string {
    // Build the dictionary.
    dictSize := 256
    dictionary := make(map[int]string)
    for i := 0; i < 256; i++ {
        dictionary[i] = string(i)
    }

    w := string(compressed[0])
    result := w
    for _, k := range compressed[1:] {
        var entry string
        if x, ok := dictionary[k]; ok {
            entry = x
        } else if k == dictSize {
            entry = w + w[:1]
        } else {
            panic(fmt.Sprintf("Bad compressed k: %d", k))
        }

        result += entry

        // Add w+entry[0] to the dictionary.
        dictionary[dictSize] = w + entry[:1]
        dictSize++

        w = entry
    }
    return result
}

func main() {
    compressed := compress("TOBEORNOTTOBEORTOBEORNOT")
    fmt.Println(compressed)
    decompressed := decompress(compressed)
    fmt.Println(decompressed)
}
