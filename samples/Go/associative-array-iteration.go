myMap := map[string]int {
	   "hello": 13,
	   "world": 31,
	   "!"    : 71 }

// iterating over key-value pairs:
for key, value := range myMap {
    fmt.Printf("key = %s, value = %d\n", key, value)
}

// iterating over keys:
for key := range myMap {
    fmt.Printf("key = %s\n", key)
}

// iterating over values:
for _, value := range myMap {
    fmt.Printf("value = %d\n", value)
}
