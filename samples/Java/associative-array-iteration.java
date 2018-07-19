Map<String, Integer> myDict = new HashMap<String, Integer>();
myDict.put("hello", 1);
myDict.put("world", 2);
myDict.put("!", 3);

// iterating over key-value pairs:
for (Map.Entry<String, Integer> e : myDict.entrySet()) {
    String key = e.getKey();
    Integer value = e.getValue();
    System.out.println("key = " + key + ", value = " + value);
}

// iterating over keys:
for (String key : myDict.keySet()) {
    System.out.println("key = " + key);
}

// iterating over values:
for (Integer value : myDict.values()) {
    System.out.println("value = " + value);
}
