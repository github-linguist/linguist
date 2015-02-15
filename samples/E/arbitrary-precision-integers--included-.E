? def value := 5**(4**(3**2)); null
? def decimal := value.toString(10); null
? decimal(0, 20)
# value: "62060698786608744707"

? decimal(decimal.size() - 20)
# value: "92256259918212890625"

? decimal.size()
# value: 183231
