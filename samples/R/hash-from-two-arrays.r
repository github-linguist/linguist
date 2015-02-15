# Set up hash table
keys <- c("John Smith", "Lisa Smith", "Sam Doe", "Sandra Dee", "Ted Baker")
values <- c(152, 1, 254, 152, 153)
names(values) <- keys
# Get value corresponding to a key
values["Sam Doe"]                          # vals["Sam Doe"]
# Get all keys corresponding to a value
names(values)[values==152]                 # "John Smith" "Sandra Dee"
