// rename file in current directory
f := File with("input.txt")
f moveTo("output.txt")

// rename file in root directory
f := File with("/input.txt")
f moveTo("/output.txt")

// rename directory in current directory
d := Directory with("docs")
d moveTo("mydocs")

// rename directory in root directory
d := Directory with("/docs")
d moveTo("/mydocs")
