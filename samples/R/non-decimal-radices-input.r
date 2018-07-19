# parse a string to decimal
as.numeric("20")    # 20
# parse a hex-string to decimal
as.numeric("0x20")  # 32
# parse a string to hexadecimal
as.hexmode(as.numeric("32")) # "20"
# parse a string to octal
as.octmode(as.numeric("20")) # "24"
