import strutils

var s: string = "alphaBETA_123"
echo s," as upper case: ", toUpper(s)
echo s," as lower case: ", toLower(s)
echo s," as Capitalized: ", capitalize(s)
echo s," as normal case: ", normalize(s)  # remove underscores, toLower
