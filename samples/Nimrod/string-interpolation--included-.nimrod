import strutils

var str = "little"
echo "Mary had a $# lamb" % [str]
# doesn't need an array for one substitution, but use an array for multiple substitutions
