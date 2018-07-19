def str = 'abcdefgh'
def n = 2
def m = 3
println str[n..n+m-1]
println str[n..-1]
println str[0..-2]
def index1 = str.indexOf('d')
println str[index1..index1+m-1]
def index2 = str.indexOf('de')
println str[index2..index2+m-1]
