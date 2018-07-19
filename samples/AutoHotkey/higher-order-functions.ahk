f(x) {
return x
}
g(x, y) {
msgbox %x%
msgbox %y%
}
g(f("RC Function as an Argument AHK implementation"), "Non-function argument")
return
