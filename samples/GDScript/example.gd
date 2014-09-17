# Taken from https://github.com/okamstudio/godot/wiki/gdscript
# a file is a class!

# inheritance

extends BaseClass

# member variables

var a = 5 
var s = "Hello"
var arr = [1, 2, 3]
var dict = {"key":"value", 2:3}

# constants

const answer = 42
const thename = "Charly"

# built-in vector types

var v2 = Vector2(1, 2)
var v3 = Vector3(1, 2, 3)

# function

func some_function(param1, param2):
    var local_var = 5

    if param1 < local_var:
        print(param1)
    elif param2 > 5:
        print(param2)
    else:
        print("fail!")

    for i in range(20):
        print(i)

    while(param2 != 0):
        param2 -= 1

    var local_var2 = param1+3
    return local_var2


# subclass

class Something:
    var a = 10

# constructor

func _init():
    print("constructed!")
    var lv = Something.new()
    print(lv.a)
