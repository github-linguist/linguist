Strict

' single line comment

#rem
multi
line
comment
#end

#rem
nested
#rem
multi
line
#end
comment
#end

Import mojo

Const ONECONST:Int = 1
Const TWOCONST := 2
Const THREECONST := 3, FOURCONST:Int = 4

Global someVariable:Int = 4

' sample class from the documentation
Class Game Extends App

    Function New()
    End

    Function DrawSpiral(clock)
        Local w=DeviceWidth/2
        For Local i#=0 Until w*1.5 Step .2
            Local x#,y#
            x=w+i*Sin(i*3+clock)
            y=w+i*Cos(i*2+clock)
            DrawRect  x,y,1,1
        Next
        hitbox.Collide(event.pos)
    End

    Field updateCount

    Method OnCreate()
        Print "spiral"

        SetUpdateRate 60
    End

    Method OnUpdate()
        updateCount+=1
    End

    Method OnRender()
        Cls
        DrawSpiral updateCount
        DrawSpiral updateCount*1.1
    End

End

Class Enemy
  Method Die () Abstract
End

' extending
Class Hoodlum Extends Enemy
    ' field
    Field testField:Bool = True

    ' naming class with modulepath
    Local currentNode:list.Node<Vector2D>

    Method Die ()
        Print "B'oss, he-- he killed me, b'oss!"
    End
End

' extending with generics
Class VectorNode Extends Node<Vector2D>
End

' interfaces
Interface Computer
  Method Boot ()
  Method Process ()
  Method Display ()
End

Class PC Implements Computer
End

' array syntax
Global listOfStuff:String[42]
Global lessStuff:String[5] = listOfStuff[4..8]
Global oneStuff:String = listOfStuff[23]

'a comma separated sequence
Global scores:Int[]=[10,20,30]
'a comma separated sequence
Global text:String[]=["Hello","There","World"]
Global worstCase:worst.List<String[]>

' string type
Global string1:String = "Hello world"
Global string2$ = "Hello world"

' escape characers in strings
Global string3 := "Hello~zWorld"
Global string4 := "~qHello World~q"
Global string5 := "~tIndented~n"
Global string6 := "tilda is wavey... ~~"

' string pseudofunctions
Print "  Hello World  ~n".Trim()    ' prints "Hello World"
Print "Hello World".ToUpper()       ' prints "HELLO WORLD"

' Boolean shorttype
Global boolVariable1:Bool = True
Global boolVariable2? = False

' number formats
Global hexNum1:Int = $3d0dead
Global hexNum2% = $CAFEBABE

Global floatNum1:Float = 3.141516
Global floatNum2# = 3.141516
Global floatNum3 := .141516

' preprocessor keywords
#If TARGET = "android"
DoStuff()
#ElseIf TARGET = "ios"
DoOtherStuff()
#End

' preprocessor variable
#SOMETHING = True
#Print SOMETHING
#If SOMETHING
#End

' operators
Global a = 32
Global b = 32 ~ 0
b ~= 16
b |= 16
b &= 16
Global c = a | b
