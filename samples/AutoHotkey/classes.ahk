obj := new MyClass
obj.WhenCreated()

class MyClass {
; Instance Variable #1
   time := A_Hour ":" A_Min ":" A_Sec

; Constructor
   __New() {
      MsgBox, % "Constructing new object of type: " this.__Class
      FormatTime, date, , MM/dd/yyyy
   ; Instance Variable #2
      this.date := date
   }
; Method
   WhenCreated() {
      MsgBox, % "Object created at " this.time " on " this.date
   }
}
