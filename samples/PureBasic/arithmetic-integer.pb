OpenConsole()

Define a, b

Print("Number 1: "): a = Val(Input())
Print("Number 2: "): b = Val(Input())

PrintN("Sum:        " + Str(a + b))
PrintN("Difference: " + Str(a - b))
PrintN("Product:    " + Str(a * b))
PrintN("Quotient:   " + Str(a / b)) ; Integer division (rounding mode=truncate)
PrintN("Remainder: " + Str(a % b))
PrintN("Power:      " + Str(Pow(a, b)))

Input()

CloseConsole()
