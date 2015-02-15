MsgBox % compose("sin","cos",1.5)

compose(f,g,x) { ; function composition
   Return %f%(%g%(x))
}
