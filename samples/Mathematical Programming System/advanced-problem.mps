*This is a less trivial MPS problem to test the MPS reader
name          simple
rows
 N  obj
 L  row1
 L  row2
 G  row3
columns
    w         obj       -1              row3      1
    X         obj       1               row1      3
    X         row3      2
    Y         obj       4.5             row1      1
    Y         row2      1
    Z         obj       8               row2      2
    Z         row3      -1
RHS
    testrhs   row1      10              row2      18
    testrhs   row3      6
    rhs1      row1      8               row2      10
    rhs1      row3      -1
bounds
 BV wbin      w
 LO z         Z         0
 UP z         Z         4
 FR freex     X
objsense
    min
ENDATA
