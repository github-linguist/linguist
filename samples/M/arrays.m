start ; create student data
  set student("name","first")="Linus"
  set student("name","last")="Torvalds"
  set student("language")="C"
  set student("classes","monday")="Algebra"
  set student("classes","tuesday")="Geometry"
  set student("classes","wednesday")="English"
  set student("classes","thursday")="French"
  set student("classes","friday")="Jujitsu"
  zwrite student
  write $order(student)

  write !,"Student array top level",!
  set x=""
  for  do  quit:x=""
  . set x=$order(student(x))
  . write x,!

  write !,"Student classes ",!
  set x=""
  for  do  quit:x=""
  . set x=$order(student("classes",x))
  . write:x'="" x," : ",student("classes",x),!

  quit
