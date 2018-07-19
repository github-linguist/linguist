library(gWidgets)
options(guiToolkit="RGtk2") ## using gWidgtsRGtk2

w <- gwindow("Enter a string and a number")
lyt <- glayout(cont=w)
lyt[1,1] <- "Enter a string"
lyt[1,2] <- gedit("", cont=lyt)
lyt[2,1] <- "Enter 75000"
lyt[2,2] <- gedit("", cont=lyt)
lyt[3,2] <- gbutton("validate", cont=lyt, handler=function(h,...) {
  txt <- svalue(lyt[1,2])
  x <- svalue(lyt[2,2])
  x <- gsub(",", "", x)
  x <- as.integer(x)

  if(nchar(txt) > 0 && x == 75000)
    gmessage("Congratulations, you followed directions", parent=w)
  else
    gmessage("You failed this simple task", parent=w)
})
