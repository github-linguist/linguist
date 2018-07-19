library(gWidgets)
options(guiToolkit="RGtk2") ## using gWidgtsRGtk2

w <- gwindow("Interaction")

g <- ggroup(cont=w, horizontal=FALSE)
e <- gedit(0, cont=g, coerce.with=as.numeric)
bg <- ggroup(cont=g)

inc_btn <- gbutton("increment", cont=bg)
rdm_btn <- gbutton("random", cont=bg)

addHandlerChanged(e, handler=function(h,...) {
  val <- svalue(e)
  if(is.na(val))
    galert("You need to enter a number", parent=w)
})

addHandlerChanged(inc_btn, handler=function(h,...) {
  val <- svalue(e)
  if(is.na(val))
    galert("Can't increment if not a number", parent=w)
  else
    svalue(e) <- val + 1
})

addHandlerChanged(rdm_btn, handler=function(h,...) {
  if(gconfirm("Really replace value?"))
    svalue(e) <- sample(1:1000, 1)
})
