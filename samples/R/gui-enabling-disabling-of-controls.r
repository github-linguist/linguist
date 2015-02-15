library(gWidgets)
options(guiToolkit="RGtk2") ## using gWidgtsRGtk2

w <- gwindow("Disable components")

g <- ggroup(cont=w, horizontal=FALSE)
e <- gedit("0", cont=g, coerce.with=as.numeric)
bg <- ggroup(cont=g)

down_btn <- gbutton("-", cont=bg)
up_btn <- gbutton("+", cont=bg)

update_ctrls <- function(h,...) {
  val <- svalue(e)
  enabled(down_btn) <- val >= 0
  enabled(up_btn) <- val <= 10
}

rement <- function(h,...) {
  svalue(e) <- svalue(e) + h$action
  update_ctrls(h,...)
}

addHandlerChanged(e, handler=update_ctrls)
addHandlerChanged(down_btn, handler=rement, action=-1)
addHandlerChanged(up_btn, handler=rement, action=1)
