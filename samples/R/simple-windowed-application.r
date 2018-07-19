library(gWidgets)
library(gWidgetstcltk)
win <- gwindow()
lab <- glabel("There have been no clicks yet", container=win)
btn <- gbutton("click me", container=win, handle=function(h, ...)
   {
      val <- as.numeric(svalue(lab))
      svalue(lab) <- ifelse(is.na(val) ,"1", as.character(val + 1))
   }
)
