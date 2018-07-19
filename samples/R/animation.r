rotate_string <- function(x, forwards)
{
   nchx <- nchar(x)
   if(forwards)
   {                                  
      paste(substr(x, nchx, nchx), substr(x, 1, nchx - 1), sep = "")
   } else
   {                                                          
      paste(substr(x, 2, nchx), substr(x, 1, 1), sep = "")
   }
}

handle_rotate_label <- function(obj, interval = 100)
{
  addHandlerIdle(obj,
    handler = function(h, ...)
    {
       svalue(obj) <- rotate_string(svalue(obj), tag(obj, "forwards"))
    },
    interval = interval
  )
}

handle_change_direction_on_click <- function(obj)
{
  addHandlerClicked(obj,
    handler = function(h, ...)
    {
      tag(h$obj, "forwards") <- !tag(h$obj, "forwards")
    }
  )
}

library(gWidgets)
library(gWidgetstcltk) #or library(gWidgetsRGtk2) or library(gWidgetsrJava)             
lab <- glabel("Hello World! ", container = gwindow())  
tag(lab, "forwards") <- TRUE
handle_rotate_label(lab)
handle_change_direction_on_click(lab)
