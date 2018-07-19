sleep <- function(time=1)
{
   message("Sleeping...")
   flush.console()
   Sys.sleep(time)
   message("Awake!")
}

sleep()
