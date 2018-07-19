showmenu <- function()
{
   choices <- c("fee fie", "huff and puff", "mirror mirror", "tick tock")
   ans <- menu(choices)
   if(ans==0) "" else choices[ans]
}
str <- showmenu()
