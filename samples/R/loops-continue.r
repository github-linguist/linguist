for(i in 1:10)
{
   cat(i)
   if(i %% 5 == 0)
   {
      cat("\n")
      next
   }
   cat(", ")
}
