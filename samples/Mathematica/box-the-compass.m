Map[List[Part[#,1], dirs[[Part[#,1]]], ToString@Part[#,2]<>"°"]&,
  Map[{Floor[Mod[ #+5.625 , 360]/11.25]+1,#}&,input] ]//TableForm
