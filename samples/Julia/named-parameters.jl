function surround(string ; border = :default, padding = 0)

 ve, ho, ul, ur, dl, dr =
   border == :round ? ("\u2502","\u2500","\u256d","\u256e","\u2570","\u256f") :
   border == :bold  ? ("\u2503","\u2501","\u250F","\u2513","\u2517","\u251b") :
   border == :double? ("\u2551","\u2550","\u2554","\u2557","\u255a","\u255d") :
   border == :dotted? ("\u254e","\u254c","\u250c","\u2510","\u2514","\u2518") :
   border == :cross ? ("\u2502","\u2500","\u253c","\u253c","\u253c","\u253c") :
                      ("\u2502","\u2500","\u250c","\u2510","\u2514","\u2518")

 println(ul, ho^(length(string) + 2padding),  ur, "\n",
         ve, " "^padding, string," "^padding, ve, "\n",
         dl, ho^(length(string) + 2padding),  dr)
end
