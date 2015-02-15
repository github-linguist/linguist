declare
  fun {DropUntil Xs Prefix}
     case Xs of nil then nil
     [] _|Xr then
        if {List.isPrefix Prefix Xs} then Xs
        else {DropUntil Xr Prefix}
        end
     end
  end

  Digits = "1234567890"
in
  {ForAll
   [{List.take {List.drop Digits 2} 3}     = "345"
    {List.drop Digits 2}                   = "34567890"
    {List.take Digits {Length Digits}-1}   = "123456789"
    {List.take {DropUntil Digits "4"} 3}   = "456"
    {List.take {DropUntil Digits "56"} 3}  = "567"
    {List.take {DropUntil Digits "31"} 3}  = ""
   ]
   System.showInfo}
