declare
  class Example
     meth init skip end

     meth foo {System.showInfo foo} end

     meth bar {System.showInfo bar} end

     meth otherwise(Msg)
        {System.showInfo "Unknown method "#{Label Msg}}
        if {Width Msg} > 0 then
           {System.printInfo "Arguments: "}
           {System.show {Record.toListInd Msg}}
        end
     end
  end

  Object = {New Example init}
 in
  {Object foo}
  {Object bar}
  {Object grill}
  {Object ding(dong)}
