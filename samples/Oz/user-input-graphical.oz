functor
import
   Application
   QTk at 'x-oz://system/wp/QTk.ozf'
   System
define
   Number NumberWidget
   Text
   StatusLabel
   WindowClosed
   GUI = td(action:OnClose
	    return:WindowClosed
	    lr(label(text:"Enter some text:" width:20)
	       entry(return:Text glue:ew)
	      glue:ew)
	    lr(label(text:"Enter a number:" width:20)
	       numberentry(max:100000 return:Number handle:NumberWidget)
	       label(handle:StatusLabel width:20)
	       glue:ew
	      )
	    button(text:"Ok" glue:ew
		   action:OnClose
		  )
	   )
   proc {OnClose}
      if {NumberWidget get($)} \= 75000 then
	 {StatusLabel set(text:"Invalid value")}
      else
	 {Window close}
      end
   end
   Window = {QTk.build GUI}
   {Window show}
   {Wait WindowClosed}
   {System.showInfo "You entered; "#Text#", "#Number}
   {Application.exit 0}
end
