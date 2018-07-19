functor
import
   Application
   QTk at 'x-oz://system/wp/QTk.ozf'
define
   Count = {NewCell 0}
   Label
   GUI = td(action:proc {$} {Application.exit 0} end %% exit on close
	    label(text:"There have been no clicks yet." handle:Label)
	    button(text:"Click Me"
		   action:proc {$}
			     Count := @Count + 1
			     {Label set(text:"Number of clicks: "#@Count#".")}
			  end
		  ))
   Window = {QTk.build GUI}
   {Window show}
end
