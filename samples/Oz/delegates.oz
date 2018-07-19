declare
  class Delegator from BaseObject
     attr
	delegate:unit

     meth set(DG)
	{Object.is DG} = true %% assert: DG must be an object
	delegate := DG
     end

     meth operation($)
	if @delegate == unit then
	   {self default($)}
	else
	   try
	      {@delegate thing($)}
	   catch error(object(lookup ...) ...) then
	      %% the delegate did not understand the message
	      {self default($)}
	   end
	end
     end

     meth default($)
	"default implementation"
     end
  end

  class Delegate from BaseObject
     meth thing($)
	"delegate Implementation"
     end
  end

  A = {New Delegator noop}
in
  {System.showInfo {A operation($)}}

  {A set({New BaseObject noop})}
  {System.showInfo {A operation($)}}

  {A set({New Delegate noop})}
  {System.showInfo {A operation($)}}
