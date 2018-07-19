declare
  class BaseQueue
     attr
        contents:nil

     meth init
        raise notImplemented(self init) end
     end

     meth enqueue(Item)
        raise notImplemented(self enqueue) end
     end

     meth dequeue(?Item)
        raise notImplemented(self dequeue) end
     end

     meth printContents
        {ForAll @contents Show}
     end
  end

  Queue = {New BaseQueue init} %% throws
