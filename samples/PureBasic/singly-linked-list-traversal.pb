Procedure traverse(*node.MyData)
  While *node
    ;access data, i.e. PrintN(Str(*node\Value))
    *node = *node\next
  Wend
EndProcedure

;called using
traverse(*firstnode.MyData)
