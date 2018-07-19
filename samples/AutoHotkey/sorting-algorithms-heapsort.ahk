heapSort(a) {
    Local end
    end := %a%0
    heapify(a,end)
    While end > 1
        %a%%end% := (%a%1 "", %a%1 := %a%%end%)
       ,siftDown(a, 1, --end)
}

heapify(a, count) {
    Local start
    start := count // 2
    While start
       siftDown(a, start--, count)
}

siftDown(a, start, end) {
    Local child, c1
    While start*2 <= end {
        c1 := 1 + child := start*2
        If (c1 <= end && %a%%child% < %a%%c1%)
            child := c1
        If (%a%%start% < %a%%child%)
            %a%%start% := (%a%%child% "", %a%%child% := %a%%start%)
           ,start := child
        Else Return
    }
}

a = 1,5,2,7,3,4,6,8,1 ; ----- test -----
StringSplit a, a, `,
heapSort("a")
ListVars
MsgBox
