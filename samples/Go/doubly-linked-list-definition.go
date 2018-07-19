type dlNode struct {
    int
    next, prev *dlNode
}

// Field 'members' allows loops to be prevented.  All nodes
// inserted should be added to members.  Code that operates
// on the list can check any pointer against members to
// find out if the pointer is already in the list.
type dlList struct {
    members map[*dlNode]int
    head, tail **dlNode
}
