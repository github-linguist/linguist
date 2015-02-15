def btree := [1, [2, [4, [7, null, null],
                         null],
                     [5, null, null]],
                 [3, [6, [8, null, null],
                         [9, null, null]],
                     null]]

def backtrackingOrder(node, pre, mid, post) {
    switch (node) {
        match ==null {}
        match [value, left, right] {
            pre(value)
            backtrackingOrder(left, pre, mid, post)
            mid(value)
            backtrackingOrder(right, pre, mid, post)
            post(value)
        }
    }
}

def levelOrder(root, func) {
    var level := [root].diverge()
    while (level.size() > 0) {
        for node in level.removeRun(0) {
            switch (node) {
                match ==null {}
                match [value, left, right] {
                    func(value)
                    level.push(left)
                    level.push(right)
}   }   }   }   }

print("preorder:   ")
backtrackingOrder(btree, fn v { print(" ", v) }, fn _ {}, fn _ {})
println()

print("inorder:    ")
backtrackingOrder(btree, fn _ {}, fn v { print(" ", v) }, fn _ {})
println()

print("postorder:  ")
backtrackingOrder(btree, fn _ {}, fn _ {}, fn v { print(" ", v) })
println()

print("level-order:")
levelOrder(btree, fn v { print(" ", v) })
println()
