// Stack data structure for Krypton
// Stack is represented as a comma-separated string with count

module stack


// Create new empty stack
func stackNew() {
    emit ""
}

// Push a value onto the stack
func stackPush(stk, val) {
    if stk == "" {
        emit val
    }
    emit stk + "," + val
}

// Peek at the top value without removing
func stackPeek(stk) {
    let size = count(stk)
    emit split(stk, size - 1)
}

// Pop the top value, returns pair of "value,newStack"
func stackPop(stk) {
    let size = count(stk)
    if size <= 1 {
        emit stk + ","
    }
    // Get the top value
    let top = split(stk, size - 1)
    // Rebuild without last
    let sb = sbNew()
    let i = 0
    while i < size - 1 {
        if i > 0 {
            sb = sbAppend(sb, ",")
        }
        sb = sbAppend(sb, split(stk, i))
        i += 1
    }
    emit top + "|" + sbToString(sb)
}

// Get stack size
func stackSize(stk) {
    if stk == "" {
        emit 0
    }
    emit count(stk)
}

// Check if stack is empty
func stackEmpty(stk) {
    if stk == "" {
        emit 1
    }
    emit 0
}

// Print all stack elements (top to bottom)
func stackPrint(stk) {
    let size = count(stk)
    kp("--- Stack (top to bottom) ---")
    let i = size - 1
    while i >= 0 {
        kp("  " + split(stk, i))
        i -= 1
    }
    kp("-----------------------------")
}

// Clear the stack
func stackClear() {
    emit ""
}

// Check if value exists in stack
func stackContains(stk, val) {
    let size = count(stk)
    let i = 0
    while i < size {
        if split(stk, i) == val {
            emit 1
        }
        i += 1
    }
    emit 0
}
