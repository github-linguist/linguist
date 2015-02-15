fn binary_search(haystack: ~[int], needle: int) -> int {
    let mut low  = 0;
    let mut high = haystack.len() as int - 1;

    if high == 0 { return -1 }

    while low <= high {
        // avoid overflow
        let mid = low + (high - low) / 2;

        if      haystack[mid] > needle { high = mid - 1 }
        else if haystack[mid] < needle { low  = mid + 1 }
        else                           { return mid     }
    }

    return -1;
}
