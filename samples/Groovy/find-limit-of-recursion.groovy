def recurse;
recurse = {
    try {
        recurse (it + 1)
    } catch (StackOverflowError e) {
        return it
    }
}

recurse(0)
