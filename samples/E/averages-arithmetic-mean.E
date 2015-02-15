def meanOrZero(numbers) {
    var count := 0
    var sum := 0
    for x in numbers {
        sum += x
        count += 1
    }
    return sum / 1.max(count)
}
