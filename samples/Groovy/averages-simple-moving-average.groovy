def simple_moving_average = { size ->
    def nums = []
    double total = 0.0
    return { newElement ->
        nums += newElement
        oldestElement = nums.size() > size ? nums.remove(0) : 0
        total += newElement - oldestElement
        total / nums.size()
    }
}

ma5 = simple_moving_average(5)

(1..5).each{ printf( "%1.1f ", ma5(it)) }
(5..1).each{ printf( "%1.1f ", ma5(it)) }
