function move($n,$from,$to,$via) {
    if ($n === 1) {
        print("Move disk from pole $from to pole $to");
    } else {
        move($n-1,$from,$via,$to);
        move(1,$from,$to,$via);
        move($n-1,$via,$to,$from);
    }
}
