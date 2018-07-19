function loop_plus_half(start, end) {
    var str = '',
        i;
    for (i = start; i <= end; i += 1) {
        str += i;
        if (i === end) {
          break;
        }
        str += ', ';
    }
    return str;
}

alert(loop_plus_half(1, 10));
