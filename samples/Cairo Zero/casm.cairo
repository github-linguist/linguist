jmp rel 4 if [fp + -3] != 0;
[ap + 0] = [fp + -5], ap++;
ret;
[ap + 0] = [fp + -4], ap++;
[ap + 0] = [fp + -5] + [fp + -4], ap++;
[fp + -3] = [ap + 0] + 1, ap++;
call rel -8;
ret;
