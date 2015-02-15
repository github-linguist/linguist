use Random;

var nums:[1..10, 1..10] int;
var rnd = new RandomStream();

[ n in nums ] n = floor(rnd.getNext() * 21):int;
delete rnd;

// this shows a clumsy explicit way of iterating, to actually create nested loops:
label outer for i in nums.domain.dim(1) {
        for j in nums.domain.dim(2) {
                write(" ", nums(i,j));
                if nums(i,j) == 20 then break outer;
        }
        writeln();
}
