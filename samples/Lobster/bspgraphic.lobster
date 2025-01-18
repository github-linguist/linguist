import vec
import color
import gl

let dirs = [ int2_x, int2_y ]

def square(pos, size, depth) -> void:
    if (rnd(4) or depth < 2) and size.x > 8 and size.y > 8:
        var notfound = true
        var dir = 0
        var split = 0
        while notfound: // FIXME
            dir = 0
            split = rnd(size.x + size.y)
            if split >= size.x:
                split -= size.x
                dir = 1
            if split >= 3 and split <= size[dir] - 3:
                notfound = false
        for(2) i:
            let width = if i: size[dir] - split else: split
            square(pos + dirs[dir] * split * i, dirs[dir] * width + dirs[1 - dir] * size[1 - dir], depth + 1)
    else:
        gl.translate pos:
            gl.color(color_black)
            gl.rect(float(size))
            let range = max(0.1, min(0.7, 1.0 - sqrt(size.x * size.y) / 250.0))
            let col = color { rnd_float(), rnd_float() * range + (1.0 - range), rnd_float(), 1.0 } * 0.15 + 0.75
            gl.color(col)
            gl.rect(float(size - int2_1 * 2))

var seed = 342342432

fatal(gl.window("bsp", 512, 512))

while gl.frame():
    if gl.button("escape") == 1: return
    if gl.button("space") == 1: seed += 675656
    gl.clear(color_black)
    rnd_seed(seed)
    square(int2_0, gl.window_size(), 0)
