<body>
<canvas id='c'></canvas>

<script>
var canvas = document.getElementById('c');
var ctx = canvas.getContext('2d');

var w = canvas.width = 320;
var h = canvas.height = 240;
var t1 = new Date().getTime();
var frame_count = 0;
ctx.font = 'normal 400 24px/2 Unknown Font, sans-serif';
var img = ctx.createImageData(w, h);

var index_init = 0;
for (var x = 0; x < w; x++) {
    for (var y = 0; y < h; y++) {
        img.data[index_init + 3] = 255; // alpha
        index_init += 4;
    }
}

function animate() {
    var index = 0;
    for (var x = 0; x < w; x++) {
        for (var y = 0; y < h; y++) {
            var value = (Math.random() > 0.5) ? 255 : 0;
            img.data[index    ] = value;
            img.data[index + 1] = value;
            img.data[index + 2] = value;
            // alpha channel is constant
            index += 4;
        }
    }

    ctx.putImageData(img, 0, 0);

    frame_count++;
    if (frame_count % 50 == 0) {
        var fps = frame_count / (new Date().getTime() - t1) * 1000;
        window.status = fps.toFixed(2) + " fps";
    }

    setTimeout(animate, 0);
}

animate();
</script>
</body>
