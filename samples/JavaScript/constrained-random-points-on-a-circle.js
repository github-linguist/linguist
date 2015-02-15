<html><head><title>Circle</title></head>
<body>
<canvas id="cv" width="320" height="320"></canvas>
<script type="application/javascript">

var cv = document.getElementById('cv');
var ctx = cv.getContext('2d');

var w = cv.width;
var h = cv.height;

//draw circles
ctx.fillStyle = 'rgba(0, 255, 200, .3)';
ctx.strokeStyle = 'rgba(0,0,0,.1)';
ctx.beginPath();
ctx.arc(w/2, h/2, 150, 0, Math.PI*2, true);
ctx.arc(w/2, h/2, 100, 0, Math.PI*2, false);
ctx.closePath();
ctx.fill();

// draw grids
ctx.beginPath();
for (var i = 10; i < w; i += 10) {
	ctx.moveTo(i, 0);
	ctx.lineTo(i, h);
	ctx.moveTo(0, i);
	ctx.lineTo(w, i);
}
ctx.closePath();
ctx.stroke();

//draw points
ctx.fillStyle = 'navy';
var pts = 0;
while (pts < 100) {
	var x = Math.floor(Math.random() * 31) - 15;
	var y = Math.floor(Math.random() * 31) - 15;
	var r = x * x + y * y;
	if (r < 100 || r > 225) continue;
	x = x * 10 + w/2;
	y = y * 10 + h/2;
	ctx.fillRect(x - 2, y - 2, 4, 4);
	pts++;
}

</script></body></html>
