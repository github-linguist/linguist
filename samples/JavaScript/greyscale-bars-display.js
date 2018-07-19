<html><body>
<script type="text/javascript">
var width = 640; var height = 400;
var c = document.createElement("canvas");
c.setAttribute('id',    'myCanvas');
c.setAttribute('style', 'border:1px solid black;');
c.setAttribute('width',  width);
c.setAttribute('height', height);
document.body.appendChild(c);
var ctx = document.getElementById('myCanvas').getContext("2d");

var columnCount = 8;    // number of columns
var rowCount    = 4;    // number of rows
var direction   = 1;    // 1 = from left to right, 0 = from right to left
var blackLeft   = 1;    // black is left: 1 = true, 0 = false
for(var j = 0; j < rowCount; j++){
    for(var i = 0; i < columnCount; i++){
        ctx.fillStyle = 'rgba(0,0,0,'+ (blackLeft-(1/(columnCount-1)*i))*direction +')';
        ctx.fillRect(
            (width/columnCount)*i,(height/rowCount)*j,
            (width/columnCount),(height/rowCount)
            );
        }
    columnCount *= 2;
    direction *= -1;
    blackLeft = blackLeft ? 0 : 1;
    }
</script>
</body></html>
