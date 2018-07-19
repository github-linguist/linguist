<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
        "http://www.w3.org/TR/html4/strict.dtd">
<html><head><script type="text/javascript">
window.addEventListener(
    "load", function(){
        var img = new Image();
        // ***********************************************************
        // RUN LOCAL WEBSERVER TO LOAD LOCAL FILES: e.g. python -m http.server (python3)
        // ***********************************************************
        // img.src = prompt("enter image path","http://localhost:8000/test.jpg");
        img.src =
            'data:image/gif;base64,R0lGODlhEAAOALMAAOazToeHh0tLS/7LZv/0jvb29t/f3//Ub/\
            /ge8WSLf/rhf/3kdbW1mxsbP//mf///yH5BAAAAAAALAAAAAAQAA4AAARe8L1Ekyky67QZ1hLnjM5UUde0ECwLJoExKcpp\
            V0aCcGCmTIHEIUEqjgaORCMxIC6e0CcguWw6aFjsVMkkIr7g77ZKPJjPZqIyd7sJAgVGoEGv2xsBxqNgYPj/gAwXEQA7';
        img.onload = function(){
            var can1 = new CustomCanvas("color", img.width, img.height);
            var can2 = new CustomCanvas("grayscale",  img.width, img.height);
            can1.ctx.drawImage(img,0, 0, img.width, img.height);
            var imgData = can1.ctx.getImageData(0, 0, can1.w, can1.h);
            // desaturate
            var avg; var max; var rwgt=0.2126; var gwgt=0.7152;  var bwgt=0.0722;
            for(var i = 0, max = can1.w*can1.h*4; i < max; i=i+4){
                avg = imgData.data[i]*rwgt + imgData.data[i+1]*gwgt + imgData.data[i+2]*bwgt;
                imgData.data[i  ] = avg;  // red
                imgData.data[i+1] = avg;  // green
                imgData.data[i+2] = avg;} // blue, alpha=alpha
            can2.ctx.putImageData(imgData, 0, 0);
            }
        }, false);

    function CustomCanvas(id, w, h, s) { /* Custom Canvas Object */
        var c = document.createElement("canvas");
        c.setAttribute('id', id); c.setAttribute('width', w);
        c.setAttribute('height', h); (s)?c.setAttribute('style', s):0;
        document.body.appendChild(c, document.body.firstChild);
        this.ctx = document.getElementById(id).getContext("2d");
        this.w = w; this.h = h;}
</script></head><body></body></html>
