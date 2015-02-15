<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
<title>Sierpinski Carpet</title>
<script type='text/javascript'>

var black_char = "#";
var white_char = " ";

var SierpinskiCarpet = function(size) {
    this.carpet = [black_char];
    for (var i = 1; i <= size; i++) {
        this.carpet = [].concat(
            this.carpet.map(this.sier_top),
            this.carpet.map(this.sier_middle),
            this.carpet.map(this.sier_top)
        );
    }
}

SierpinskiCarpet.prototype.sier_top = function(x) {
    var str = new String(x);
    return new String(str+str+str);
}

SierpinskiCarpet.prototype.sier_middle = function (x) {
    var str = new String(x);
    var spacer = str.replace(new RegExp(black_char, 'g'), white_char);
    return new String(str+spacer+str);
}

SierpinskiCarpet.prototype.to_string = function() {
    return this.carpet.join("\n")
}

SierpinskiCarpet.prototype.to_html = function(target) {
    var table = document.createElement('table');
    for (var i = 0; i < this.carpet.length; i++) {
        var row = document.createElement('tr');
        for (var j = 0; j < this.carpet[i].length; j++) {
            var cell = document.createElement('td');
            cell.setAttribute('class', this.carpet[i][j] == black_char ? 'black' : 'white');
            cell.appendChild(document.createTextNode('\u00a0'));
            row.appendChild(cell);
        }
        table.appendChild(row);
    }
    target.appendChild(table);
}

</script>
<style type='text/css'>
    table {border-collapse: collapse;}
    td {width: 1em;}
    .black {background-color: black;}
    .white {background-color: white;}
</style>
</head>
<body>

<pre id='to_string' style='float:left; margin-right:0.25in'></pre>
<div id='to_html'></div>

<script type='text/javascript'>
    var sc = new SierpinskiCarpet(3);
    document.getElementById('to_string').appendChild(document.createTextNode(sc.to_string()));
    sc.to_html(document.getElementById('to_html'));
</script>

</body>
</html>
