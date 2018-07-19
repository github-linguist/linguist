<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>12 times table</title>
<script type='text/javascript'>

    function multiplication_table(n, target) {
        var table = document.createElement('table');

        var row = document.createElement('tr');
        var cell = document.createElement('th');
        cell.appendChild(document.createTextNode('x'));
        row.appendChild(cell);
        for (var x = 1; x <=n; x++) {
            cell = document.createElement('th');
            cell.appendChild(document.createTextNode(x));
            row.appendChild(cell);
        }
        table.appendChild(row);

        for (var x = 1; x <=n; x++) {
            row = document.createElement('tr');
            cell = document.createElement('th');
            cell.appendChild(document.createTextNode(x));
            row.appendChild(cell);
            var y;
            for (y = 1; y < x; y++) {
                cell = document.createElement('td');
                cell.appendChild(document.createTextNode('\u00a0'));
                row.appendChild(cell);
            }
            for (; y <= n; y++) {
                cell = document.createElement('td');
                cell.appendChild(document.createTextNode(x*y));
                row.appendChild(cell);
            }
            table.appendChild(row);
        }
        target.appendChild(table);
    }

</script>
<style type='text/css'>
    body {font-family: sans-serif;}
    table {border-collapse: collapse;}
    th, td {border: 1px solid black; text-align: right; width: 4ex;}
</style>
</head>
<body onload="multiplication_table(12, document.getElementById('target'));">
<div id='target'></div>
</body>
</html>
