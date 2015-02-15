<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>stem and leaf plot</title>
<script type='text/javascript'>

    function has_property(obj, propname) {
        return typeof(obj[propname]) === "undefined" ? false : true;
    }

    function compare_numbers(a, b) {return a-b;}

    function stemplot(data, target) {
        var stem_data = {};
        var all_stems = [];
        for (var i = 0; i < data.length; i++) {
            var stem = Math.floor(data[i] / 10);
            var leaf = Math.round(data[i] % 10);
            if (has_property(stem_data, stem)) {
                stem_data[stem].push(leaf);
            } else {
                stem_data[stem] = [leaf];
                all_stems.push(stem);
            }
        }
        all_stems.sort(compare_numbers);

        var min_stem = all_stems[0];
        var max_stem = all_stems[all_stems.length - 1];

        var table = document.createElement('table');
        for (var stem = min_stem; stem <= max_stem; stem++) {
            var row = document.createElement('tr');
            var label = document.createElement('th');
            row.appendChild(label);
            label.appendChild(document.createTextNode(stem));
            if (has_property(stem_data, stem)) {
                stem_data[stem].sort(compare_numbers);
                for (var i = 0; i < stem_data[stem].length; i++) {
                    var cell = document.createElement('td');
                    cell.appendChild(document.createTextNode(stem_data[stem][i]));
                    row.appendChild(cell);
                }
            }
            table.appendChild(row);
        }
        target.appendChild(table);
    }

</script>
<style type='text/css'>
    body {font-family: monospace;}
    table {border-collapse: collapse;}
    th {border-right: 1px solid black; text-align: right;}
    td {text-align: right;}
</style>
</head>
<body>

<div id="target"></div>

<script type='text/javascript'>

    var data = [
        12,127,28,42,39,113,42,18,44,118,44,37,113,124,37,48,127,36,29,31,125,139,131,
        115,105,132,104,123,35,113,122,42,117,119,58,109,23,105,63,27,44,105,99,41,128,
        121,116,125,32,61,37,127,29,113,121,58,114,126,53,114,96,25,109,7,31,141,46,13,
        27,43,117,116,27,7,68,40,31,115,124,42,128,52,71,118,117,38,27,106,33,117,116,
        111,40,119,47,105,57,122,109,124,115,43,120,43,27,27,18,28,48,125,107,114,34,
        133,45,120,30,127,31,116,146
    ];
    stemplot(data, document.getElementById('target'));

</script>

</body>
</html>
