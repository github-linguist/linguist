function createRow(i, point, heading) {
    var tr = document.createElement('tr'),
        td;

    td = document.createElement('td');
    td.appendChild(document.createTextNode(i));
    tr.appendChild(td);

    td = document.createElement('td');
    point = point.substr(0, 1).toUpperCase() + point.substr(1);
    td.appendChild(document.createTextNode(point));
    tr.appendChild(td);

    td = document.createElement('td');
    td.appendChild(document.createTextNode(heading));
    tr.appendChild(td);

    return tr;
}

function getPoint(i) {
    var j = i % 8,
        i = Math.floor(i / 8) % 4,
        cardinal = ['north', 'east', 'south', 'west'],
        pointDesc = ['1', '1 by 2', '1-C', 'C by 1', 'C', 'C by 2', '2-C', '2 by 1'],
        str1, str2, strC;

    str1 = cardinal[i];
    str2 = cardinal[(i + 1) % 4];
    strC = (str1 === 'north' || str1 === 'south') ? str1 + str2 : str2 + str1;
    return pointDesc[j].replace('1', str1).replace('2', str2).replace('C', strC);
}

var i,
    heading,
    table = document.createElement('table'),
    tbody = document.createElement('tbody'),
    tr;
for (i = 0; i <= 32; i += 1) {
    heading = i * 11.25 + [0, 5.62, -5.62][i % 3];
    tr = createRow(i % 32 + 1, getPoint(i), heading + '°');
    tbody.appendChild(tr);
}
table.appendChild(tbody);
document.body.appendChild(table);
