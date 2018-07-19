var data = [
    {name: "Tyler Bennett",   id: "E10297", salary: 32000, dept: "D101"},
    {name: "John Rappl",      id: "E21437", salary: 47000, dept: "D050"},
    {name: "George Woltman",  id: "E00127", salary: 53500, dept: "D101"},
    {name: "Adam Smith",      id: "E63535", salary: 18000, dept: "D202"},
    {name: "Claire Buckman",  id: "E39876", salary: 27800, dept: "D202"},
    {name: "David McClellan", id: "E04242", salary: 41500, dept: "D101"},
    {name: "Rich Holcomb",    id: "E01234", salary: 49500, dept: "D202"},
    {name: "Nathan Adams",    id: "E41298", salary: 21900, dept: "D050"},
    {name: "Richard Potter",  id: "E43128", salary: 15900, dept: "D101"},
    {name: "David Motsinger", id: "E27002", salary: 19250, dept: "D202"},
    {name: "Tim Sampair",     id: "E03033", salary: 27000, dept: "D101"},
    {name: "Kim Arlich",      id: "E10001", salary: 57000, dept: "D190"},
    {name: "Timothy Grove",   id: "E16398", salary: 29900, dept: "D190"},
];

function top_rank(n) {
    var by_dept = group_by_dept(data);
    for (var dept in by_dept) {
        output(dept);
        for (var i = 0; i < n && i < by_dept[dept].length; i++) {
            var emp = by_dept[dept][i];
            output(emp.name + ", id=" + emp.id + ", salary=" + emp.salary);
        }
        output("");
    }
}

// group by dept, and sort by balary
function group_by_dept(data) {
    var by_dept = {};
    for (var idx in data)  {
        var dept = data[idx].dept;
        if ( ! has_property(by_dept, dept)) {
            by_dept[dept] = new Array();
        }
        by_dept[dept].push(data[idx]);
    }
    for (var dept in by_dept) {
        // numeric sort
        by_dept[dept].sort(function (a,b){return b.salary - a.salary});
    }
    return by_dept;
}

function has_property(obj, propname) {
    return typeof(obj[propname]) != "undefined";
}

function output(str) {
    try {
        WScript.Echo(str);  // WSH
    } catch(err) {
        print(str);  // Rhino
    }
}

top_rank(3);
