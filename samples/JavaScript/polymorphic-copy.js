function clone(obj){
    if (obj == null || typeof(obj) != 'object')
        return obj;

    var temp = {};
    for (var key in obj)
        temp[key] = clone(obj[key]);
    return temp;
}
