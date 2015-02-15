RegExp.escape = function(text) {
    return text.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&");
}

multisplit = function(string, seps) {
    var sep_regex = RegExp(_.map(seps, function(sep) { return RegExp.escape(sep); }).join('|'));
    return string.split(sep_regex);
}
