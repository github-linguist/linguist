test() {
    int *arr = ({1, 2, 3});
    int *i = filter(arr, (: $1 > 1 :));
}

private object *apply_custom_filter(object *obs, function f, object tp) {
    return filter(obs, (: (*$(f))($1, $(tp)) :)) ;
}