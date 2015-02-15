function rangex($str) {
    $lst = array();
    foreach (explode(',', $str) as $e) {
        if (strpos($e, '-', 1) !== FALSE) {
            list($a, $b) = explode('-', substr($e, 1), 2);
            $lst = array_merge($lst, range($e[0] . $a, $b));
        } else {
            $lst[] = (int) $e;
        }
    }
    return $lst;
}
