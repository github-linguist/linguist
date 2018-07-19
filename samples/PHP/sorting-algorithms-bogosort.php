function bogosort($l) {
    while (!in_order($l))
        shuffle($l);
    return $l;
}

function in_order($l) {
    for ($i = 1; $i < count($l); $i++)
        if ($l[$i] < $l[$i-1])
            return FALSE;
    return TRUE;
}
