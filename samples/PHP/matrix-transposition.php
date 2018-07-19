function transpose($m) {
  if (count($m) == 0) // special case: empty matrix
    return array();
  else if (count($m) == 1) // special case: row matrix
    return array_chunk($m[0], 1);

  // array_map(NULL, m[0], m[1], ..)
  array_unshift($m, NULL); // the original matrix is not modified because it was passed by value
  return call_user_func_array('array_map', $m);
}
