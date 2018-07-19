$dir = 'path_here';

if(is_dir($dir)){
  //scandir grabs the contents of a directory and array_diff is being used to filter out .. and .
  $list = array_diff(scandir($dir), array('..', '.'));
  //now we can just use empty to check if the variable has any contents regardless of it's type
  if(empty($list)){
    echo 'dir is empty';
  }
  else{
    echo 'dir is not empty';
  }
}
else{
  echo 'not a directory';
}
