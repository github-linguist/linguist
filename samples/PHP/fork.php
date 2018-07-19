<?php
$pid = pcntl_fork();
if ($pid == 0)
  echo "This is the new process\n";
else if ($pid > 0)
  echo "This is the original process\n";
else
  echo "ERROR: Something went wrong\n";
?>
