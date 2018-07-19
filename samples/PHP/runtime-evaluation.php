<?php
  $code = 'echo "hello world"';
  eval($code);
  $code = 'return "hello world"';
  print eval($code);
