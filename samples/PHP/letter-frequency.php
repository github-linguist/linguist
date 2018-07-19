<?php
print_r(array_count_values(str_split(file_get_contents($argv[1]))));
?>
