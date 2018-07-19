<?php
$stdin = fopen("php://stdin", "r");
$allowed = array(1 => 'fee fie', 'huff and puff', 'mirror mirror', 'tick tock');

for(;;) {
    foreach ($allowed as $id => $name) {
        echo "  $id: $name\n";
    }
    echo "Which is from the four pigs: ";
    $stdin_string = fgets($stdin, 4096);
    if (isset($allowed[(int) $stdin_string])) {
        echo "You chose: {$allowed[(int) $stdin_string]}\n";
        break;
    }
}
