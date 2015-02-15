for ($i = 1; $i <= 10; $i++) {
    echo $i;
    if ($i % 5 == 0) {
        echo "\n";
        continue;
    }
    echo ', ';
}
