function createMatrix($size)
{
    $result = array();

    for ($i = 0; $i < $size; $i++) {
        $row      = array_fill(0, $size, 0);
        $row[$i]  = 1;
        $result[] = $row;
    }

    return $result;
}

function printMatrix(array $matrix)
{
    foreach ($matrix as $row) {
        foreach ($row as $column) {
            echo $column . " ";
        }
        echo PHP_EOL;
    }
    echo PHP_EOL;
}

printMatrix(createMatrix(5));
