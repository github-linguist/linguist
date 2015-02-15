<?php

function rref($matrix)
{
    $lead = 0;
    $rowCount = count($matrix);
    if ($rowCount == 0)
        return $matrix;
    $columnCount = 0;
    if (isset($matrix[0])) {
        $columnCount = count($matrix[0]);
    }
    for ($r = 0; $r < $rowCount; $r++) {
        if ($lead >= $columnCount)
            break;        {
            $i = $r;
            while ($matrix[$i][$lead] == 0) {
                $i++;
                if ($i == $rowCount) {
                    $i = $r;
                    $lead++;
                    if ($lead == $columnCount)
                        return $matrix;
                }
            }
            $temp = $matrix[$r];
            $matrix[$r] = $matrix[$i];
            $matrix[$i] = $temp;
        }        {
            $lv = $matrix[$r][$lead];
            for ($j = 0; $j < $columnCount; $j++) {
                $matrix[$r][$j] = $matrix[$r][$j] / $lv;
            }
        }
        for ($i = 0; $i < $rowCount; $i++) {
            if ($i != $r) {
                $lv = $matrix[$i][$lead];
                for ($j = 0; $j < $columnCount; $j++) {
                    $matrix[$i][$j] -= $lv * $matrix[$r][$j];
                }
            }
        }
        $lead++;
    }
    return $matrix;
}
?>
