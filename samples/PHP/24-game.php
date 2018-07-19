#!/usr/bin/env php
The 24 Game

Given any four digits in the range 1 to 9, which may have repetitions,
Using just the +, -, *, and / operators; and the possible use of
brackets, (), show how to make an answer of 24.

An answer of "q" will quit the game.
An answer of "!" will generate a new set of four digits.
Otherwise you are repeatedly asked for an expression until it evaluates to 24

Note: you cannot form multiple digit numbers from the supplied digits,
so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.

<?php

while (true) {
    $numbers = make_numbers();

    for ($iteration_num = 1; ; $iteration_num++) {
        echo "Expresion $iteration_num: ";

        $entry = rtrim(fgets(STDIN));

        if ($entry === '!') break;
        if ($entry === 'q') exit;

        $result = play($numbers, $entry);

        if ($result === null) {
            echo "That's not valid\n";
            continue;
        }
        elseif ($result != 24) {
            echo "Sorry, that's $result\n";
            continue;
        }
        else {
            echo "That's right! 24!!\n";
            exit;
        }
    }
}

function make_numbers() {
    $numbers = array();

    echo "Your four digits: ";

    for ($i = 0; $i < 4; $i++) {
        $number = rand(1, 9);
        // The check is needed to avoid E_NOTICE from PHP
        if (!isset($numbers[$number])) {
            $numbers[$number] = 0;
        }
        $numbers[$number]++;
        print "$number ";
    }

    print "\n";

    return $numbers;
}

function play($numbers, $expression) {
    $operator = true;
    for ($i = 0, $length = strlen($expression); $i < $length; $i++) {
        $character = $expression[$i];

        if (in_array($character, array('(', ')', ' ', "\t"))) continue;

        $operator = !$operator;

        if (!$operator) {
            if (!empty($numbers[$character])) {
                $numbers[$character]--;
                continue;
            }
            return;
        }
        elseif (!in_array($character, array('+', '-', '*', '/'))) {
            return;
        }
    }

    foreach ($numbers as $remaining) {
        if ($remaining > 0) {
            return;
        }
    }

    return eval("return $expression;");
}
?>
