<?php
$queue = new SplQueue;
echo $queue->isEmpty() ? 'true' : 'false', "\n";  // empty test - returns true
// $queue->dequeue();                             // would raise RuntimeException
$queue->enqueue(1);
$queue->enqueue(2);
$queue->enqueue(3);
echo $queue->dequeue(), "\n";                     // returns 1
echo $queue->isEmpty() ? 'true' : 'false', "\n";  // returns false
?>
