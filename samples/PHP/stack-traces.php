<?php
class StackTraceDemo {
    static function inner() {
        debug_print_backtrace();
    }
    static function middle() {
        self::inner();
    }
    static function outer() {
        self::middle();
    }
}

StackTraceDemo::outer();
?>
