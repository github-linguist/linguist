<?php
function a() {
    static $i = 0;
    print ++$i . "\n";
    a();
}
a();
