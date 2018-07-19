error_reporting(E_ALL & ~ ( E_NOTICE | E_WARNING ));

while (true) {
    echo "\nEnter a value in kelvin (q to quit): ";
    if (($kelvin = trim(fgets(STDIN))) !== false) {
        if ($kelvin == 'q') {
            echo 'quitting';
            break;
        }
        if (is_numeric($kelvin)) {
            $kelvin = floatVal($kelvin);
            if ($kelvin >= 0) {
                printf(" K  %2.2f\n", $kelvin);
                printf(" C  %2.2f\n", $kelvin - 273.15);
                printf(" F  %2.2f\n", $kelvin * 1.8 - 459.67);
                printf(" R  %2.2f\n", $kelvin * 1.8);
            } else printf(" %2.2f K is below absolute zero\n", $kelvin);
        }
    }
}
