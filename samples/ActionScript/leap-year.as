public function isLeapYear(year:int):Boolean {
    if (year % 100 == 0) {
        return (year % 400 == 0);
    }
    return (year % 4 == 0);
}
