void main() {
    DateYear[] years = {1900, 1994, 1996, 1997, 2000};
    foreach (DateYear year in years) {
        string status = year.is_leap_year() ? "" : "not ";
        print("%d is %sa leap year.\n", (int) year, status);
    }
}
