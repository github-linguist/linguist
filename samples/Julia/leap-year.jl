leap(y) = y%4==0 && (y<1582 || y%400==0 || y%100!=0)

# some tests
@assert all([leap(yr) for yr in [2400, 2012, 2000, 1600, 1500, 1400]])
@assert all([~leap(yr) for yr in [2100, 2014, 1900, 1800, 1700, 1499]])
