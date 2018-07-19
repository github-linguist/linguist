#        NAME, WEIGHT, VALUE (for this weight)
items = [("beef",    3.8, 36.0),
         ("pork",    5.4, 43.0),
         ("ham",     3.6, 90.0),
         ("greaves", 2.4, 45.0),
         ("flitch",  4.0, 30.0),
         ("brawn",   2.5, 56.0),
         ("welt",    3.7, 67.0),
         ("salami",  3.0, 95.0),
         ("sausage", 5.9, 98.0)]

MAXWT = 15.0

sorted_items = sorted(((value/amount, amount, name)
                       for name, amount, value in items),
                      reverse = True)
wt = val = 0
bagged = []
for unit_value, amount, name in sorted_items:
    portion = min(MAXWT - wt, amount)
    wt     += portion
    addval  = portion * unit_value
    val    += addval
    bagged += [(name, portion, addval)]
    if wt >= MAXWT:
        break

print("    ITEM   PORTION VALUE")
print("\n".join("%10s %6.2f %6.2f" % item for item in bagged))
print("\nTOTAL WEIGHT: %5.2f\nTOTAL VALUE: %5.2f" % (wt, val))
