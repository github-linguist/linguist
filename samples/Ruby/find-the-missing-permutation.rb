given = %w{
  ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
  CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB
}

all = given[0].split(//).permutation.collect {|perm| perm.join('')}

missing = all - given   # ["DBAC"]
