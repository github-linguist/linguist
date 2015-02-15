print '''
  n        binary
----- ---------------
'''
[5, 50, 9000].each {
    printf('%5d %15s\n', it, Integer.toBinaryString(it))
}
