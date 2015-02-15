def div_check(x, y):
  try:
    x / y
  except ZeroDivisionError:
    return True
  else:
    return False
