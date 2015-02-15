prec = get_bigfloat_precision()
spi = ""
digit = 1
while true
  if digit > length(spi) - 6
    prec *= 2
    set_bigfloat_precision(prec)
    spi = string(big(π))
  end
  print(spi[digit])
  digit += 1
end
