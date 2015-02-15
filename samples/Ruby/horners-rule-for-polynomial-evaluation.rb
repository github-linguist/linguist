def horner(coeffs, x)
  coeffs.reverse.inject(0) {|acc, coeff| acc * x + coeff}
end
p horner([-19, 7, -4, 6], 3)  # ==> 128
