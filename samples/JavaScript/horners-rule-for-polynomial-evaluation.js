function horner(coeffs, x) {
    return coeffs.reduceRight(function(acc, coeff) {return(acc * x + coeff)}, 0);
}
print(horner([-19,7,-4,6],3));  // ==> 128
