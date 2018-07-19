These numbers are given in the default output format. Large numbers are given in scientific notation.
{6.7^-4,6.7^6,6.7^8}
{0.00049625,90458.4,4.06068*10^6}

This gives all numbers in scientific notation.
ScientificForm[%]
{4.9625*10^(-4),9.04584*10^(4),4.06068*10^(6)}

This gives the numbers in engineering notation, with exponents arranged to be multiples of three.
EngineeringForm[%]
{496.25*10^(-6),90.4584*10^(3),4.06068*10^(6)}

In accounting form, negative numbers are given in parentheses, and scientific notation is never used.
AccountingForm[{5.6,-6.7,10.^7}]
{5.6,(6.7),10000000.}
