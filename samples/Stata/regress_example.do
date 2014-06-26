* Setup
sysuse auto

* Fit a linear regression
regress mpg weight foreign

* Fit a better linear regression, from a physics standpoint
gen gp100m = 100/mpg
regress gp100m weight foreign

* Obtain beta coefficients without refitting model
regress, beta

* Suppress intercept term
regress weight length, noconstant

* Model already has constant
regress weight length bn.foreign, hascons
