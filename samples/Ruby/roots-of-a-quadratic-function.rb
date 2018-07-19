require 'complex'

def quadratic(a, b, c)
  sqrt_discriminant = Math.sqrt(b**2 - 4*a*c)
  [(-b + sqrt_discriminant) / (2.0*a), (-b - sqrt_discriminant) / (2.0*a)]
end

p quadratic(3, 4, 4/3.0)  # [-2/3]
p quadratic(3, 2, -1)     # [1/3, -1]
p quadratic(3, 2,  1)     # [(-1/3 + sqrt(2/9)i), (-1/3 - sqrt(2/9)i)]
p quadratic(1, 0,  1)     # [(0+i), (0-i)]
p quadratic(1, -1e6, 1)   # [1e6, 1e-6]
p quadratic(-2,  7, 15)   # [-3/2, 5]
p quadratic(1, -2,  1)    # [1]
p quadratic(1,  3,  3)    # [(-3 + sqrt(3)i)/2), (-3 - sqrt(3)i)/2)]
