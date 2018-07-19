# First, define a bernoulli sample, of length 26.
x <- sample(c(0, 1), 26, replace=T)

x
# [1] 1 1 1 1 0 1 1 0 1 0 1 1 1 0 1 1 0 1 0 1 0 1 1 0 1 0

# Give names to the entries. "letters" is a builtin value
names(x) <- letters

x
# a b c d e f g h i j k l m n o p q r s t u v w x y z
# 1 1 1 1 0 1 1 0 1 0 1 1 1 0 1 1 0 1 0 1 0 1 1 0 1 0

# The unstable one, see how "a" appears after "l" now
sort(x, method="quick")
# z h s u e q x n j r t v w y p o m l a i g f d c b k
# 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

# The stable sort, letters are ordered in each section
sort(x, method="shell")
# e h j n q s u x z a b c d f g i k l m o p r t v w y
# 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
