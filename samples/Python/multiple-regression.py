import numpy as np
from numpy.random import random

n=100; k=10
y = np.mat(random((1,n)))
X = np.mat(random((k,n)))

b= y * X.T * np.linalg.inv(X*X.T)
print(b)
