import java.util.Random

list = double[999]
mean = 1.0
std = 0.5
rng = Random.new
0.upto(998) do | i |
    list[i] = mean + std * rng.nextGaussian
end
