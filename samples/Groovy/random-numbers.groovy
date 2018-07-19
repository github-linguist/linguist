rnd = new Random()
result = (1..1000).inject([]) { r, i -> r << rnd.nextGaussian() }
