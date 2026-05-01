```
/*
 * Copyright (c) 2026 Jonathan Brachthäuser and contributors
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
```

# Probabilistic Inference

This case study shows how we can perform inference over probabilistic models with the help of effects.

---

We require some imports:

```
import exception
```

We need some effects to perform probabilistic operations:

```
type Distribution {
  Gaussian(mean: Double, variance: Double)
  Uniform(lower: Double, upper: Double)
  Beta(mean: Double, sampleSize: Double)
}
type Probability = Double
effect sample(dist: Distribution): Double
effect observe(value: Double, dist: Distribution): Unit
effect random(): Double
effect weight(prob: Probability): Unit
```

With the effect `sample`, we can sample from a given distribution, and with the effect `observe` we can determine how probable it is to observe a given value under a given distribution (probability density function (PDF)).

Currently, we support models with Gaussian, Uniform or Beta distributions. Thus, we need a way to draw from these distributions and to compute the probability density.

```
def draw(dist: Distribution): Double / random = {
  dist match {
    case Gaussian(mean, variance) =>
      val u1 = do random();
      val u2 = do random();
      val sigma = variance
      // box-muller transform
      val mag = sigma * sqrt(-2.0 * log(u1))
      val z0 = mag * cos(2.0 * PI * u2)
      return z0 + mean

    case Uniform(lower, upper) =>
      val x = do random()
      return (lower + x * upper)

    case Beta(mean, sampleSize) =>
      val alpha = mean * sampleSize
      return draw(Gaussian(0.5, 1.0 / (4.0 * (2.0 * alpha + 1.0))))
  }
}
```

For the Beta distribution, we need to approximate the Gamma function

```
def gamma(z0: Double): Double = {
  var z = z0
  // g is a small integer and p is a list of coeficients dependent on g
  val g = 7.0
  val p = [0.99999999999980993,
    676.5203681218851,
    -1259.1392167224028,
    771.32342877765313,
    -176.61502916214059,
    12.507343278686905,
    -0.13857109526572012,
    9.9843695780195716 * pow(10.0, 0.0 - 6.0),
    1.5056327351493116 * pow(10.0, 0.0 - 7.0)]

  // lanczos approximation
  if (z < 0.5) {
    val y = PI / (sin(PI * z) * gamma(1.0 - z))
    return y
  } else {
    with on[OutOfBounds].panic
    z = z - 1.0
    var x = p.get(0)
    var i = 1
    while (i <= 8) {
      x = x + (p.get(i) / (z + i.toDouble - 1.0))
      i = i + 1
    }
    val t = z + g + 0.5
    val y = sqrt(2.0 * PI) * pow(t, (z + 0.5)) * exp(0.0 - t) * x
    return y
  }
}
```

and then can compute the probability density function at a given value for the aforementioned distributions.

```
def density(value: Double, dist: Distribution): Double = {
  dist match {
    case Gaussian(mean, variance) =>
      val density = 1.0 / (sqrt(variance) * (sqrt(2.0 * PI))) * exp((0.0 - 1.0 / 2.0) * (square(value - mean) / variance))
      return density

    case Uniform(lower, upper) =>
      if (lower < value && value < upper) { 1.0 / (upper - lower) }
      else { 0.0 }

    case Beta(mean, sampleSize) =>
      val alpha = mean * sampleSize
      val beta = (1.0 - mean) * sampleSize
      val density = (gamma(alpha + beta) / (gamma(alpha) * gamma(beta))) * pow(value, (alpha - 1.0)) * pow((1.0 - value), (beta - 1.0))
      return density
  }
}
```

Next, we define default handlers for all effects:

```
def handleSample[R] { program: () => R / sample }: R / random = {
  try { program() }
  with sample { dist => resume(draw(dist)) }
}
def handleObserve[R] { program: () => R / observe }: R / weight = {
  try { program() }
  with observe { (value, dist) => do weight(density(value, dist)); resume(()) }
}
def handleWeight[R] { program: () => R / weight }: (R, Double)  = {
  var current = 1.0
  try { (program(), current) }
  with weight { (prob) =>
    current = current * prob;
    resume(())
  }
}
```

For generating random numbers, we either use the `random` function from stdlib

```
def handleRandom[R] { program: () => R / random }: R =
  try { program() }
  with random { () => resume(random()) }
```

or a pseudo random number generator using a linear congruential generator:

```
def linearCongruentialGenerator[R](seed: Int) { prog: => R / random } : R = {
  // parameters from Numerical Recipes (https://en.wikipedia.org/wiki/Linear_congruential_generator)
  val a = 1664525
  val c = 1013904223
  val m = 1073741824
  var x: Int = seed;
  def next() = { x = mod((a * x + c), m); x }
  try { prog() }
  with random {
    resume(next().toDouble / m.toDouble)
  }
}
```

With the effect `emit` it is also possible to limit infinite loops. This allows for flexible numbers of steps to be performed with any algorithm that uses the effect `emit`.

```
def onEmit[A] { handler: A => Unit } { program: => Unit / emit[A] }: Unit =
  try { program(); () }
  with emit[A] {
    def emit(element) = { handler(element); resume(()) }
  }
```

## Rejection Sampling

The effect `weight` is the basis of the [rejection sampling algorithm](https://en.wikipedia.org/wiki/Rejection_sampling#) is given by the following handler:

```
def handleRejection[R] { program: () => R / weight }: R / random = {
  try { program() }
  with weight { (prob) =>
    if (do random() < prob) { resume(()) }
    else { handleRejection {program} }
  }
}
```

Intuitively, invoking `do weight(p)` in some program `prog` assigns this path the probability of `p` such that `prog` is non-deterministically repeated until it is accepted by the underlying proposal distribution.

```effekt:repl
region _ {
  with linearCongruentialGenerator(1)
  with handleSample
  with handleRejection
  val N = Gaussian(0.0, 1.0)
  val sample = do sample(N)
  println(show(round(sample, 2)))
  do weight(0.01)
  println("accepted")
}
```

For easier usage, we define a wrapper to be used for rejection sampling:

```
def rejectionSampling[R] { program: () => R / { sample, observe } }: Unit / { random, emit[R] } = {
  def loop(): Unit / emit[R] = {
    with handleSample
    with handleRejection
    with handleObserve
    do emit(program())
    loop()
  }
  loop()
}
```

## Slice Sampling

The following function is implementing a form of slice sampling — a Markov Chain Monte Carlo (MCMC) algorithm often used in probabilistic programming for sampling from a distribution.
This algorithm iteratively samples from a distribution and adjusts weights or probabilities to ensure that the samples align with the target distribution.

```
def sliceSamplingAlgo[R]() { program: () => R / weight } = {
  val (result, prob) = handleSample { handleWeight { program() }}

  def step(result0: R, prob0: Probability) = handleRejection {
    val (result1, prob1) = handleWeight { program() }
    if (prob1 < prob0) { do weight(prob1 / prob0) }
    (result1, prob1)
  }
  def loop(result: R, prob: Probability): Unit / emit[R] = {
    do emit(result)
    val (result1, prob1) = step(result, prob)
    loop(result1, prob1)
  }
  loop(result, prob)
}
```

First, we get an initial sample `result` with its associated probability `prob`. `step` is then used to generate new samples based on the previous result and probability.
If the new sample has a lower probability (prob1 < prob0), the sample is down-weighted by adjusting the weight using `do weight(prob1 / prob0)`. This ensures that samples with lower probabilities are less likely to be accepted. Finally, using `loop` and `emit`, we emit the sampling results.

## Metropolis-Hastings

### Tracing

A trace records past samples used by an algorithm. The trace also controls where the algorithm draws samples from in the next iterations.
Constructing traces is possible by handling the effect `sample`, not with the default handler, but with one that draws new samples and records them in a trace.

```
type Trace = List[Probability]

def handleTracing[R] { program: () => R / sample }: (R, Trace) / sample = {
  var trace = []
  val result = try { program() }
  with sample { (dist) =>
    val d = do sample(dist);
    trace = Cons(d, trace)
    resume(d)
  }
  (result, trace.reverse)
}
def handleReusingTrace[R](trace0: Trace) { program: () => R / sample } = handleObserve {
  var trace = trace0
  try { program() }
  with sample { (dist) =>
    trace match {
      case Nil() => panic("empty trace")
      case Cons(t, ts) =>
        do observe(t, dist)
        trace = ts; resume(t)
    }
  }
}
```

### Proposing samples

How the candidates of this algorithm are proposed can vary between different implementations of the algorithm and thus creating inference of the Metropolis-Hastings algorithm.
The Metropolis-Hastings algorithm proposes a new trace based on the old trace, by recursively adding noise to the samples in the old trace and thus creating a new trace.

```
def propose(trace: Trace): Trace / sample = {
  trace match {
    case Nil() => []
    case Cons(t, ts) =>
      val noise = do sample(Gaussian(0.0, 1.0))
      val proposal = t + noise
      Cons(proposal, propose(ts))
  }
}
```

### Metropolis-Hastings algorithm

The algorithm is implemented with a helper function

```
def metropolisStep[A](prob0: Probability, trace0: Trace) { program: Trace => (A, Probability) } = {
  val trace1 = propose(trace0)
  val (result1, prob1) = program(trace1)
  if (prob1 < prob0) {
    do weight(prob1 / prob0)
  }
  ((result1, trace1), prob1)
}
```

`metropolisStep` is then called in the algorithm implementation

```
def metropolisHastingsAlgo[A] { program: () => A / { sample, weight } } = {
  val ((result0, trace0), prob0) = handleWeight {
    with handleSample
    with handleTracing
    program()
  }
  def loop(result: A, trace: Trace, prob: Probability): Unit / emit[A] = {
    do emit(result)
    val ((result1, trace1), prob1) =
    handleSample {
      with handleRejection
      metropolisStep(prob, trace) { trace =>
        with handleWeight
        with handleReusingTrace(trace)
        program()
      }
    }
    loop(result1, trace1, prob1)
  }
  loop(result0, trace0, prob0)
}
```

### Single-Site Metropolis-Hastings

To perform inference over the Metropolis-Hastings algorithm, and for creating the single-site Metropolis-Hastings algorithm, we just need to alter the implementation of the `propose` function.
In this implementation, the noise is added to only one random sample in the trace and the other samples are reused.

```
def proposeSingleSite(trace: Trace): Trace / sample = {
  val tsize = toDouble(size(trace))
  val rand = do sample(Uniform(0.0, tsize))
  def putAti(i: Int, p0: List[Double]): List[Double] = {
    p0 match {
      case Nil() => []
      case Cons(p, ps) =>
        if (i == 0) {
          val noise = do sample(Gaussian(0.0, 1.0))
          Cons(p + noise, ps)
        }
        else {
          Cons(p, putAti(i - 1, ps))
        }
    }
  }
  putAti(floor(rand), trace)
}
```

The single-site Metropolis-Hastings algorithm is implemented like the Metropolis-Hastings algorithm above.

```
def metropolisStepSingleSite[A](prob0: Probability, trace0: Trace) { program: Trace => (A, Probability) } = {
  val trace1 = proposeSingleSite(trace0)
  val (result1, prob1) = program(trace1)
  if (prob1 < prob0) {
    do weight(prob1 / prob0)
  }
  ((result1, trace1), prob1)
}
def metropolisHastingsSingleSiteAlgo[A] {program: () => A / { sample, weight } } = {
  val ((result0, trace0), prob0) = handleWeight {
    with handleSample
    with handleTracing
    program()
  }
  def loop(result: A, trace: Trace, prob: Probability): Unit / emit[A] = {
    do emit(result)
    val ((result1, trace1), prob1) = handleSample {
      with handleRejection
      metropolisStepSingleSite(prob, trace) { trace =>
        with handleWeight
        with handleReusingTrace(trace)
        program()
      }
    }
    loop(result1, trace1, prob1)
  }
  loop(result0, trace0, prob0)
}
```

## Wrappers

In order to make it easier to use these algorithms without having to call the various effect handlers, we constructed wrappers for the algorithms implemented previously.

```
def sliceSampling[R](n: Int) { program: () => R / { sample, observe } }: Unit / { random, emit[R] } = {
  with handleSample
  with sliceSamplingAlgo[R]
  with handleObserve
  program()
}
def metropolisHastings[R](n: Int) { program: () => R / { sample, observe } }: Unit / { random, emit[R] } = {
  with metropolisHastingsAlgo[R]
  with handleObserve
  program()
}
def metropolisHastingsSingleSite[R](n: Int) { program: () => R / { sample, observe } }: Unit / { random, emit[R] } = {
  with metropolisHastingsSingleSiteAlgo[R]
  with handleObserve
  program()
}
```

## Examples

### Linear Regression

As a short example of how the effects `sample` and `observe` can be used in a model, we construct a linear regression model.

```
record Point(x: Double, y: Double)

def linearRegression(observations: List[Point]) = {
  val m = do sample(Gaussian(0.0, 3.0))
  val c = do sample(Gaussian(0.0, 2.0))
  observations.foreach {
    case Point(x, y) => do observe(y, Gaussian(m * x + c, 1.0))
  }
  return Point(m, c)
}
```

Remember from earlier that `handleObserve` uses the `weight` effect operation.
Therefore, the call `do observe(y, Gaussian(m * x + c, 1.0))` can be understood as `P(Y | C)` where `Y` are the observations and `C` are the parameters sampled from a Gaussian given by `m ~ N(0, 3)` and `c ~ N(0, 2)`.
If the chosen parameters do not explain the observations well, it is likely it will be rejected and new parameters will be sampled. Conversely, if the model fits well, it is unlikely it will be rejected.

```effekt:repl
with linearCongruentialGenerator(1)
with onEmit[Point] { s => println(s.show) };

limit[Point](5) {
  with rejectionSampling[Point]

  linearRegression([
    Point(5.0, 5.0),
    Point(1.0, 1.0),
    Point(-2.0, -2.0),
    Point(3.0, 3.0),
    Point(20.0, 20.0),
    Point(5.0, 5.0)
  ])
}
```

### Robot Movements

It is also possible to construct bigger examples on which we can apply these algorithms.
In this example, the movements of a robot on a 2D-plane are observed. In the center of this plane, at the coordinates (0, 0), there is a radar station that can measure the distance to the robot at any point in time. We try to predict the state of the robot while taking into account the measurements.
The current state of the robot consists of the position and velocity.

```
record State(x: Double, y: Double, vx: Double, vy: Double)
```

In this example, we also define a type alias for `Measurement`:

```
type Measurement = Double
type Path = List[State]
```

For simulating the movement of the robot at a given state, we sample accelerations `xa`, `ya` along the x and y axis from a Gaussian distribution.
By applying the laws of motion and assuming that the difference between each time step is one, we can derive the update equations given the sampled acceleration for the velocity and the position.

```
def move(s: State): State / sample =
  s match {
    case State(x0, y0, vx0, vy0) =>
      val xa1 = do sample(Gaussian(0.0, 1.0))
      val ya1 = do sample(Gaussian(0.0, 1.0))
      val (x1, y1) = (x0 + vx0 + xa1, y0 + vy0 + ya1)
      val (vx1, vy1) = (vx0 + xa1, vy0 + ya1)
      return State(x1, y1, vx1, vy1)
  }
```

After predicting the next state of the robot, we get a distance measurement from the radar station.
We then compute the probabilistic distance given the predicted state using the Euclidean distance.
Next, we try to reconcile our model with the given measurement by applying the `obeserve` effect operation.
Similar to linear regression example, we reject our prediction if the observation is unlikely given our previously predicted acceleration.

```
def measure(s: State, m: Measurement): Unit / observe = {
  s match {
    case State(x, y, vx, vy) =>
      val dist = sqrt(square(x) + square(y))
      do observe(m, Gaussian(dist, 0.4))
  }
}
```

Putting it all together, we first call `move` for predicting the acceleration and then reconcile it with a given measurement. If the predicted state is unlikely to have produced the measurement, we predict another state. Finally, we return the first state that is not rejected.

```
def step(s0: State, m1: Measurement): State / { sample, observe } = {
  val s1 = move(s0)
  measure(s1, m1)
  return s1
}
```

We can now put our model to the test by running the following simple example:

```effekt:repl
with linearCongruentialGenerator(1)
with onEmit[State] { s => println(s.show) };

limit[State](5) {
  with sliceSampling[State](5)

  val init = State(0.0, 3.0, 2.0, 0.0)
  step(init, 5.0)
}
```

As a more complex example, we now also probabilisticly augment the distance measurements by applying Gaussian noise to it instead of keeping constant.
More importantly, we now predict five different possible paths consisting of five states each.

```effekt:repl
with linearCongruentialGenerator(1)
with onEmit[Path] { path => println(path.show) };

limit[Path](5) {
  with sliceSampling[Path](1);

  var nextState = State(0.0, 3.0, 2.0, 0.0)
  var nextdis = 5.0
  var m = 5
  var path = [nextState]
  while (m > 0) {
    nextState = step(nextState, nextdis)
    val noise = do sample(Gaussian(0.0, 1.0))
    nextdis = nextdis + noise
    path = append(path, [nextState])
    m = m - 1
  }
  path
}
```

### Epidemic Spreading

This example simulates a population experiencing an epidemic outbreak with the SIR model. The SIR model divides the population into susceptible **S**, infected **I** and recovered **R**.

```
record Population(susceptible: Double, infected: Double, recovered: Double)
```

Having defined our population state, we now try to predict the population state at the next time step:

```
def progression(p: Population): Population / sample = {
  p match {
    case Population(s, i, r) =>
      val noise = do sample(Gaussian(0.0, 0.01))
      val s1 = 0.5 * s - noise
      val i1 = 0.3 * i + 0.5 * s + 0.1 * r + noise
      val r1 =  0.9 * r + 0.7 * i
      return Population(s1, i1, r1)
  }
}
```

Next, given a population and a positive-rate of COVID-like test, we gauge how likely it is that the positive-rate was observed assuming the test result is distributed according to a Beta distribution.
Formally, we compute $\text{Beta}(pr \mid I, 100)$ where $I$ is the number of infected individuals in the predicted population.
If it is unlikely, the predicted population will be rejected and another one will be proposed.

```
def test(p: Population, pr: Double): Unit / observe = {
  p match {
    case Population(s, i, r) =>
      val mean = i
      val sampleSize = 100.0
      do observe(pr, Beta(mean, sampleSize))
  }
}
```

Like in the previous examples, we combine these two functions into a `step` function:

```
// approximate next state of population based on current state
def step(p0: Population, pr1: Double): Population / { sample, observe } = {
  val p1 = progression(p0)
  test(p1, pr1)
  return p1
}
```

Testing it on a simple example, is similar as we have seen previously with the robot movement predictions:

```effekt:repl
with linearCongruentialGenerator(1)
with onEmit[Population] { p => println(p.show) };

limit[Population](5) {
  with metropolisHastings[Population](5)

  val init = Population(0.7, 0.2, 0.1)
  step(init, 0.8)
}
```

We again can also predict multiple different scenarios where the disease spreads differently throughout a population:

```effekt:repl
with linearCongruentialGenerator(1)
with onEmit[List[Population]] { ps => println(ps.show) }

limit[List[Population]](1) {
  with metropolisHastings[List[Population]](1)

  var nextPop = Population(0.7, 0.2, 0.1)
  var nextpr = 0.8
  var m = 5
  var path : List[Population] = [nextPop]
  while (m > 0) {
    nextPop = step(nextPop, nextpr)
    val noise = do sample(Gaussian(0.0, 0.01))
    var nextpr1 = nextpr + noise
    if (not(nextpr1 < 0.0 || nextpr1 > 1.0)) { nextpr = nextpr1 }
    path = append(path, [nextPop])
    m = m - 1
  }
  path
}
```

The other algorithms of this library can be called used similar as shown in the previous examples.
