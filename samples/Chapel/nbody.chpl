/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Albert Sidelnik
   modified by Brad Chamberlain
*/


//
// The number of timesteps to simulate; may be set via the command-line
//
config const n = 10000;

//
// Constants representing pi, the solar mass, and the number of days per year
//
const pi = 3.141592653589793,
      solarMass = 4 * pi**2,
      daysPerYear = 365.24;

//
// a record representing one of the bodies in the solar system
//
record body {
  var pos: 3*real;
  var v: 3*real;
  var mass: real;  // does not change after it is set up
}

//
// the array of bodies that we'll be simulating
//
var bodies = [/* sun */
              new body(mass = solarMass),

              /* jupiter */
              new body(pos = ( 4.84143144246472090e+00,
                              -1.16032004402742839e+00,
                              -1.03622044471123109e-01),
                         v = ( 1.66007664274403694e-03 * daysPerYear,
                               7.69901118419740425e-03 * daysPerYear,
                              -6.90460016972063023e-05 * daysPerYear),
                      mass =   9.54791938424326609e-04 * solarMass),
  
              /* saturn */
              new body(pos = ( 8.34336671824457987e+00,
                               4.12479856412430479e+00,
                              -4.03523417114321381e-01),
                         v = (-2.76742510726862411e-03 * daysPerYear,
                               4.99852801234917238e-03 * daysPerYear,
                               2.30417297573763929e-05 * daysPerYear),
                      mass =   2.85885980666130812e-04 * solarMass),

              /* uranus */
              new body(pos = ( 1.28943695621391310e+01,
                              -1.51111514016986312e+01,
                              -2.23307578892655734e-01),
                         v = ( 2.96460137564761618e-03 * daysPerYear,
                               2.37847173959480950e-03 * daysPerYear,
                              -2.96589568540237556e-05 * daysPerYear),
                      mass =   4.36624404335156298e-05 * solarMass),

              /* neptune */
              new body(pos = ( 1.53796971148509165e+01,
                              -2.59193146099879641e+01,
                               1.79258772950371181e-01),
                         v = ( 2.68067772490389322e-03 * daysPerYear,
                               1.62824170038242295e-03 * daysPerYear,
                              -9.51592254519715870e-05 * daysPerYear),
                      mass =   5.15138902046611451e-05 * solarMass)
              ];

//
// the number of bodies to be simulated
//
const numbodies = bodies.numElements;

//
// The computation involves initializing the sun's velocity,
// writing the initial energy, advancing the system through 'n'
// timesteps, and writing the final energy.
//
proc main() {
  initSun();

  writef("%.9r\n", energy());
  for 1..n do
    advance(0.01);
  writef("%.9r\n", energy());
}

//
// compute the sun's initial velocity
//
proc initSun() {
  const p = + reduce (for b in bodies do (b.v * b.mass));
  bodies[1].v = -p / solarMass;
}

//
// advance the positions and velocities of all the bodies
//
proc advance(dt) {
  for i in 1..numbodies {
    for j in i+1..numbodies {
      updateVelocities(bodies[i], bodies[j]);
      
      inline proc updateVelocities(ref b1, ref b2) {
        const dpos = b1.pos - b2.pos,
               mag = dt / sqrt(sumOfSquares(dpos))**3;
        
        b1.v -= dpos * b2.mass * mag;
        b2.v += dpos * b1.mass * mag;
      }
    }
  }
  
  for b in bodies do
    b.pos += dt * b.v;
}

//
// compute the energy of the bodies
//
proc energy() {
  var e = 0.0;
  
  for i in 1..numbodies {
    const b1 = bodies[i];
    
    e += 0.5 * b1.mass * sumOfSquares(b1.v);
    
    for j in i+1..numbodies {
      const b2 = bodies[j];
      
      e -= (b1.mass * b2.mass) / sqrt(sumOfSquares(b1.pos - b2.pos));
    }
  }
  
  return e;
}

//
// a helper routine to compute the sum of squares of a 3-tuple's components
//
inline proc sumOfSquares(x)
  return x(1)**2 + x(2)**2 + x(3)**2;
