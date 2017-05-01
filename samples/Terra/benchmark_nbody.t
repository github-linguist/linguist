local C = terralib.includecstring[[

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

]]

pi = 3.141592653589793
solar_mass = (4 * pi * pi)
days_per_year = 365.24

struct planet {
  x : double;
  y : double;
  z : double;
  vx : double;
  vy : double;
  vz : double;
  mass : double;
}

terra advance(nbodies : int, bodies : &planet, dt : double)
  
  for i = 0, nbodies do
    var b = &bodies[i]
    for j = i + 1, nbodies do
      var b2 = &bodies[j]
      
      var dx = b.x - b2.x;
      var dy = b.y - b2.y;
      var dz = b.z - b2.z;
      var distance = C.sqrt(dx * dx + dy * dy + dz * dz);
      var mag = dt / (distance * distance * distance);
      --C.printf("%f %f %f %f %f\n",dx,dy,dz,distance,mag);
      b.vx = b.vx - dx * b2.mass * mag;
      b.vy = b.vy - dy * b2.mass * mag;
      b.vz = b.vz - dz * b2.mass * mag;
      b2.vx = b2.vx + dx * b.mass * mag;
      b2.vy = b2.vy + dy * b.mass * mag;
      b2.vz = b2.vz + dz * b.mass * mag;
      --C.printf("%f %f %f %f %f %f\n",b.vx,b.vy,b.vz,b2.vx,b2.vy,b2.vz)
    end
  end
  for i = 0,nbodies do
    var b = &bodies[i]
    b.x = b.x + dt * b.vx;
    b.y = b.y + dt * b.vy;
    b.z = b.z + dt * b.vz;
  end
end

terra energy(nbodies : int, bodies : &planet)
  var e = 0.0
  for i = 0, nbodies do
    var b = &bodies[i]
    e = e + 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz);
    for j = i + 1, nbodies do
      var b2 = &bodies[j]
      var dx = b.x - b2.x
      var dy = b.y - b2.y
      var dz = b.z - b2.z
      var distance = C.sqrt(dx * dx + dy * dy + dz * dz)
      e = e - (b.mass * b2.mass) / distance
    end
  end
  return e
end

terra offset_momentum(nbodies : int, bodies : &planet)
  var px,py,pz = 0.0,0.0,0.0
  
  for i = 0,nbodies do
    px = px + bodies[i].vx * bodies[i].mass
    py = py + bodies[i].vy * bodies[i].mass
    pz = pz + bodies[i].vz * bodies[i].mass
  end
  bodies[0].vx = - px / solar_mass
  bodies[0].vy = - py / solar_mass
  bodies[0].vz = - pz / solar_mass
end

NBODIES = 5

terra main(argc : int, argv : &&int8)
    var bodies = array(
      planet {                               -- sun */
        0, 0, 0, 0, 0, 0, solar_mass
      },
      planet {                               -- jupiter */
        4.84143144246472090e+00,
        -1.16032004402742839e+00,
        -1.03622044471123109e-01,
        1.66007664274403694e-03 * days_per_year,
        7.69901118419740425e-03 * days_per_year,
        -6.90460016972063023e-05 * days_per_year,
        9.54791938424326609e-04 * solar_mass
      },
      planet {                               -- saturn */
        8.34336671824457987e+00,
        4.12479856412430479e+00,
        -4.03523417114321381e-01,
        -2.76742510726862411e-03 * days_per_year,
        4.99852801234917238e-03 * days_per_year,
        2.30417297573763929e-05 * days_per_year,
        2.85885980666130812e-04 * solar_mass
      },
      planet {                               -- uranus */
        1.28943695621391310e+01,
        -1.51111514016986312e+01,
        -2.23307578892655734e-01,
        2.96460137564761618e-03 * days_per_year,
        2.37847173959480950e-03 * days_per_year,
        -2.96589568540237556e-05 * days_per_year,
        4.36624404335156298e-05 * solar_mass
      },
      planet {                               -- neptune */
        1.53796971148509165e+01,
        -2.59193146099879641e+01,
        1.79258772950371181e-01,
        2.68067772490389322e-03 * days_per_year,
        1.62824170038242295e-03 * days_per_year,
        -9.51592254519715870e-05 * days_per_year,
        5.15138902046611451e-05 * solar_mass
      }
    )
    var n = C.atoi(argv[1])    
    offset_momentum(NBODIES, bodies)
    C.printf ("%.9f\n", energy(NBODIES, bodies))
    for i = 0,n do
        advance(NBODIES, bodies, 0.01)
    end
    C.printf ("%.9f\n", energy(NBODIES, bodies));
  return 0
end

terra run()
    main(2,array("what","1000000"))
end

--run:compile()


--local test = require("test")

--print(test.time(run))

terralib.saveobj("benchmark_nbody",{ main = main } )
energy:disas()
energy:printpretty()