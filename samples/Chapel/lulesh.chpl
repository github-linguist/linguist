/*
  Derived from the DARPA/Livermore Unstructured Lagrangian Explicit
  Shock Hydrodynamics (LULESH)
  https://computation.llnl.gov/casc/ShockHydro/

  Original port to Chapel by Brandon Holt (8/2011).  Further
  improvements for the sake of performance and/or generality made by
  Sung-Eun Choi (12/2011, 11/2012), Jeff Keasler (3/2012), and Brad
  Chamberlain (3-4,9-11/2012, 2/2013).


  Notes on the Initial Implementation
  -----------------------------------
   
  This implementation was designed to mirror the overall structure of
  the C++ Lulesh but use Chapel constructs where they can help make
  the code more readable, easier to maintain, or more
  'elegant'. Function names are preserved for the most part, with some
  additional helper functions, and original comments from the C++ code
  are interspersed approximately where they belong to give an idea of
  how the two codes line up. One major difference for this Chapel
  version is the use of a number of module-level variables and
  constants.


  Status:

  This code remains a work-in-progress as we gain further experience
  with it.  Proposed improvements are noted in the README in this
  directory and (in some cases) in TODO comments in the code.

 */



use Time,       // to get timing routines for benchmarking
    BlockDist;  // for block-distributed arrays

use luleshInit;   // initialization code for data set

/* The configuration parameters for lulesh.  These can be set on the
   compiler command line using -s<paramName>=<value>.  For example,

     chpl -suseBlockDist=true

   useBlockDist : says whether or not to block-distribute the arrays.
                  The default is based on the value of CHPL_COMM, as
                  an indication of whether this is a single- or multi-
                  locale execution.

   use3DRepresentation : indicates whether the element node arrays
                         should be stored using a 3D representation
                         (limiting the execution to cube inputs) or
                         the more general 1D representation (supporting
                         arbitrary data sets).

   useSparseMaterials : indicates whether sparse domains/arrays should be
                        used to represent the materials.  Sparse domains
                        are more realistic in that they permit an arbitrary
                        subset of the problem space to store a material.
                        Dense domains are sufficient for LULESH since there's
                        an assumption that the material spans all cells.

   printWarnings : prints performance-oriented warnings to prevent
                   surprises.
*/
   
config param useBlockDist = (CHPL_COMM != "none"),
             use3DRepresentation = false,
             useSparseMaterials = true,
             printWarnings = true;


//
// Sanity check to ensure that input files aren't used with the 3D
// representation
//
if (use3DRepresentation && (luleshInit.filename != "")) then
  halt("The 3D representation does not support reading input from files");


/* Configuration constants: Override defaults on executable's command-line */

config const initialEnergy = 3.948746e+7;            // initial energy value


config const showProgress = false,   // print time and dt values on each step
             debug = false,          // print various debug info
             doTiming = true,        // time the main timestep loop
             printCoords = true;     // print the final computed coordinates


/* Compile-time constants */

param XI_M        = 0x003,
      XI_M_SYMM   = 0x001,
      XI_M_FREE   = 0x002,

      XI_P        = 0x00c,
      XI_P_SYMM   = 0x004,
      XI_P_FREE   = 0x008,

      ETA_M       = 0x030,
      ETA_M_SYMM  = 0x010,
      ETA_M_FREE  = 0x020,

      ETA_P       = 0x0c0,
      ETA_P_SYMM  = 0x040,
      ETA_P_FREE  = 0x080,

      ZETA_M      = 0x300,
      ZETA_M_SYMM = 0x100,
      ZETA_M_FREE = 0x200,

      ZETA_P      = 0xc00,
      ZETA_P_SYMM = 0x400,
      ZETA_P_FREE = 0x800;


/* Set up the problem size */

const (numElems, numNodes) = initProblemSize();


/* Declare abstract problem domains */

const ElemSpace = if use3DRepresentation
                    then {0..#elemsPerEdge, 0..#elemsPerEdge, 0..#elemsPerEdge}
                    else {0..#numElems},
      NodeSpace = if use3DRepresentation
                    then {0..#nodesPerEdge, 0..#nodesPerEdge, 0..#nodesPerEdge}
                    else {0..#numNodes};


/* Declare the (potentially distributed) problem domains */

const Elems = if useBlockDist then ElemSpace dmapped Block(ElemSpace)
                              else ElemSpace,
      Nodes = if useBlockDist then NodeSpace dmapped Block(NodeSpace)
                              else NodeSpace;


/* The coordinates */

var x, y, z: [Nodes] real;
                              

/* The number of nodes per element.  In a rank-independent version,
   this could be written 2**rank */

param nodesPerElem = 8;

                                 
// We could name this, but chose not to since it doesn't add that much clarity
//
// const elemNeighbors = 1..nodesPerElem;


/* The element-to-node mapping */

var elemToNode: [Elems] nodesPerElem*index(Nodes);


/* the Greek variables */

var lxim, lxip, letam, letap, lzetam, lzetap: [Elems] index(Elems);


/* the X, Y, Z Symmetry values */

var XSym, YSym, ZSym: sparse subdomain(Nodes);



/* Constants */

const u_cut = 1.0e-7,           /* velocity tolerance */
      hgcoef = 3.0,             /* hourglass control */
      qstop = 1.0e+12,          /* excessive q indicator */
      monoq_max_slope = 1.0,
      monoq_limiter_mult = 2.0,
      e_cut = 1.0e-7,           /* energy tolerance */
      p_cut = 1.0e-7,           /* pressure tolerance */
      ss4o3 = 4.0/3.0,
      q_cut = 1.0e-7,           /* q tolerance */
      v_cut = 1.0e-10,          /* relative volume tolerance */
      qlc_monoq = 0.5,          /* linear term coef for q */
      qqc_monoq = 2.0/3.0,      /* quadratic term coef for q */
      qqc = 2.0, 
      qqc2 = 64.0 * qqc**2,
      eosvmax = 1.0e+9,
      eosvmin = 1.0e-9,
      pmin = 0.0,               /* pressure floor */
      emin = -1.0e+15,          /* energy floor */
      dvovmax = 0.1,            /* maximum allowable volume change */
      refdens = 1.0,            /* reference density */

      deltatimemultlb = 1.1,
      deltatimemultub = 1.2,
      dtmax = 1.0e-2;           /* maximum allowable time increment */

                              
config const stoptime = 1.0e-2,      /* end time for simulation */
             maxcycles = max(int),   /* max number of cycles to simulate */
             dtfixed = -1.0e-7;      /* fixed time increment */


/* The list of material elements */

const MatElems: MatElemsType = if useSparseMaterials then enumerateMatElems()
                                                     else Elems;


proc MatElemsType type {
  if useSparseMaterials {
    if (printWarnings && useBlockDist && numLocales > 1) then
      writeln("WARNING: The LULESH Material Elements (MatElems) are not yet\n",
              "         distributed, so result in excessive memory use on,\n",
              "         and communication with, locale 0\n");
    return sparse subdomain(Elems);
  } else
    return Elems.type;
}

iter enumerateMatElems() {
  if (printWarnings && useBlockDist && numLocales > 1) then
    writeln("WARNING: generation of matrix elements is serial and\n",
            "         unlikely to scale");
  for i in Elems do
    yield i;
}


/* Element fields */

var elemBC: [Elems] int,

    e: [Elems] real, // energy
    p: [Elems] real, // pressure

    q: [Elems] real, // q
    ql: [Elems] real, // linear term for q
    qq: [Elems] real, // quadratic term for q

    v:    [Elems] real = 1.0, //relative volume
    vnew: [Elems] real,

    volo: [Elems] real, // reference volume
    delv: [Elems] real, // m_vnew - m_v
    vdov: [Elems] real, // volume derivative over volume

    arealg: [Elems] real, // elem characteristic length

    ss: [Elems] real, // "sound speed"

    elemMass: [Elems] real; // mass


/* Nodal fields */

var xd, yd, zd: [Nodes] real, // velocities

    xdd, ydd, zdd: [Nodes] real, // acceleration

    fx, fy, fz: [Nodes] atomic real, // forces

    nodalMass: [Nodes] real; // mass


/* Parameters */

var time = 0.0,          // current time
    deltatime = 1.0e-7,  // variable time increment
    dtcourant = 1.0e20,  // courant constraint
    dthydro = 1.0e20,    // volume change constraint

    cycle = 0;           // iteration count for simulation


proc main() {
  if debug then writeln("Lulesh -- Problem Size = ", numElems);

  initLulesh();

  var st: real;
  if doTiming then st = getCurrentTime();
  while (time < stoptime && cycle < maxcycles) {
    const iterTime = if showProgress then getCurrentTime() else 0.0;

    TimeIncrement();

    LagrangeLeapFrog();

    if debug {
      deprintatomic("[[ Forces ]]", fx, fy, fz);
      deprint("[[ Positions ]]", x, y, z);
      deprint("[[ p, e, q ]]", p, e, q);
    }
    if showProgress then
      writef("time = %er, dt=%er, %s", time, deltatime,
             if doTiming then ", elapsed = " + (getCurrentTime()-iterTime) +"\n"
                         else "\n");
  }
  if (cycle == maxcycles) {
    writeln("Stopped early due to reaching maxnumsteps");
  }
  if doTiming {
    const et = getCurrentTime();
    writeln("Total Time: ", et-st);
    writeln("Time/Cycle: ", (et-st)/cycle);
  }
  writeln("Number of cycles: ", cycle);

  if printCoords {
    var outfile = open("coords.out", iomode.cw);
    var writer = outfile.writer();
    var fmtstr = if debug then "%1.9re %1.9er %1.9er\n" 
                          else "%1.4er %1.4er %1.4er\n";
    for i in Nodes do
      writer.writef(fmtstr, x[i], y[i], z[i]);
    writer.close();
    outfile.close();
  }
}


/* Initialization functions */

proc initLulesh() {
  // initialize the coordinates
  initCoordinates(x,y,z);

  // initialize the element to node mapping
  initElemToNodeMapping(elemToNode);

  // initialize the greek symbols
  initGreekVars(lxim, lxip, letam, letap, lzetam, lzetap);

  // initialize the symmetry plane locations
  initXSyms(XSym);
  initYSyms(YSym);
  initZSyms(ZSym);

  /* embed hexehedral elements in nodal point lattice */
  //calculated on the fly using: elemToNodes(i: index(Elems)): index(Nodes)

  // initialize the masses
  initMasses();

  // initialize the boundary conditions
  const octantCorner = initBoundaryConditions();

  // deposit the energy for Sedov Problem
  e[octantCorner] = initialEnergy;
}


proc initMasses() {
  // This is a temporary array used to accumulate masses in parallel
  // without losing updates by using 'atomic' variables
  var massAccum: [Nodes] atomic real;

  forall eli in Elems {
    var x_local, y_local, z_local: 8*real;
    localizeNeighborNodes(eli, x, x_local, y, y_local, z, z_local);

    const volume = CalcElemVolume(x_local, y_local, z_local);
    volo[eli] = volume;
    elemMass[eli] = volume;

    for neighbor in elemToNodes[eli] do
      massAccum[neighbor].add(volume);
  }

  // When we're done, copy the accumulated masses into nodalMass, at
  // which point the massAccum array can go away (and will at the
  // procedure's return

  forall i in Nodes do
    nodalMass[i] = massAccum[i].read() / 8.0;

  if debug {
    writeln("ElemMass:");
    for mass in elemMass do writeln(mass);

    writeln("NodalMass:");
    for mass in nodalMass do writeln(mass);
  }
}


proc initBoundaryConditions() {
  var surfaceNode: [Nodes] int;

  forall n in XSym do
    surfaceNode[n] = 1;
  forall n in YSym do
    surfaceNode[n] = 1;
  forall n in ZSym do
    surfaceNode[n] = 1;

  forall e in Elems do {
    var mask: int;
    for i in 1..nodesPerElem do
      mask += surfaceNode[elemToNode[e][i]] << (i-1);

    // TODO: make an inlined function for this little idiom? (and below)

    if ((mask & 0x0f) == 0x0f) then elemBC[e] |= ZETA_M_SYMM;
    if ((mask & 0xf0) == 0xf0) then elemBC[e] |= ZETA_P_SYMM;
    if ((mask & 0x33) == 0x33) then elemBC[e] |= ETA_M_SYMM;
    if ((mask & 0xcc) == 0xcc) then elemBC[e] |= ETA_P_SYMM;
    if ((mask & 0x99) == 0x99) then elemBC[e] |= XI_M_SYMM;
    if ((mask & 0x66) == 0x66) then elemBC[e] |= XI_P_SYMM;
  }


  //
  // We find the octant corner by looking for the element with
  // all three SYMM flags set, which will have the largest
  // integral value.  Thus, we can use a maxloc to identify it.
  //
  const (check, loc) = maxloc reduce zip(elemBC, Elems);

  if debug then writeln("Found the octant corner at: ", loc);

  if (check != (XI_M_SYMM | ETA_M_SYMM | ZETA_M_SYMM)) then
    halt("maxloc got a value of ", check, " at loc ", loc);

  // TODO: This is an example of an array that, in a distributed
  // memory code, would typically be completely local and only storing
  // the local nodes owned by the locale -- noting that some nodes
  // are logically owned by multiple locales and therefore would 
  // redundantly be stored in both locales' surfaceNode arrays -- it's
  // essentially local scratchspace that does not need to be communicated
  // or kept coherent across locales.
  //

  surfaceNode = 0;

  /* the free surfaces */

  var freeSurface: sparse subdomain(Nodes);

  // initialize the free surface
  initFreeSurface(freeSurface);

  forall n in freeSurface do
    surfaceNode[n] = 1;

  forall e in Elems do {
    var mask: int;
    for i in 1..nodesPerElem do
      mask += surfaceNode[elemToNode[e][i]] << (i-1);

    if ((mask & 0x0f) == 0x0f) then elemBC[e] |= ZETA_M_FREE;
    if ((mask & 0xf0) == 0xf0) then elemBC[e] |= ZETA_P_FREE;
    if ((mask & 0x33) == 0x33) then elemBC[e] |= ETA_M_FREE;
    if ((mask & 0xcc) == 0xcc) then elemBC[e] |= ETA_P_FREE;
    if ((mask & 0x99) == 0x99) then elemBC[e] |= XI_M_FREE;
    if ((mask & 0x66) == 0x66) then elemBC[e] |= XI_P_FREE;
  }

  if debug {
    writeln("elemBC:");
    for b in elemBC do writeln(b);
  }

  return loc;
}


/* Helper functions */

inline proc localizeNeighborNodes(eli: index(Elems),
                                  x: [] real, ref x_local: 8*real,
                                  y: [] real, ref y_local: 8*real,
                                  z: [] real, ref z_local: 8*real) {

  for i in 1..nodesPerElem {
    const noi = elemToNode[eli][i];

    x_local[i] = x[noi];
    y_local[i] = y[noi];
    z_local[i] = z[noi];
  }
}

inline proc TripleProduct(x1, y1, z1, x2, y2, z2, x3, y3, z3) {
  return x1*(y2*z3 - z2*y3) + x2*(z1*y3 - y1*z3) + x3*(y1*z2 - z1*y2);
}


proc CalcElemVolume(x, y, z) {
  const dx61 = x[7] - x[2],
        dy61 = y[7] - y[2],
        dz61 = z[7] - z[2],

        dx70 = x[8] - x[1],
        dy70 = y[8] - y[1],
        dz70 = z[8] - z[1],

        dx63 = x[7] - x[4],
        dy63 = y[7] - y[4],
        dz63 = z[7] - z[4],

        dx20 = x[3] - x[1],
        dy20 = y[3] - y[1],
        dz20 = z[3] - z[1],

        dx50 = x[6] - x[1],
        dy50 = y[6] - y[1],
        dz50 = z[6] - z[1],

        dx64 = x[7] - x[5],
        dy64 = y[7] - y[5],
        dz64 = z[7] - z[5],

        dx31 = x[4] - x[2],
        dy31 = y[4] - y[2],
        dz31 = z[4] - z[2],

        dx72 = x[8] - x[3],
        dy72 = y[8] - y[3],
        dz72 = z[8] - z[3],

        dx43 = x[5] - x[4],
        dy43 = y[5] - y[4],
        dz43 = z[5] - z[4],

        dx57 = x[6] - x[8],
        dy57 = y[6] - y[8],
        dz57 = z[6] - z[8],

        dx14 = x[2] - x[5],
        dy14 = y[2] - y[5],
        dz14 = z[2] - z[5],

        dx25 = x[3] - x[6],
        dy25 = y[3] - y[6],
        dz25 = z[3] - z[6];

  const volume = TripleProduct(dx31 + dx72, dx63, dx20,
                               dy31 + dy72, dy63, dy20,
                               dz31 + dz72, dz63, dz20) +
                 TripleProduct(dx43 + dx57, dx64, dx70,
                               dy43 + dy57, dy64, dy70,
                               dz43 + dz57, dz64, dz70) +
                 TripleProduct(dx14 + dx25, dx61, dx50,
                               dy14 + dy25, dy61, dy50,
                               dz14 + dz25, dz61, dz50);

  return volume / 12.0;
}

proc InitStressTermsForElems(p, q, sigxx, sigyy, sigzz: [?D] real) {
  forall i in D {
    sigxx[i] = -p[i] - q[i];
    sigyy[i] = -p[i] - q[i];
    sigzz[i] = -p[i] - q[i];
  }
}


proc CalcElemShapeFunctionDerivatives(x: 8*real, y: 8*real, z: 8*real, 
                                      ref b_x: 8*real,
                                      ref b_y: 8*real,
                                      ref b_z: 8*real, 
                                      ref volume: real) {

  const fjxxi = .125 * ((x[7]-x[1]) + (x[6]-x[4]) - (x[8]-x[2]) - (x[5]-x[3])),
        fjxet = .125 * ((x[7]-x[1]) - (x[6]-x[4]) + (x[8]-x[2]) - (x[5]-x[3])),
        fjxze = .125 * ((x[7]-x[1]) + (x[6]-x[4]) + (x[8]-x[2]) + (x[5]-x[3])),

        fjyxi = .125 * ((y[7]-y[1]) + (y[6]-y[4]) - (y[8]-y[2]) - (y[5]-y[3])),
        fjyet = .125 * ((y[7]-y[1]) - (y[6]-y[4]) + (y[8]-y[2]) - (y[5]-y[3])),
        fjyze = .125 * ((y[7]-y[1]) + (y[6]-y[4]) + (y[8]-y[2]) + (y[5]-y[3])),

        fjzxi = .125 * ((z[7]-z[1]) + (z[6]-z[4]) - (z[8]-z[2]) - (z[5]-z[3])),
        fjzet = .125 * ((z[7]-z[1]) - (z[6]-z[4]) + (z[8]-z[2]) - (z[5]-z[3])),
        fjzze = .125 * ((z[7]-z[1]) + (z[6]-z[4]) + (z[8]-z[2]) + (z[5]-z[3]));

  /* compute cofactors */
  const cjxxi =    (fjyet * fjzze) - (fjzet * fjyze),
        cjxet =  - (fjyxi * fjzze) + (fjzxi * fjyze),
        cjxze =    (fjyxi * fjzet) - (fjzxi * fjyet),

        cjyxi =  - (fjxet * fjzze) + (fjzet * fjxze),
        cjyet =    (fjxxi * fjzze) - (fjzxi * fjxze),
        cjyze =  - (fjxxi * fjzet) + (fjzxi * fjxet),

        cjzxi =    (fjxet * fjyze) - (fjyet * fjxze),
        cjzet =  - (fjxxi * fjyze) + (fjyxi * fjxze),
        cjzze =    (fjxxi * fjyet) - (fjyxi * fjxet);

  /* calculate partials :
     this need only be done for l = 0,1,2,3   since , by symmetry ,
     (6,7,4,5) = - (0,1,2,3) .
  */
  b_x[1] =   -  cjxxi  -  cjxet  -  cjxze;
  b_x[2] =      cjxxi  -  cjxet  -  cjxze;
  b_x[3] =      cjxxi  +  cjxet  -  cjxze;
  b_x[4] =   -  cjxxi  +  cjxet  -  cjxze;
  b_x[5] = -b_x[3];
  b_x[6] = -b_x[4];
  b_x[7] = -b_x[1];
  b_x[8] = -b_x[2];

  b_y[1] =   -  cjyxi  -  cjyet  -  cjyze;
  b_y[2] =      cjyxi  -  cjyet  -  cjyze;
  b_y[3] =      cjyxi  +  cjyet  -  cjyze;
  b_y[4] =   -  cjyxi  +  cjyet  -  cjyze;
  b_y[5] = -b_y[3];
  b_y[6] = -b_y[4];
  b_y[7] = -b_y[1];
  b_y[8] = -b_y[2];

  b_z[1] =   -  cjzxi  -  cjzet  -  cjzze;
  b_z[2] =      cjzxi  -  cjzet  -  cjzze;
  b_z[3] =      cjzxi  +  cjzet  -  cjzze;
  b_z[4] =   -  cjzxi  +  cjzet  -  cjzze;
  b_z[5] = -b_z[3];
  b_z[6] = -b_z[4];
  b_z[7] = -b_z[1];
  b_z[8] = -b_z[2];

  /* calculate jacobian determinant (volume) */
  volume = 8.0 * ( fjxet * cjxet + fjyet * cjyet + fjzet * cjzet);
}


proc CalcElemNodeNormals(ref pfx: 8*real, ref pfy: 8*real, ref pfz: 8*real, 
                         x: 8*real, y: 8*real, z: 8*real) {

  proc ElemFaceNormal(param n1, param n2, param n3, param n4) {
    const bisectX0 = 0.5 * (x[n4] + x[n3] - x[n2] - x[n1]),
          bisectY0 = 0.5 * (y[n4] + y[n3] - y[n2] - y[n1]),
          bisectZ0 = 0.5 * (z[n4] + z[n3] - z[n2] - z[n1]),
          bisectX1 = 0.5 * (x[n3] + x[n2] - x[n4] - x[n1]),
          bisectY1 = 0.5 * (y[n3] + y[n2] - y[n4] - y[n1]),
          bisectZ1 = 0.5 * (z[n3] + z[n2] - z[n4] - z[n1]),
          areaX    = 0.25 * (bisectY0 * bisectZ1 - bisectZ0 * bisectY1),
          areaY    = 0.25 * (bisectZ0 * bisectX1 - bisectX0 * bisectZ1),
          areaZ    = 0.25 * (bisectX0 * bisectY1 - bisectY0 * bisectX1);

    var rx, ry, rz: 8*real; //results

    (rx[n1], rx[n2], rx[n3], rx[n4]) = (areaX, areaX, areaX, areaX);
    (ry[n1], ry[n2], ry[n3], ry[n4]) = (areaY, areaY, areaY, areaY);
    (rz[n1], rz[n2], rz[n3], rz[n4]) = (areaZ, areaZ, areaZ, areaZ);

    return (rx, ry, rz);
  }

  // calculate total normal from each face (faces are made up of
  // combinations of nodes)

  (pfx, pfy, pfz) = ElemFaceNormal(1,2,3,4) + ElemFaceNormal(1,5,6,2) +
                    ElemFaceNormal(2,6,7,3) + ElemFaceNormal(3,7,8,4) +
                    ElemFaceNormal(4,8,5,1) + ElemFaceNormal(5,8,7,6);
}


proc SumElemStressesToNodeForces(b_x: 8*real, b_y: 8*real, b_z: 8*real, 
                                 stress_xx:real,
                                 stress_yy:real,
                                 stress_zz: real, 
                                 ref fx: 8*real,
                                 ref fy: 8*real,
                                 ref fz: 8*real) {
  for i in 1..8 {
    fx[i] = -(stress_xx * b_x[i]);
    fy[i] = -(stress_yy * b_y[i]);
    fz[i] = -(stress_zz * b_z[i]);
  }
}

proc CalcElemVolumeDerivative(x: 8*real, y: 8*real, z: 8*real) {

  proc VoluDer(param n0, param n1, param n2, param n3, param n4, param n5) {
    const ox =   (y[n1] + y[n2]) * (z[n0] + z[n1]) 
               - (y[n0] + y[n1]) * (z[n1] + z[n2])
               + (y[n0] + y[n4]) * (z[n3] + z[n4]) 
               - (y[n3] + y[n4]) * (z[n0] + z[n4])
               - (y[n2] + y[n5]) * (z[n3] + z[n5])
               + (y[n3] + y[n5]) * (z[n2] + z[n5]),
          oy = - (x[n1] + x[n2]) * (z[n0] + z[n1])
               + (x[n0] + x[n1]) * (z[n1] + z[n2])
               - (x[n0] + x[n4]) * (z[n3] + z[n4])
               + (x[n3] + x[n4]) * (z[n0] + z[n4])
               + (x[n2] + x[n5]) * (z[n3] + z[n5])
               - (x[n3] + x[n5]) * (z[n2] + z[n5]),
          oz = - (y[n1] + y[n2]) * (x[n0] + x[n1])
               + (y[n0] + y[n1]) * (x[n1] + x[n2])
               - (y[n0] + y[n4]) * (x[n3] + x[n4])
               + (y[n3] + y[n4]) * (x[n0] + x[n4])
               + (y[n2] + y[n5]) * (x[n3] + x[n5])
               - (y[n3] + y[n5]) * (x[n2] + x[n5]);

    return (ox/12.0, oy/12.0, oz/12.0);
  }

  var dvdx, dvdy, dvdz: 8*real;

  (dvdx[1], dvdy[1], dvdz[1]) = VoluDer(2,3,4,5,6,8);
  (dvdx[4], dvdy[4], dvdz[4]) = VoluDer(1,2,3,8,5,7);
  (dvdx[3], dvdy[3], dvdz[3]) = VoluDer(4,1,2,7,8,6);
  (dvdx[2], dvdy[2], dvdz[2]) = VoluDer(3,4,1,6,7,5);
  (dvdx[5], dvdy[5], dvdz[5]) = VoluDer(8,7,6,1,4,2);
  (dvdx[6], dvdy[6], dvdz[6]) = VoluDer(5,8,7,2,1,3);
  (dvdx[7], dvdy[7], dvdz[7]) = VoluDer(6,5,8,3,2,4);
  (dvdx[8], dvdy[8], dvdz[8]) = VoluDer(7,6,5,4,3,1);

  return (dvdx, dvdy, dvdz);
}

inline proc CalcElemFBHourglassForce(xd: 8*real, yd: 8*real, zd: 8*real,
                                     hourgam: 8*(4*real),
                                     coefficient: real,
                                     ref hgfx: 8*real,
                                     ref hgfy: 8*real,
                                     ref hgfz: 8*real) {
  var hx, hy, hz: 4*real;

  // reduction
  for i in 1..4 {
    for j in 1..8 {
      hx[i] += hourgam[j][i] * xd[j];
      hy[i] += hourgam[j][i] * yd[j];
      hz[i] += hourgam[j][i] * zd[j];
    }
  }

  for i in 1..8 {
    var shx, shy, shz: real;
    for j in 1..4 {
      shx += hourgam[i][j] * hx[j];
      shy += hourgam[i][j] * hy[j];
      shz += hourgam[i][j] * hz[j];
    }
    hgfx[i] = coefficient * shx;
    hgfy[i] = coefficient * shy;
    hgfz[i] = coefficient * shz;
  }
}


proc CalcElemCharacteristicLength(x, y, z, volume) {
  proc AreaFace(param p0, param p1, param p2, param p3) {
    const fx = (x[p2] - x[p0]) - (x[p3] - x[p1]),
          fy = (y[p2] - y[p0]) - (y[p3] - y[p1]),
          fz = (z[p2] - z[p0]) - (z[p3] - z[p1]),
          gx = (x[p2] - x[p0]) + (x[p3] - x[p1]),
          gy = (y[p2] - y[p0]) + (y[p3] - y[p1]),
          gz = (z[p2] - z[p0]) + (z[p3] - z[p1]),
          area = (fx * fx + fy * fy + fz * fz) *
                 (gx * gx + gy * gy + gz * gz) -
                 (fx * gx + fy * gy + fz * gz) *
                 (fx * gx + fy * gy + fz * gz);

    return area ;
  }

  const charLength = max(AreaFace(1, 2, 3, 4),
                         AreaFace(5, 6, 7, 8),
                         AreaFace(1, 2, 6, 5),
                         AreaFace(2, 3, 7, 6),
                         AreaFace(3, 4, 8, 7),
                         AreaFace(4, 1, 5, 8));

  return 4.0 * volume / sqrt(charLength);
}


proc CalcElemVelocityGradient(xvel, yvel, zvel, pfx,  pfy, pfz,
                              detJ, ref d: 6*real) {
  const inv_detJ = 1.0 / detJ;

  d[1] = inv_detJ * ( pfx[1] * (xvel[1]-xvel[7])
                    + pfx[2] * (xvel[2]-xvel[8])
                    + pfx[3] * (xvel[3]-xvel[5])
                    + pfx[4] * (xvel[4]-xvel[6]) );
  d[2] = inv_detJ * ( pfy[1] * (yvel[1]-yvel[7])
                    + pfy[2] * (yvel[2]-yvel[8])
                    + pfy[3] * (yvel[3]-yvel[5])
                    + pfy[4] * (yvel[4]-yvel[6]) );
  d[3] = inv_detJ * ( pfz[1] * (zvel[1]-zvel[7])
                    + pfz[2] * (zvel[2]-zvel[8])
                    + pfz[3] * (zvel[3]-zvel[5])
                    + pfz[4] * (zvel[4]-zvel[6]) );

  const dyddx  = inv_detJ * ( pfx[1] * (yvel[1]-yvel[7])
                            + pfx[2] * (yvel[2]-yvel[8])
                            + pfx[3] * (yvel[3]-yvel[5])
                            + pfx[4] * (yvel[4]-yvel[6]) ),

        dxddy  = inv_detJ * ( pfy[1] * (xvel[1]-xvel[7])
                            + pfy[2] * (xvel[2]-xvel[8])
                            + pfy[3] * (xvel[3]-xvel[5])
                            + pfy[4] * (xvel[4]-xvel[6]) ),

        dzddx  = inv_detJ * ( pfx[1] * (zvel[1]-zvel[7])
                            + pfx[2] * (zvel[2]-zvel[8])
                            + pfx[3] * (zvel[3]-zvel[5])
                            + pfx[4] * (zvel[4]-zvel[6]) ),

        dxddz  = inv_detJ * ( pfz[1] * (xvel[1]-xvel[7])
                            + pfz[2] * (xvel[2]-xvel[8])
                            + pfz[3] * (xvel[3]-xvel[5])
                            + pfz[4] * (xvel[4]-xvel[6]) ),

        dzddy  = inv_detJ * ( pfy[1] * (zvel[1]-zvel[7])
                            + pfy[2] * (zvel[2]-zvel[8])
                            + pfy[3] * (zvel[3]-zvel[5])
                            + pfy[4] * (zvel[4]-zvel[6]) ),

        dyddz  = inv_detJ * ( pfz[1] * (yvel[1]-yvel[7])
                            + pfz[2] * (yvel[2]-yvel[8])
                            + pfz[3] * (yvel[3]-yvel[5])
                            + pfz[4] * (yvel[4]-yvel[6]) );

  d[6]  = 0.5 * ( dxddy + dyddx );
  d[5]  = 0.5 * ( dxddz + dzddx );
  d[4]  = 0.5 * ( dzddy + dyddz );
}


proc CalcPressureForElems(p_new: [?D] real, bvc, pbvc, 
                          e_old, compression, vnewc,
                          pmin: real, p_cut: real, eosvmax: real) {

  //
  // TODO: Uncomment local once sparse domain is distributed
  //
  forall i in D /* do local */ {
    const c1s = 2.0 / 3.0;
    bvc[i] = c1s * (compression[i] + 1.0);
    pbvc[i] = c1s;
  }

  forall i in D {
    p_new[i] = bvc[i] * e_old[i];

    if abs(p_new[i]) < p_cut then p_new[i] = 0.0;
    if vnewc[i] >= eosvmax then p_new[i] = 0.0; //impossible?
    if p_new[i] < pmin then p_new[i] = pmin;
  }
}


proc TimeIncrement() {
  var targetdt = stoptime - time;

  if dtfixed <= 0.0 && cycle != 0 { //don't do this the first cycle
    var olddt = deltatime,
        newdt = 1.0e20;

    if dtcourant < newdt then newdt = dtcourant / 2.0;
    if dthydro < newdt then   newdt = 2.0/3.0 * dthydro;

    const ratio = newdt / olddt;
    if ratio >= 1.0 {
      if ratio < deltatimemultlb then      newdt = olddt;
      else if ratio > deltatimemultub then newdt = olddt * deltatimemultub;
    }
    if newdt > dtmax then newdt = dtmax;

    deltatime = newdt;
  }

  /* TRY TO PREVENT VERY SMALL SCALING ON THE NEXT CYCLE */
  if targetdt > deltatime && targetdt < (4.0/3.0 * deltatime) {
    targetdt = 2.0/3.0 * deltatime;
  }
  if targetdt < deltatime then deltatime = targetdt;

  time += deltatime;
  cycle += 1;
}

inline proc LagrangeLeapFrog() {
  /* calculate nodal forces, accelerations, velocities, positions, with
   * applied boundary conditions and slide surface considerations */
  LagrangeNodal();

  /* calculate element quantities (i.e. velocity gradient & q), and update
   * material states */
  LagrangeElements();

  CalcTimeConstraintsForElems();
}


inline proc LagrangeNodal() {
  CalcForceForNodes();

  CalcAccelerationForNodes();

  ApplyAccelerationBoundaryConditionsForNodes();

  CalcVelocityForNodes(deltatime, u_cut);

  CalcPositionForNodes(deltatime);
}


inline proc LagrangeElements() {
  CalcLagrangeElements();

  /* Calculate Q.  (Monotonic q option requires communication) */
  CalcQForElems();

  ApplyMaterialPropertiesForElems();

  UpdateVolumesForElems();
}


inline proc CalcTimeConstraintsForElems() {
  /* evaluate time constraint */
  CalcCourantConstraintForElems();

  /* check hydro constraint */
  CalcHydroConstraintForElems();
}


inline proc computeDTF(indx) {
  const myvdov = vdov[indx];

  if myvdov == 0.0 then
    return max(real);

  const myarealg = arealg[indx];
  var dtf = ss[indx]**2;
  if myvdov < 0.0 then
    dtf += qqc2 * myarealg**2 * myvdov**2;
  dtf = sqrt(dtf);
  dtf = myarealg / dtf;

  return dtf;
}


proc CalcCourantConstraintForElems() {
  const val = min reduce [indx in MatElems] computeDTF(indx);

  if (val != max(real)) then
    dtcourant = val;
}


inline proc calcDtHydroTmp(indx) {
  const myvdov = vdov[indx];
  if (myvdov == 0.0) then
    return max(real);
  else
    return dvovmax / (abs(myvdov)+1.0e-20);
}


proc CalcHydroConstraintForElems() {
  const val = min reduce [indx in MatElems] calcDtHydroTmp(indx);

  if (val != max(real)) then
    dthydro = val;
}


/* calculate nodal forces, accelerations, velocities, positions, with
 * applied boundary conditions and slide surface considerations */

proc CalcForceForNodes() {
  //zero out all forces
  forall x in fx do x.write(0);
  forall y in fy do y.write(0);
  forall z in fz do z.write(0);

  /* Calcforce calls partial, force, hourq */
  CalcVolumeForceForElems();

  /* Calculate Nodal Forces at domain boundaries */
  // this was commented out in C++ code, so left out here
}

proc CalcVolumeForceForElems() {
  var sigxx, sigyy, sigzz, determ: [Elems] real;

  /* Sum contributions to total stress tensor */
  InitStressTermsForElems(p, q, sigxx, sigyy, sigzz);

  /* call elemlib stress integration loop to produce nodal forces from
     material stresses. */
  IntegrateStressForElems(sigxx, sigyy, sigzz, determ);

  /* check for negative element volume */
  forall e in Elems {
    if determ[e] <= 0.0 then
      halt("can't have negative volume (determ[", e, "]=", determ[e], ")");
  }

  CalcHourglassControlForElems(determ);
}


proc IntegrateStressForElems(sigxx, sigyy, sigzz, determ) {
  forall k in Elems {
    var b_x, b_y, b_z: 8*real;
    var x_local, y_local, z_local: 8*real;
    localizeNeighborNodes(k, x, x_local, y, y_local, z, z_local);

    var fx_local, fy_local, fz_local: 8*real;

    local {
      /* Volume calculation involves extra work for numerical consistency. */
      CalcElemShapeFunctionDerivatives(x_local, y_local, z_local, 
                                       b_x, b_y, b_z, determ[k]);
    
      CalcElemNodeNormals(b_x, b_y, b_z, x_local, y_local, z_local);

      SumElemStressesToNodeForces(b_x, b_y, b_z, sigxx[k], sigyy[k], sigzz[k], 
                                  fx_local, fy_local, fz_local);
    }

    for (noi, t) in elemToNodesTuple(k) {
      fx[noi].add(fx_local[t]);
      fy[noi].add(fy_local[t]);
      fz[noi].add(fz_local[t]);
    }
  }
}


proc CalcHourglassControlForElems(determ) {
  var dvdx, dvdy, dvdz, x8n, y8n, z8n: [Elems] 8*real;

  forall eli in Elems {
    /* Collect domain nodes to elem nodes */
    var x1, y1, z1: 8*real;
    localizeNeighborNodes(eli, x, x1, y, y1, z, z1);
    var pfx, pfy, pfz: 8*real;

    local {
      /* load into temporary storage for FB Hour Glass control */
      (dvdx[eli], dvdy[eli], dvdz[eli]) = CalcElemVolumeDerivative(x1, y1, z1);
    }

    x8n[eli]  = x1;
    y8n[eli]  = y1;
    z8n[eli]  = z1;

    determ[eli] = volo[eli] * v[eli];
  }

  /* Do a check for negative volumes */
  forall e in Elems {
    if v[e] <= 0.0 then
      halt("can't have negative (or zero) volume. (in Hourglass, v[", e, "]=", v[e], ")");
  }

  if hgcoef > 0.0 {
    CalcFBHourglassForceForElems(determ, x8n, y8n, z8n, dvdx, dvdy, dvdz);
  }
}


const gammaCoef: 4*(8*real) = // WAS: [1..4, 1..8] real = 
                (( 1.0,  1.0, -1.0, -1.0, -1.0, -1.0,  1.0,  1.0),
                 ( 1.0, -1.0, -1.0,  1.0, -1.0,  1.0,  1.0, -1.0),
                 ( 1.0, -1.0,  1.0, -1.0,  1.0, -1.0,  1.0, -1.0),
                 (-1.0,  1.0, -1.0,  1.0,  1.0, -1.0,  1.0, -1.0));

/* Calculates the Flanagan-Belytschko anti-hourglass force. */
proc CalcFBHourglassForceForElems(determ, x8n, y8n, z8n, dvdx, dvdy, dvdz) {

  /* compute the hourglass modes */
  forall eli in Elems {
    var hourgam: 8*(4*real);
    const volinv = 1.0 / determ[eli];
    var ss1, mass1, volume13: real;
    var hgfx, hgfy, hgfz: 8*real;
    var coefficient: real;

    var xd1, yd1, zd1: 8*real;
    localizeNeighborNodes(eli, xd, xd1, yd, yd1, zd, zd1);

    /* TODO: Can we enable this local block? */
    // local {
      for i in 1..4 {
        var hourmodx, hourmody, hourmodz: real;
        // reduction
        for j in 1..8 {
          hourmodx += x8n[eli][j] * gammaCoef[i][j];
          hourmody += y8n[eli][j] * gammaCoef[i][j];
          hourmodz += z8n[eli][j] * gammaCoef[i][j];
        }

        for j in 1..8 {
          hourgam[j][i] = gammaCoef[i][j] - volinv * 
            (dvdx[eli][j] * hourmodx +
             dvdy[eli][j] * hourmody +
             dvdz[eli][j] * hourmodz);
        }
      }

      /* compute forces */
      /* store forces into h arrays (force arrays) */
      ss1 = ss[eli];
      mass1 = elemMass[eli];
      volume13 = cbrt(determ[eli]);

      coefficient = - hgcoef * 0.01 * ss1 * mass1 / volume13;

      CalcElemFBHourglassForce(xd1, yd1, zd1, hourgam, coefficient,
                               hgfx, hgfy, hgfz);
      // } // end local

    for (noi,i) in elemToNodesTuple(eli) {
      fx[noi].add(hgfx[i]);
      fy[noi].add(hgfy[i]);
      fz[noi].add(hgfz[i]);
    }
  }
}


proc CalcAccelerationForNodes() {
  forall noi in Nodes do local {
      xdd[noi] = fx[noi].read() / nodalMass[noi];
      ydd[noi] = fy[noi].read() / nodalMass[noi];
      zdd[noi] = fz[noi].read() / nodalMass[noi];
    }
}


proc ApplyAccelerationBoundaryConditionsForNodes() {
  // TODO: Shouldn't we be able to write these as follows?
  //
  // xdd[XSym] = 0.0;
  // ydd[YSym] = 0.0;
  // zdd[ZSym] = 0.0;
  
  forall x in XSym do xdd[x] = 0.0;
  forall y in YSym do ydd[y] = 0.0;
  forall z in ZSym do zdd[z] = 0.0;
}


proc CalcVelocityForNodes(dt: real, u_cut: real) {
  forall i in Nodes do local {
    var xdtmp = xd[i] + xdd[i] * dt,
        ydtmp = yd[i] + ydd[i] * dt,
        zdtmp = zd[i] + zdd[i] * dt;
    if abs(xdtmp) < u_cut then xdtmp = 0.0;
    if abs(ydtmp) < u_cut then ydtmp = 0.0;
    if abs(zdtmp) < u_cut then zdtmp = 0.0;
    xd[i] = xdtmp;
    yd[i] = ydtmp;
    zd[i] = zdtmp;
  }
}


proc CalcPositionForNodes(dt: real) {
  forall ijk in Nodes {
    x[ijk] += xd[ijk] * dt;
    y[ijk] += yd[ijk] * dt;
    z[ijk] += zd[ijk] * dt;
  }
}

// sungeun: Temporary array reused throughout
proc CalcLagrangeElements() {
  var dxx, dyy, dzz: [Elems] real;

  CalcKinematicsForElems(dxx, dyy, dzz, deltatime);

  // element loop to do some stuff not included in the elemlib function.
  forall k in Elems do local {
    vdov[k] = dxx[k] + dyy[k] + dzz[k];
    var vdovthird = vdov[k] / 3.0;
    dxx[k] -= vdovthird;
    dyy[k] -= vdovthird;
    dzz[k] -= vdovthird;
  }

  // See if any volumes are negative, and take appropriate action.
  forall e in Elems {
    if vnew[e] <= 0.0 then
      halt("can't have negative volume (vnew[", e, "]=", vnew[e], ")");
  }
}


proc CalcKinematicsForElems(dxx, dyy, dzz, const dt: real) {
  // loop over all elements
  forall k in Elems {
    var b_x, b_y, b_z: 8*real,
        d: 6*real,
        detJ: real;

    //get nodal coordinates from global arrays and copy into local arrays
    var x_local, y_local, z_local: 8*real;
    localizeNeighborNodes(k, x, x_local, y, y_local, z, z_local);

    //get nodal velocities from global arrays and copy into local arrays
    var xd_local, yd_local, zd_local: 8*real;
    localizeNeighborNodes(k, xd, xd_local, yd, yd_local, zd, zd_local);
    var dt2 = 0.5 * dt; //wish this was local, too...

    local {
      //volume calculations
      const volume = CalcElemVolume(x_local, y_local, z_local);
      const relativeVolume = volume / volo[k];
      vnew[k] = relativeVolume;
      delv[k] = relativeVolume - v[k];

      //set characteristic length
      arealg[k] = CalcElemCharacteristicLength(x_local, y_local, z_local,
                                               volume);

      for i in 1..8 {
        x_local[i] -= dt2 * xd_local[i];
        y_local[i] -= dt2 * yd_local[i];
        z_local[i] -= dt2 * zd_local[i];
      }

      CalcElemShapeFunctionDerivatives(x_local, y_local, z_local,
                                       b_x, b_y, b_z, detJ);

      CalcElemVelocityGradient(xd_local, yd_local, zd_local, b_x, b_y, b_z,
                               detJ, d);

    }

    // put velocity gradient quantities into their global arrays.
    dxx[k] = d[1];
    dyy[k] = d[2];
    dzz[k] = d[3];
  }
}


// sungeun: Temporary array reused throughout
/* velocity gradient */
var delv_xi, delv_eta, delv_zeta: [Elems] real;
/* position gradient */
var delx_xi, delx_eta, delx_zeta: [Elems] real;

proc CalcQForElems() {
  // MONOTONIC Q option

  /* Calculate velocity gradients */
  CalcMonotonicQGradientsForElems(delv_xi, delv_eta, delv_zeta, 
                                  delx_xi, delx_eta, delx_zeta);

  /* Transfer veloctiy gradients in the first order elements */
  /* problem->commElements->Transfer(CommElements::monoQ) ; */
  CalcMonotonicQForElems(delv_xi, delv_eta, delv_zeta,
                         delx_xi, delx_eta, delx_zeta);

  /* Don't allow excessive artificial viscosity */
  forall e in Elems {
    if q[e] > qstop then
      halt("Excessive artificial viscosity!  (q[", e, "]=", q[e], ")");
  }
}


// sungeun: Temporary array reused throughout
var vnewc: [MatElems] real;

/* Expose all of the variables needed for material evaluation */
proc ApplyMaterialPropertiesForElems() {

  forall i in MatElems do vnewc[i] = vnew[i];

  if eosvmin != 0.0 then
    [c in vnewc] if c < eosvmin then c = eosvmin;

  if eosvmax != 0.0 then
    [c in vnewc] if c > eosvmax then c = eosvmax;


  // old comment: The following loop should compute min/max reductions;
  // currently, race-y

  forall matelm in MatElems {
    var vc = v[matelm];
    if eosvmin != 0.0 && vc < eosvmin then vc = eosvmin;
    if eosvmax != 0.0 && vc > eosvmax then vc = eosvmax;
    if vc <= 0.0 {
      writeln("Volume error (in ApplyMaterialProperiesForElems).");
      exit(1);
    }
  }

  EvalEOSForElems(vnewc);
}


proc UpdateVolumesForElems() {
  forall i in Elems do local {
    var tmpV = vnew[i];
    if abs(tmpV-1.0) < v_cut then tmpV = 1.0;
    v[i] = tmpV;
  }
}


proc CalcMonotonicQGradientsForElems(delv_xi, delv_eta, delv_zeta, 
                                     delx_xi, delx_eta, delx_zeta) {
  forall eli in Elems {
    const ptiny = 1.0e-36;
    var xl, yl, zl: 8*real;
    localizeNeighborNodes(eli, x, xl, y, yl, z, zl);
    var xvl, yvl, zvl: 8*real;
    localizeNeighborNodes(eli, xd, xvl, yd, yvl, zd, zvl);

    local {
      const vol = volo[eli] * vnew[eli],
            norm = 1.0 / (vol + ptiny);
      var ax, ay, az, dxv, dyv, dzv: real;

      const dxj = -0.25*((xl[1]+xl[2]+xl[6]+xl[5])-(xl[4]+xl[3]+xl[7]+xl[8])),
            dyj = -0.25*((yl[1]+yl[2]+yl[6]+yl[5])-(yl[4]+yl[3]+yl[7]+yl[8])),
            dzj = -0.25*((zl[1]+zl[2]+zl[6]+zl[5])-(zl[4]+zl[3]+zl[7]+zl[8])),
      
            dxi =  0.25*((xl[2]+xl[3]+xl[7]+xl[6])-(xl[1]+xl[4]+xl[8]+xl[5])),
            dyi =  0.25*((yl[2]+yl[3]+yl[7]+yl[6])-(yl[1]+yl[4]+yl[8]+yl[5])),
            dzi =  0.25*((zl[2]+zl[3]+zl[7]+zl[6])-(zl[1]+zl[4]+zl[8]+zl[5])),
        
            dxk =  0.25*((xl[5]+xl[6]+xl[7]+xl[8])-(xl[1]+xl[2]+xl[3]+xl[4])),
            dyk =  0.25*((yl[5]+yl[6]+yl[7]+yl[8])-(yl[1]+yl[2]+yl[3]+yl[4])),
            dzk =  0.25*((zl[5]+zl[6]+zl[7]+zl[8])-(zl[1]+zl[2]+zl[3]+zl[4]));

      /* find delvk and delxk ( i cross j ) */

      ax = dyi*dzj - dzi*dyj;
      ay = dzi*dxj - dxi*dzj;
      az = dxi*dyj - dyi*dxj;

      delx_zeta[eli] = vol / sqrt(ax*ax + ay*ay + az*az + ptiny);

      ax *= norm;
      ay *= norm;
      az *= norm;

      dxv = 0.25*((xvl[5]+xvl[6]+xvl[7]+xvl[8])-(xvl[1]+xvl[2]+xvl[3]+xvl[4]));
      dyv = 0.25*((yvl[5]+yvl[6]+yvl[7]+yvl[8])-(yvl[1]+yvl[2]+yvl[3]+yvl[4]));
      dzv = 0.25*((zvl[5]+zvl[6]+zvl[7]+zvl[8])-(zvl[1]+zvl[2]+zvl[3]+zvl[4]));

      delv_zeta[eli] = ax*dxv + ay*dyv + az*dzv;

      /* find delxi and delvi ( j cross k ) */

      ax = dyj*dzk - dzj*dyk;
      ay = dzj*dxk - dxj*dzk;
      az = dxj*dyk - dyj*dxk;

      delx_xi[eli] = vol / sqrt(ax*ax + ay*ay + az*az + ptiny) ;

      ax *= norm;
      ay *= norm;
      az *= norm;

      dxv = 0.25*((xvl[2]+xvl[3]+xvl[7]+xvl[6])-(xvl[1]+xvl[4]+xvl[8]+xvl[5]));
      dyv = 0.25*((yvl[2]+yvl[3]+yvl[7]+yvl[6])-(yvl[1]+yvl[4]+yvl[8]+yvl[5]));
      dzv = 0.25*((zvl[2]+zvl[3]+zvl[7]+zvl[6])-(zvl[1]+zvl[4]+zvl[8]+zvl[5]));

      delv_xi[eli] = ax*dxv + ay*dyv + az*dzv ;

      /* find delxj and delvj ( k cross i ) */

      ax = dyk*dzi - dzk*dyi;
      ay = dzk*dxi - dxk*dzi;
      az = dxk*dyi - dyk*dxi;

      delx_eta[eli] = vol / sqrt(ax*ax + ay*ay + az*az + ptiny) ;

      ax *= norm;
      ay *= norm;
      az *= norm;

      dxv= -0.25*((xvl[1]+xvl[2]+xvl[6]+xvl[5])-(xvl[4]+xvl[3]+xvl[7]+xvl[8]));
      dyv= -0.25*((yvl[1]+yvl[2]+yvl[6]+yvl[5])-(yvl[4]+yvl[3]+yvl[7]+yvl[8]));
      dzv= -0.25*((zvl[1]+zvl[2]+zvl[6]+zvl[5])-(zvl[4]+zvl[3]+zvl[7]+zvl[8]));

      delv_eta[eli] = ax*dxv + ay*dyv + az*dzv ;
    } /* local */
  } /* forall eli */
}


proc CalcMonotonicQForElems(delv_xi, delv_eta, delv_zeta, 
                            delx_xi, delx_eta, delx_zeta) {
  //got rid of call through to "CalcMonotonicQRegionForElems"

  forall i in MatElems {
    const ptiny = 1.0e-36;
    const bcMask = elemBC[i];
    var norm, delvm, delvp: real;

    /* phixi */
    norm = 1.0 / (delv_xi[i] + ptiny);

    select bcMask & XI_M {
      when 0         do delvm = delv_xi[lxim(i)];
      when XI_M_SYMM do delvm = delv_xi[i];
      when XI_M_FREE do delvm = 0.0;
    }
    select bcMask & XI_P {
      when 0         do delvp = delv_xi[lxip(i)];
      when XI_P_SYMM do delvp = delv_xi[i];
      when XI_P_FREE do delvp = 0.0;
    }

    delvm *= norm;
    delvp *= norm;

    var phixi = 0.5 * (delvm + delvp);

    delvm *= monoq_limiter_mult;
    delvp *= monoq_limiter_mult;

    if delvm < phixi           then phixi = delvm;
    if delvp < phixi           then phixi = delvp;
    if phixi < 0               then phixi = 0.0;
    if phixi > monoq_max_slope then phixi = monoq_max_slope;

    /* phieta */
    norm = 1.0 / (delv_eta[i] + ptiny);

    select bcMask & ETA_M {
      when 0          do delvm = delv_eta[letam[i]];
      when ETA_M_SYMM do delvm = delv_eta[i];      
      when ETA_M_FREE do delvm = 0.0;      
    }
    select bcMask & ETA_P {
      when 0          do delvp = delv_eta[letap[i]];
      when ETA_P_SYMM do delvp = delv_eta[i];      
      when ETA_P_FREE do delvp = 0.0;      
    }

    delvm = delvm * norm;
    delvp = delvp * norm;

    var phieta = 0.5 * (delvm + delvp);

    delvm *= monoq_limiter_mult;
    delvp *= monoq_limiter_mult;

    if delvm  < phieta          then phieta = delvm;
    if delvp  < phieta          then phieta = delvp;
    if phieta < 0.0             then phieta = 0.0;
    if phieta > monoq_max_slope then phieta = monoq_max_slope;

    /*  phizeta     */
    norm = 1.0 / (delv_zeta[i] + ptiny);

    select bcMask & ZETA_M {
      when 0           do delvm = delv_zeta[lzetam[i]];
      when ZETA_M_SYMM do delvm = delv_zeta[i];       
      when ZETA_M_FREE do delvm = 0.0;        
    }
    select bcMask & ZETA_P {
      when 0           do delvp = delv_zeta[lzetap[i]];
      when ZETA_P_SYMM do delvp = delv_zeta[i];       
      when ZETA_P_FREE do delvp = 0.0;        
    }

    delvm = delvm * norm;
    delvp = delvp * norm;

    var phizeta = 0.5 * (delvm + delvp);

    delvm *= monoq_limiter_mult;
    delvp *= monoq_limiter_mult;

    if delvm   < phizeta          then phizeta = delvm;
    if delvp   < phizeta         then phizeta = delvp;
    if phizeta < 0.0                 then phizeta = 0.0;
    if phizeta > monoq_max_slope then phizeta = monoq_max_slope;

    /* Remove length scale */
    var qlin, qquad: real;
    if vdov[i] > 0.0 {
      qlin  = 0.0;
      qquad = 0.0;
    } else {
      var delvxxi   = delv_xi[i]   * delx_xi[i],
          delvxeta  = delv_eta[i]  * delx_eta[i],
          delvxzeta = delv_zeta[i] * delx_zeta[i];

      if delvxxi   > 0.0 then delvxxi   = 0.0;
      if delvxeta  > 0.0 then delvxeta  = 0.0;
      if delvxzeta > 0.0 then delvxzeta = 0.0;

      const rho = elemMass[i] / (volo[i] * vnew[i]);

      qlin = -qlc_monoq * rho *
        ( delvxxi   * (1.0 - phixi) +
          delvxeta  * (1.0 - phieta) +
          delvxzeta * (1.0 - phizeta));

      qquad = qqc_monoq * rho *
        ( delvxxi**2   * (1.0 - phixi**2) +
          delvxeta**2  * (1.0 - phieta**2) +
          delvxzeta**2 * (1.0 - phizeta**2));
    }
    qq[i] = qquad;
    ql[i] = qlin;

  }
}


proc EvalEOSForElems(vnewc) {
  const rho0 = refdens;

  var e_old, delvc, p_old, q_old, compression, compHalfStep, 
    qq_old, ql_old, work, p_new, e_new, q_new, bvc, pbvc: [Elems] real;

  /* compress data, minimal set */
  forall i in MatElems {
    e_old[i]  = e[i];
    delvc[i]  = delv[i];
    p_old[i]  = p[i];
    q_old[i]  = q[i];
    qq_old[i] = qq[i];
    ql_old[i] = ql[i];
  }

  //
  // TODO: Uncomment local once sparse domain is distributed
  //
  forall i in Elems /* do local */ {
    compression[i] = 1.0 / vnewc[i] - 1.0;
    const vchalf = vnewc[i] - delvc[i] * 0.5;
    compHalfStep[i] = 1.0 / vchalf - 1.0;
  }

  /* Check for v > eosvmax or v < eosvmin */
  // (note: I think this was already checked for in calling function!)
  if eosvmin != 0.0 {
    forall i in Elems {
      if vnewc[i] <= eosvmin then compHalfStep[i] = compression[i];
    }
  }
  if eosvmax != 0.0 {
    forall i in Elems {
      if vnewc[i] >= eosvmax {
        p_old[i] = 0.0;
        compression[i] = 0.0;
        compHalfStep[i] = 0.0;
      }
    }
  }

  CalcEnergyForElems(p_new, e_new, q_new, bvc, pbvc, 
                     p_old, e_old, q_old, compression, compHalfStep, 
                     vnewc, work, delvc, qq_old, ql_old);

  forall i in MatElems {
    p[i] = p_new[i];
    e[i] = e_new[i];
    q[i] = q_new[i];
  }

  CalcSoundSpeedForElems(vnewc, rho0, e_new, p_new, pbvc, bvc);
}


proc CalcEnergyForElems(p_new, e_new, q_new, bvc, pbvc,
                        p_old, e_old, q_old, compression, compHalfStep, 
                        vnewc, work, delvc, qq_old, ql_old) {
  // TODO: might need to move these consts into foralls or global
  // Otherwise, they live on Locale0 and everyone else has to do 
  // remote reads.  OR: Check if they're remote value forwarded.
  const rho0 = refdens; 
  const sixth = 1.0 / 6.0;

  var pHalfStep: [MatElems] real;

  forall i in Elems {
    e_new[i] = e_old[i] - 0.5 * delvc[i] * (p_old[i] + q_old[i]) 
                        + 0.5 * work[i];
    if e_new[i] < emin then e_new[i] = emin;
  }

  CalcPressureForElems(pHalfStep, bvc, pbvc, e_new, compHalfStep,
                       vnewc, pmin, p_cut, eosvmax);

  forall i in Elems {
    const vhalf = 1.0 / (1.0 + compHalfStep[i]);

    if delvc[i] > 0.0 {
      q_new[i] = 0.0;
    } else {
      var ssc = (pbvc[i] * e_new[i] + vhalf**2 * bvc[i] * pHalfStep[i]) / rho0;
      if ssc <= 0.0 then ssc = 0.333333e-36;
      else ssc = sqrt(ssc);
      q_new[i] = ssc * ql_old[i] + qq_old[i];
    }

    e_new[i] += 0.5 * delvc[i]
      * (3.0*(p_old[i] + q_old[i]) - 4.0*(pHalfStep[i] + q_new[i]));
  }
  forall i in Elems {
    e_new[i] += 0.5 * work[i];
    if abs(e_new[i] < e_cut) then e_new[i] = 0.0;
    if e_new[i] < emin then e_new[i] = emin;
  }

  CalcPressureForElems(p_new, bvc, pbvc, e_new, compression, vnewc, pmin,
                       p_cut, eosvmax);

  forall i in Elems {
    var q_tilde:real;

    if delvc[i] > 0.0 {
      q_tilde = 0.0;
    } else {
      var ssc = (pbvc[i] * e_new[i] + vnewc[i]**2 * bvc[i] * p_new[i] ) / rho0;
      if ssc <= 0.0 then ssc = 0.333333e-36;
      else ssc = sqrt(ssc);
      q_tilde = ssc * ql_old[i] + qq_old[i];
    }

    e_new[i] -= (7.0*(p_old[i] + q_old[i]) 
                 - 8.0*(pHalfStep[i] + q_new[i]) 
                 + (p_new[i] + q_tilde)) * delvc[i] * sixth;
    if abs(e_new[i]) < e_cut then e_new[i] = 0.0;
    if e_new[i] < emin then e_new[i] = emin;
  }

  CalcPressureForElems(p_new, bvc, pbvc, e_new, compression, vnewc, pmin,
                       p_cut, eosvmax);


  //
  // TODO: Uncomment local once sparse domain is distributed
  //
  forall i in Elems /* do local */ {
    if delvc[i] <= 0.0 {
      var ssc = (pbvc[i] * e_new[i] + vnewc[i]**2 * bvc[i] * p_new[i] ) / rho0;
      if ssc <= 0.0 then ssc = 0.333333e-36;
                    else ssc = sqrt(ssc);
      q_new[i] = ssc * ql_old[i] + qq_old[i];
      if abs(q_new[i]) < q_cut then q_new[i] = 0.0;
    }
  }
}


proc CalcSoundSpeedForElems(vnewc, rho0:real, enewc, pnewc, pbvc, bvc) {
  // TODO: Open question: If we had multiple materials, should (a) ss
  // be zeroed and accumulated into, and (b) updated atomically to
  // avoid losing updates?  (Jeff will go back and think on this)
  //
  forall i in MatElems {
    var ssTmp = (pbvc[i] * enewc[i] + vnewc[i]**2 * bvc[i] * pnewc[i]) / rho0;
    if ssTmp <= 1.111111e-36 then ssTmp = 1.111111e-36;
    ss[i] = sqrt(ssTmp);
  }
}


iter elemToNodes(elem) {
  for i in 1..nodesPerElem do
    yield elemToNode[elem][i];
}
                                 
iter elemToNodesTuple(e) {
  for i in 1..nodesPerElem do
    yield (elemToNode[e][i], i);
}


proc deprint(title:string, x:[?D] real, y:[D] real, z:[D] real) {
  writeln(title);
  for i in D do
    writef("%3i: %3.4er %3.4er %3.4er\n", 
           if use3DRepresentation then idx3DTo1D(i, D.dim(1).size) else i, 
           x[i], y[i], z[i]);
}


proc deprintatomic(title:string, x:[?D] atomic real, y:[] atomic real, z:[] atomic real) {
  writeln(title);
  for i in D do
    writef("%3i: %3.4er %3.4er %3.4er\n", 
           if use3DRepresentation then idx3DTo1D(i, D.dim(1).size) else i, 
           x[i].peek(), y[i].peek(), z[i].peek());
}
