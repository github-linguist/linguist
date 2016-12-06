
// Size of the block 
Size = 1;

//// Mesh Size of the block 
Mesh_Size = 0.3;

// Adding Points and extruding 
Point(1)={-Size/2,-Size/2,-Size/2};
Extrude{Size,0,0}{Point{1};Layers{Size/Mesh_Size};Recombine;}
Extrude{0,Size,0}{Line{1};Layers{Size/Mesh_Size};Recombine;}
Extrude{0,0,Size}{Surface{5};Layers{Size/Mesh_Size};Recombine;}

//// Create Physical Groups 
Physical Volume ("Soil") = {1};
Physical Surface ("Base_Surface") = {5};
Physical Surface ("Top_Surface")  ={27};


