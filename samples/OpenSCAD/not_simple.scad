// A more complicated 3D shape in OpenSCAD
$fn=32;

difference() {
    // main shape
    union() {
        translate( [ 0, 0,  2 ] ) cube( [ 15, 15, 4 ], center=true );
        translate( [ 0, 0, 13 ] ) cylinder( h=25, r1=5, r2=3, center=true );
        translate( [ 0, 0, 28 ] ) sphere( r=6 );
    }
    // hole through center
    translate( [ 0, 0, 17 ] ) cylinder( h=35, r=2, center=true );
}
