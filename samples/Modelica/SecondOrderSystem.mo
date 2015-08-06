within ModelicaByExample.PackageExamples;
model SecondOrderSystem
  "A second order rotational system importing types from Modelica Standard Library"
  import Modelica.SIunits.*;
  parameter Angle phi1_init = 0;
  parameter Angle phi2_init = 1;
  parameter AngularVelocity omega1_init = 0;
  parameter AngularVelocity omega2_init = 0;
  parameter Inertia J1=0.4;
  parameter Inertia J2=1.0;
  parameter RotationalSpringConstant k1=11;
  parameter RotationalSpringConstant k2=5;
  parameter RotationalDampingConstant d1=0.2;
  parameter RotationalDampingConstant d2=1.0;
  Angle phi1;
  Angle phi2;
  AngularVelocity omega1;
  AngularVelocity omega2;
initial equation
  phi1 = phi1_init;
  phi2 = phi2_init;
  omega1 = omega1_init;
  omega2 = omega2_init;
equation
  omega1 = der(phi1);
  omega2 = der(phi2);
  J1*der(omega1) = k1*(phi2-phi1)+d1*der(phi2-phi1);
  J2*der(omega2) = k1*(phi1-phi2)+d1*der(phi1-phi2)-k2*phi2-d2*der(phi2);
end SecondOrderSystem;
