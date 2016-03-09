within ModelicaByExample.PackageExamples;
model NewtonCooling
  "Cooling example importing physical types from the Modelica Standard Library"
  import Modelica.SIunits.Temperature;
  import Modelica.SIunits.Mass;
  import Modelica.SIunits.Area;
  import ConvectionCoefficient = Modelica.SIunits.CoefficientOfHeatTransfer;
  import SpecificHeat = Modelica.SIunits.SpecificHeatCapacity;

  // Parameters
  parameter Temperature T_inf=300.0 "Ambient temperature";
  parameter Temperature T0=280.0 "Initial temperature";
  parameter ConvectionCoefficient h=0.7 "Convective cooling coefficient";
  parameter Area A=1.0 "Surface area";
  parameter Mass m=0.1 "Mass of thermal capacitance";
  parameter SpecificHeat c_p=1.2 "Specific heat";

  // Variables
  Temperature T "Temperature";
initial equation
  T = T0 "Specify initial value for T";
equation
  m*c_p*der(T) = h*A*(T_inf-T) "Newton's law of cooling";
end NewtonCooling;
