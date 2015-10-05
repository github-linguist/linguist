within ModelicaByExample.Subsystems.Pendula;
model System "A system of pendula"
  import Modelica.Constants.g_n;
  import Modelica.Constants.pi;

  parameter Integer n=15 "Number of pendula";
  parameter Modelica.SIunits.Position x[n] = linspace(0,(n-1)*0.05,n);
  parameter Modelica.SIunits.Time T = 54;
  parameter Modelica.SIunits.Time X = 30;
  parameter Modelica.SIunits.Length lengths[n] = { g_n*(T/(2*pi*(X+(n-i))))^2 for i in 1:n};
  parameter Modelica.SIunits.Angle phi0 = 0.5;

  Pendulum pendulum[n](x=x, each m=1, each phi=phi0, L=lengths)
    annotation (Placement(transformation(extent={{-10,-10},{10,10}})));
  inner Modelica.Mechanics.MultiBody.World world
    annotation (Placement(transformation(extent={{-80,-60},{-60,-40}})));
  annotation (experiment(StopTime=54,
      Interval=9e-3, Tolerance=1e-008));
end System;
