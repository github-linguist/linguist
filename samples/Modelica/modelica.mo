within Modelica.Electrical.Analog;
package Sensors "Potential, voltage, current, and power sensors"

  extends Modelica.Icons.SensorsPackage;

  model PotentialSensor "Sensor to measure the potential"
    extends Modelica.Icons.RotationalSensor;

    Interfaces.PositivePin p "pin to be measured" annotation (Placement(
          transformation(extent={{-110,-10},{-90,10}}, rotation=0)));
    Modelica.Blocks.Interfaces.RealOutput phi
      "Absolute voltage potential as output signal"
        annotation (Placement(transformation(extent={{100,-10},{120,10}},
            rotation=0)));
  equation
    p.i = 0;
    phi = p.v;
    annotation (
      Icon(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}},
          grid={1,1}), graphics={
          Text(
            extent={{-29,-11},{30,-70}},
            lineColor={0,0,0},
            textString="V"),
          Line(points={{-70,0},{-90,0}}, color={0,0,0}),
          Line(points={{100,0},{70,0}}, color={0,0,255}),
          Text(
            extent={{-150,80},{150,120}},
            textString="%name",
            lineColor={0,0,255})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}},
          grid={1,1}), graphics={Line(points={{-70,0},{-96,0}}, color={0,0,0}),
            Line(points={{100,0},{70,0}}, color={0,0,255})}),
      Documentation(revisions="<html>
<ul>
<li><i> 1998   </i>
       by Christoph Clauss<br> initially implemented<br>
       </li>
</ul>
</html>", info="<html>
<p>The potential sensor converts the voltage of a node (with respect to the ground node) into a real valued signal. It does not influence the current sum at the node which voltage is measured, therefore, the electrical behavior is not influenced by the sensor.</p>
</html>"));
  end PotentialSensor;

  model VoltageSensor "Sensor to measure the voltage between two pins"
    extends Modelica.Icons.RotationalSensor;

    Interfaces.PositivePin p "positive pin" annotation (Placement(
          transformation(extent={{-110,-10},{-90,10}}, rotation=0)));
    Interfaces.NegativePin n "negative pin" annotation (Placement(
          transformation(extent={{90,-10},{110,10}}, rotation=0)));
    Modelica.Blocks.Interfaces.RealOutput v
      "Voltage between pin p and n (= p.v - n.v) as output signal"
       annotation (Placement(transformation(
          origin={0,-100},
          extent={{10,-10},{-10,10}},
          rotation=90)));

  equation
    p.i = 0;
    n.i = 0;
    v = p.v - n.v;
    annotation (
      Icon(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}},
          grid={1,1}), graphics={
          Text(
            extent={{-29,-11},{30,-70}},
            lineColor={0,0,0},
            textString="V"),
          Line(points={{-70,0},{-90,0}}, color={0,0,0}),
          Line(points={{70,0},{90,0}}, color={0,0,0}),
          Line(points={{0,-90},{0,-70}}, color={0,0,255}),
          Text(
            extent={{-150,80},{150,120}},
            textString="%name",
            lineColor={0,0,255})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}},
          grid={1,1}), graphics={
          Line(points={{-70,0},{-96,0}}, color={0,0,0}),
          Line(points={{70,0},{96,0}}, color={0,0,0}),
          Line(points={{0,-90},{0,-70}}, color={0,0,255})}),
      Documentation(revisions="<html>
<ul>
<li><i> 1998   </i>
       by Christoph Clauss<br> initially implemented<br>
       </li>
</ul>
</html>", info="<html>
<p>The voltage  sensor converts the voltage between the two connectors into a real valued signal. It does not influence the current sum at the nodes in between the voltage is measured, therefore, the electrical behavior is not influenced by the sensor.</p>
</html>"));
  end VoltageSensor;

  model CurrentSensor "Sensor to measure the current in a branch"
    extends Modelica.Icons.RotationalSensor;

    Interfaces.PositivePin p "positive pin" annotation (Placement(
          transformation(extent={{-110,-10},{-90,10}}, rotation=0)));
    Interfaces.NegativePin n "negative pin" annotation (Placement(
          transformation(extent={{90,-10},{110,10}}, rotation=0)));
    Modelica.Blocks.Interfaces.RealOutput i
      "current in the branch from p to n as output signal"
       annotation (Placement(transformation(
          origin={0,-100},
          extent={{10,-10},{-10,10}},
          rotation=90)));

  equation
    p.v = n.v;
    p.i = i;
    n.i = -i;
    annotation (
      Icon(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}},
          grid={1,1}), graphics={
          Text(
            extent={{-29,-11},{30,-70}},
            lineColor={0,0,0},
            textString="A"),
          Line(points={{-70,0},{-90,0}}, color={0,0,0}),
          Text(
            extent={{-150,80},{150,120}},
            textString="%name",
            lineColor={0,0,255}),
          Line(points={{70,0},{90,0}}, color={0,0,0}),
          Line(points={{0,-90},{0,-70}}, color={0,0,255})}),
      Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}},
          grid={1,1}), graphics={
          Text(
            extent={{-153,79},{147,119}},
            textString="%name",
            lineColor={0,0,255}),
          Line(points={{-70,0},{-96,0}}, color={0,0,0}),
          Line(points={{70,0},{96,0}}, color={0,0,0}),
          Line(points={{0,-90},{0,-70}}, color={0,0,255})}),
      Documentation(revisions="<html>
<ul>
<li><i> 1998   </i>
       by Christoph Clauss<br> initially implemented<br>
       </li>
</ul>
</html>", info="<html>
<p>The current  sensor converts the current flowing between the two connectors into a real valued signal. The two connectors are in the sensor connected like a short cut. The sensor has to be placed within an electrical connection in series.  It does not influence the current sum at the connected nodes. Therefore, the electrical behavior is not influenced by the sensor.</p>
</html>"));
  end CurrentSensor;

model PowerSensor "Sensor to measure the power"

  Modelica.Electrical.Analog.Interfaces.PositivePin pc
      "Positive pin, current path"
    annotation (Placement(transformation(extent={{-90,-10},{-110,10}}, rotation=
             0)));
  Modelica.Electrical.Analog.Interfaces.NegativePin nc
      "Negative pin, current path"
    annotation (Placement(transformation(extent={{110,-10},{90,10}}, rotation=0)));
  Modelica.Electrical.Analog.Interfaces.PositivePin pv
      "Positive pin, voltage path"
    annotation (Placement(transformation(extent={{-10,110},{10,90}}, rotation=0)));
  Modelica.Electrical.Analog.Interfaces.NegativePin nv
      "Negative pin, voltage path"
    annotation (Placement(transformation(extent={{10,-110},{-10,-90}}, rotation=
             0)));
  Modelica.Blocks.Interfaces.RealOutput power
    annotation (Placement(transformation(
          origin={-80,-110},
          extent={{-10,10},{10,-10}},
          rotation=270)));
  Modelica.Electrical.Analog.Sensors.VoltageSensor voltageSensor
    annotation (Placement(transformation(
          origin={0,-30},
          extent={{10,-10},{-10,10}},
          rotation=90)));
  Modelica.Electrical.Analog.Sensors.CurrentSensor currentSensor
    annotation (Placement(transformation(extent={{-50,-10},{-30,10}}, rotation=
              0)));
  Modelica.Blocks.Math.Product product
    annotation (Placement(transformation(
          origin={-30,-50},
          extent={{-10,-10},{10,10}},
          rotation=270)));

equation
  connect(pv, voltageSensor.p) annotation (Line(points={{0,100},{0,-20},{
            6.12323e-016,-20}}, color={0,0,255}));
  connect(voltageSensor.n, nv) annotation (Line(points={{-6.12323e-016,-40},{
            -6.12323e-016,-63},{0,-63},{0,-100}}, color={0,0,255}));
  connect(pc, currentSensor.p)
    annotation (Line(points={{-100,0},{-50,0}}, color={0,0,255}));
  connect(currentSensor.n, nc)
    annotation (Line(points={{-30,0},{100,0}}, color={0,0,255}));
  connect(currentSensor.i, product.u2) annotation (Line(points={{-40,-10},{-40,
            -30},{-36,-30},{-36,-38}}, color={0,0,127}));
  connect(voltageSensor.v, product.u1) annotation (Line(points={{10,-30},{-24,
          -30},{-24,-38}},   color={0,0,127}));
  connect(product.y, power) annotation (Line(points={{-30,-61},{-30,-80},{-80,
            -80},{-80,-110}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics={
          Ellipse(
            extent={{-70,70},{70,-70}},
            lineColor={0,0,0},
            fillColor={255,255,255},
            fillPattern=FillPattern.Solid),
          Line(points={{0,100},{0,70}}, color={0,0,255}),
          Line(points={{0,-70},{0,-100}}, color={0,0,255}),
          Line(points={{-80,-100},{-80,0}}, color={0,0,255}),
          Line(points={{-100,0},{100,0}}, color={0,0,255}),
          Text(
            extent={{150,120},{-150,160}},
            textString="%name",
            lineColor={0,0,255}),
          Line(points={{0,70},{0,40}}, color={0,0,0}),
          Line(points={{22.9,32.8},{40.2,57.3}}, color={0,0,0}),
          Line(points={{-22.9,32.8},{-40.2,57.3}}, color={0,0,0}),
          Line(points={{37.6,13.7},{65.8,23.9}}, color={0,0,0}),
          Line(points={{-37.6,13.7},{-65.8,23.9}}, color={0,0,0}),
          Line(points={{0,0},{9.02,28.6}}, color={0,0,0}),
          Polygon(
            points={{-0.48,31.6},{18,26},{18,57.2},{-0.48,31.6}},
            lineColor={0,0,0},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid),
          Ellipse(
            extent={{-5,5},{5,-5}},
            lineColor={0,0,0},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid),
          Text(
            extent={{-29,-11},{30,-70}},
            lineColor={0,0,0},
            textString="P")}),
    Diagram(coordinateSystem(
          preserveAspectRatio=true,
          extent={{-100,-100},{100,100}},
          grid={2,2}), graphics),
    Documentation(info="<html>
<p>This power sensor measures instantaneous electrical power of a singlephase system and has a separated voltage and current path. The pins of the voltage path are pv and nv, the pins of the current path are pc and nc. The internal resistance of the current path is zero, the internal resistance of the voltage path is infinite.</p>
</html>", revisions="<html>
<ul>
<li><i>January 12, 2006</i> by Anton Haumer implemented</li>
</ul>
</html>"));
end PowerSensor;
  annotation (
    Documentation(info="<html>
<p>This package contains potential, voltage, and current sensors. The sensors can be used to convert voltages or currents into real signal values o be connected to components of the Blocks package. The sensors are designed in such a way that they do not influence the electrical behavior.</p>
</html>",
   revisions="<html>
<dl>
<dt>
<b>Main Authors:</b>
<dd>
Christoph Clau&szlig;
    &lt;<a href=\"mailto:Christoph.Clauss@eas.iis.fraunhofer.de\">Christoph.Clauss@eas.iis.fraunhofer.de</a>&gt;<br>
    Andr&eacute; Schneider
    &lt;<a href=\"mailto:Andre.Schneider@eas.iis.fraunhofer.de\">Andre.Schneider@eas.iis.fraunhofer.de</a>&gt;<br>
    Fraunhofer Institute for Integrated Circuits<br>
    Design Automation Department<br>
    Zeunerstra&szlig;e 38<br>
    D-01069 Dresden<br>
<p>
<dt>
<b>Copyright:</b>
<dd>
Copyright &copy; 1998-2010, Modelica Association and Fraunhofer-Gesellschaft.<br>
<i>The Modelica package is <b>free</b> software; it can be redistributed and/or modified
under the terms of the <b>Modelica license</b>, see the license conditions
and the accompanying <b>disclaimer</b> in the documentation of package
Modelica in file \"Modelica/package.mo\".</i><br>
<p>
</dl>
</html>"));
end Sensors;
