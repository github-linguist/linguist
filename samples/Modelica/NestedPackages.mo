within ModelicaByExample.PackageExamples;
package NestedPackages
  "An example of how packages can be used to organize things"
  package Types
    type Rabbits = Real(quantity="Rabbits", min=0);
    type Wolves = Real(quantity="Wolves", min=0);
    type RabbitReproduction = Real(quantity="Rabbit Reproduction", min=0);
    type RabbitFatalities = Real(quantity="Rabbit Fatalities", min=0);
    type WolfReproduction = Real(quantity="Wolf Reproduction", min=0);
    type WolfFatalities = Real(quantity="Wolf Fatalities", min=0);
  end Types;

  model LotkaVolterra "Lotka-Volterra with types"
    parameter Types.RabbitReproduction alpha=0.1;
    parameter Types.RabbitFatalities beta=0.02;
    parameter Types.WolfReproduction gamma=0.4;
    parameter Types.WolfFatalities delta=0.02;
    parameter Types.Rabbits x0=10;
    parameter Types.Wolves y0=10;
    Types.Rabbits x(start=x0);
    Types.Wolves y(start=y0);
  equation
    der(x) = x*(alpha-beta*y);
    der(y) = -y*(gamma-delta*x);
  end LotkaVolterra;
end NestedPackages;
