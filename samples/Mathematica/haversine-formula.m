distance[{theta1_, phi1_}, {theta2_, phi2_}] :=
 2*6378.14 ArcSin@
   Sqrt[Haversine[(theta2 - theta1) Degree] +
     Cos[theta1*Degree] Cos[theta2*Degree] Haversine[(phi2 - phi1) Degree]]
