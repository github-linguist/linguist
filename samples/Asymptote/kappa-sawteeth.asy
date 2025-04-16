import wtk_graph;

size(15cm,10cm,IgnoreAspect);

scale(Linear, Linear);
real xscale=1e9;
real fscale=1e12;

void graphSawtooth(string file="datafile", real k,
                   int xcol=0, int fcol=1,
                   real xscale=1, real fscale=1, real dx=0, real df=0,
                   pen p=red, string t="Title") {
  file fin=input(file).line();
  real[][] a=fin.dimension(0,0);
  a=transpose(a);
  real[] x=a[xcol];
  real[] f=a[fcol];
  x = x - f/k; /* Remove cantilever extension */
  graphData(x=x, y=f, xscale=xscale, yscale=fscale,
            dx=dx, dy=df, p=p, t=t, dots=false);
}

real[] k = {0.001, 0.01, 0.05, 0.1, 0.5, 1, 10};
pen[] p = {blue, green, red, cyan, magenta, yellow, black};

int i;
for (i=k.length-1; i>=0; --i) {/* count down so legend and plot orders match */
  string file = format("k-%f", k[i])+"."+"dat";
  /* We break up .dat so the Asymptote scanner doesn't pick up
   * .dat as a dependency (which obviously doesn't exist). */
  string label = math("k="+units(format("%f",k[i]*1e3), "pN/nm"));
  if (k[i] >= 1)
    label = math("k="+units(format("%f",k[i]),"nN/nm"));
  // TODO: rainbow pen(N, i)
  graphSawtooth(file=file, k=k[i], xscale=xscale, fscale=fscale,
                df=300e-12*i, p=p[i], t=label);
}

xlimits(0, 300e-9*xscale, crop=true);
xaxis(sLabel("Distance (nm) ($x_t-x_c$)"), BottomTop, LeftTicks);
yaxis(sLabel("Force (pN)"), LeftRight, RightTicks);
label(sLabel("Simulated force curves"), point(N), N);

add(legend(), point(E), 20E);
//add(lengend(). point(E), 20E); //, UnFill);
