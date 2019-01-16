import graph3;

//ASY file for figarc4_3D.asy in Chapter 7


size(200,200,IgnoreAspect);
//currentprojection=perspective(7,2,1);
currentprojection=orthographic((16.7,13.2,46),(0,1,0),(0,0,0),1,(-.0323,0.0012));
//currentprojection=orthographic((16.7,13.2,46),(0,1,0),(0,0,0),1,(0.0148,0.00673));
defaultrender.merge=true;

usepackage("amsmath");
usepackage("mathspec");
texpreamble("\setallmainfonts[Mapping=tex-text]{Calibri}");
texpreamble("\setmainfont[Mapping=tex-text]{Calibri}");
texpreamble("\setsansfont[Mapping=tex-text]{Calibri}");
texpreamble("\setmathsfont(Greek){[cmmi10]}");


// setup and draw the axes
real[] myxchoice={};
real[] myychoice={};
real[] myzchoice={};
defaultpen(0.5mm);

pair xbounds=(-.25,2.1);
pair ybounds=(-1.1,1.1);
pair zbounds=(-1.1,1.1);

xaxis3("",xbounds.x,xbounds.y,black,OutTicks(myxchoice),Arrow3(size=3mm));
yaxis3("",ybounds.x,ybounds.y,black,OutTicks(myychoice),Arrow3(size=3mm));
zaxis3("",zbounds.x,zbounds.y,invisible,OutTicks(myzchoice),Arrow3(size=3mm));

label("$x$",(xbounds.y+0.05*(xbounds.y-xbounds.x),0,0));
label("$y$",(0,ybounds.y+0.05*(ybounds.y-ybounds.x),0));
//label("$z$",(0,0,zbounds.y+0.05*(zbounds.y-zbounds.x)));

//\addplot3[domain=.9:1.4,y domain=-210:150,samples y=36,surf,shader=flat,colormap={mp2}{\colormapone}]
// ({x},{(.41*(x-.9)+.78)*sin(y)},{(.41*(x-.9)+.78)*cos(y)});

//\addplot3[domain=.5:1.9,,samples y=0,{\colorone},] ({x},{0},{(sin(deg(x)))});

//\addplot3[domain=0:360,,samples y=0,black,smooth] ({1.4},{.98*cos(x)},{.98*sin(x)});

//\addplot3[domain=130:330,,samples y=0,black,dashed,smooth] ({.9},{.78*cos(x)},{.78*sin(x)});

pen p=rgb(0,0,.7);

triple f2(pair t) {return (t.x,(.41*(t.x-.9)+.78)*sin(t.y),(.41*(t.x-.9)+.78)*cos(t.y));}
surface s2=surface(f2,(.9,0),(1.4,2*pi),2,16,Spline);
draw(s2,emissive(rgb(.6,.6,1)+opacity(.7)),meshpen=p);

triple g3(real t) {return (t,sin(t),0);}
path3 p3=graph(g3,.5,1.9,operator ..);
draw(p3,blue+.4mm);

triple g3(real t) {return (.9,.78*sin(t),.78*cos(t));}
path3 p3=graph(g3,0,2*pi,operator ..);
draw(p3,blue+.2mm);

triple g3(real t) {return (1.4,.985*sin(t),.985*cos(t));}
path3 p3=graph(g3,0,2*pi,operator ..);
draw(p3,blue+.2mm);

draw((.9,.78,0)--(1.4,.985,0),red+.6mm);

draw((.5,.05,0)--(.5,-.05,0),black+.2mm);
label("$a$",(.5,-.05,0),S);

draw((1.9,.05,0)--(1.9,-.05,0),black+.2mm);
label("$b$",(1.9,-.05,0),S);

draw((.9,.05,0)--(.9,-.05,0),black+.2mm);
label("$x_i$",(.9,-.05,0),S);

draw((1.4,.05,0)--(1.4,-.05,0),black+.2mm);
label("$x_{i+1}$",(1.4,-.05,0),S);

label(XY*"$\left.\rule{0pt}{40pt}\right\}\ R$",(1.4,.49,0),E,Embedded);
label(XY*"$r\left\{\rule{0pt}{30pt}\right.$",(.85,.37,0),W,Embedded);
label(rotate(20,(0,0,1))*"$\overbrace{\rule{41pt}{0pt}}^{\text{\normalsize \textit{L}}}$",(1.1,1.03,0),Embedded);

// ////////////////////////////////////
//    SAMPLE CODE

// defaultpen(fontsize(10pt));

//real f(pair z) {return -z.x^4+2*z.x^2-z.y^4+2*z.y^2;}
//surface s=surface(f,(-1.5,-1.5),(1.5,1.5),Spline);
//pen p=rgb(0,0,.7);
//draw(s,rgb(.6,.6,1)+opacity(.7),meshpen=p);

//triple f(pair t) {
//  return (cos(t.x)*1.5*cos(t.y),sin(t.x)*cos(t.y),sin(t.y));
//}
//surface s=surface(f,(0,0),(pi,2*pi),8,8,Spline);
//pen p=rgb(0,0,.7);
//draw(s,rgb(.6,.6,1)+opacity(.7),meshpen=p);

//draw(s,paleblue);
//draw(s,lightblue,meshpen=black+thick(),nolight,render(merge=true));
//draw(mypath,2bp+blue);

//triple g(real t) {return (t,t,-2*t^4+4*t^2);}
//path3 mypath=graph(g,-1,1,operator ..);
//draw(mypath,blue+dashed+linewidth(2));

//pen p=rgb(0,0,1);
//draw(s,paleblue+opacity(.5),meshpen=p,render(merge=true));








