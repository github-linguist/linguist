clear all
%mu=0.012151; %Earth-Moon
mu=0.012277471 %Earth-Moon
[xl1,yl1,xl2,yl2,xl3,yl3,xl4,yl4,xl5,yl5] = Lagr(mu);
C=3.17;
%C=2*Potential(xl1,yl1,mu);
x_0=1;
y_0=0;
vx_0=0;
vy_0=sqrt(-C-vx_0^2+2*Potential(x_0,y_0,mu));
%vy_0=0;
T=2;
C_star=2*Potential(x_0,y_0,mu)-(vx_0^2+vy_0^2)
%C=-(vx_0^2+vy_0^2)+2*Omega(x_0,y_0,mu);
E=-C/2;

options=odeset('AbsTol',1e-22,'RelTol',1e-13); 

%Integrate first orbit
[t0,Y0]=ode113(@f,[0 T],[x_0; y_0; vx_0; vy_0],options,mu);
x0=Y0(:,1);
y0=Y0(:,2);
vx0=Y0(:,3);
vy0=Y0(:,4);
l0=length(Y0);

% Precisionfirst orbit
delta_E0=abs(Energy(x0,y0,vx0,vy0,mu)-E);

% figure
% plot(delta_E0)

%Hill's region
points=500;
bb=3; % Bounding box
x=linspace(-bb,bb,points);
y=linspace(-bb,bb,points);
[x,y]=meshgrid(x,y);
z=(Potential(x,y,mu));
% figure
% surfc(x,y,z,'Edgecolor','none')

%Plot orbit
%figure
hold on
contour(x,y,z,[C/2,C/2])
plot(x0,y0)
text(-2,-2,sprintf('C=%.2f',C))
%plotto actractors
plot(-mu,0,'ok')
plot(1-mu,0,'ok')
% Plot points
plot(x0(1),y0(1),'sg')
plot(x0(l0),y0(l0),'sr')