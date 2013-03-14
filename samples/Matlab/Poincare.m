clear
%% Initial Conditions
mu=0.012277471;
T=10;
N=5;
C=3.17;
x_0=0.30910452642073;
y_0=0.07738174525518;
vx_0=-0.72560796964234;
vy_0=sqrt(-C-vx_0^2+2*Potential(x_0,y_0,mu));
k=0;
%% Integration
options=odeset('AbsTol',1e-22,'RelTol',1e-13,'Events',@cross_y);
[t,y,te,ye,ie]=ode113(@f,[0 T],[x_0; y_0; vx_0; vy_0],options,mu);

figure
%plot(ye(:,1),ye(:,3),'rs')
plot(ye(:,1),0,'rs')