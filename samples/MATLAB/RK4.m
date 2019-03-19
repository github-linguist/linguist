function x = RK4( fun, tspan, ci, mu )
%RK4 4th-order Runge Kutta integrator
%   Detailed explanation goes here
h=1e-5;
t=tspan(1);
T=tspan(length(tspan));
dim=length(ci);
%x=zeros(l,dim);
x(:,1)=ci;
i=1;
while t<T
	k1=fun(t,x(:,i),mu);
	k2=fun(t+h/2,x(:,i)+k1*h/2,mu);
	k3=fun(t+h/2,x(:,i)+k2*h/2,mu);
	k4=fun(t+h,x(:,i)+h*k3,mu);
	x(:,i+1)=x(:,i)+(h/6*(k1+2*k2+2*k3+k4));
	t=t+h;
	i=i+1;
end
x=x';
% 	function events(x)
% 	dist=
% 	return 
end
