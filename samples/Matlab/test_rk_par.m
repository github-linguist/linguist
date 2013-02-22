clear
mu=0.1;
x_0=linspace(-0.8, -0.15, 2)
y_0=zeros(1,2)
vx_0=linspace(-2, 2, 2)
vy_0=zeros(1,2)
ci=[1-mu-0.05 0 0.005 0.5290]
t0=[0;0]
T=[2;2]
tspan=2
arg1={@f;@f}
%tspan={[0 2],[0 2]};
arg=[mu;mu]
[X]=arrayfun(RK4_par,t0,T,x_0',y_0',vx_0',vy_0',arg)
% [X]=arrayfun(@f,[0;1],[ci;ci],[mu;mu]);
%Y=RK4(@f,tspan,ci,mu);
% figure
% plot(Y(:,1),Y(:,2))
% Y(end,1)