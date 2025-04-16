tic
clear all
%% Choice of the mass parameter
mu=0.1;

%% Computation of Lagrangian Points
[xl1,yl1,xl2,yl2,xl3,yl3,xl4,yl4,xl5,yl5] = Lagr(mu);

%% Computation of initial total energy
E_L1=-Omega(xl1,yl1,mu);
E=E_L1+0.03715; % Offset as in figure 2.2 "LCS in the ER3BP"

%% Initial conditions range
x_0_min=-0.8;
x_0_max=-0.2;

vx_0_min=-2;
vx_0_max=2;

y_0=0;

% Elements for grid definition
n=200;

% Dimensionless integrating time
T=2;

% Grid initializing
[x_0,vx_0]=ndgrid(linspace(x_0_min,x_0_max,n),linspace(vx_0_min,vx_0_max,n));
vy_0=sqrt(2*E+2*Omega(x_0,y_0,mu)-vx_0.^2);

% Kinetic energy computation
E_cin=E+Omega(x_0,y_0,mu);

%% Transforming into Hamiltonian variables
px_0=vx_0-y_0;
py_0=vy_0+x_0;

% Inizializing
x_T=zeros(n,n);
y_T=zeros(n,n);
px_T=zeros(n,n);
py_T=zeros(n,n);
filtro=ones(n,n);
E_T=zeros(n,n);
a=zeros(n,n); % matrix of numbers of integration steps for each integration
np=0; % number of integrated points

fprintf(' con n = %i\n',n)

%% Energy tolerance setting
energy_tol=inf;

%% Computation of the Jacobian of the system
options=odeset('Jacobian',@cr3bp_jac);

%% Parallel integration of equations of motion
parfor i=1:n
	for j=1:n
		if E_cin(i,j)>0 && isreal(vy_0(i,j)) % Check for real velocity and positive Kinetic energy
			[t,Y]=ode45(@fH,[0 T],[x_0(i,j); y_0; px_0(i,j); py_0(i,j)],options);
            % Try to obtain the name of the solver for a following use
%  			sol=ode45(@f,[0 T],[x_0(i,j); y_0; vx_0(i,j); vy_0(i,j)],options);
% 			Y=sol.y';
% 			solver=sol.solver;
			a(i,j)=length(Y);
            %Saving solutions
			x_T(i,j)=Y(a(i,j),1); 
			px_T(i,j)=Y(a(i,j),3);
			y_T(i,j)=Y(a(i,j),2);
			py_T(i,j)=Y(a(i,j),4);
			%Computation of final total energy and difference with
			%initial one
			E_T(i,j)=EnergyH(x_T(i,j),y_T(i,j),px_T(i,j),py_T(i,j),mu);
			delta_E=abs(E_T(i,j)-E);
			if  delta_E > energy_tol; %Check of total energy conservation
				fprintf(' Ouch! Wrong Integration: i,j=(%i,%i)\n E_T=%.2f \n delta_E=%.2f\n\n',i,j,E_T(i,j),delta_E);
				filtro(i,j)=2; %Saving position of the point
            end
			np=np+1;
        else
			filtro(i,j)=0; % 1=interesting point; 0=non-sense point; 2= bad integration point		
		end
	end
end

t_integrazione=toc;
fprintf('  n = %i\n',n)
fprintf(' energy_tol = %.2f\n',energy_tol)
fprintf('total	\t%i\n',n^2)
fprintf('nunber	\t%i\n',np)
fprintf('time to integrate	\t%.2f s\n',t_integr)

%% Back to Lagrangian variables
vx_T=px_T+y_T;
vy_T=py_T-x_T;
%% FTLE Computation
fprintf('adesso calcolo ftle\n')
tic
dphi=zeros(2,2);
ftle=zeros(n-2,n-2);

for i=2:n-1
	for j=2:n-1
		if filtro(i,j) && ... % Check for interesting point
				filtro(i,j-1) && ...
				filtro(i,j+1) && ...
				filtro(i-1,j) && ...
				filtro(i+1,j)
			
			dphi(1,1)=(x_T(i-1,j)-x_T(i+1,j))/(x_0(i-1,j)-x_0(i+1,j));
			
			dphi(1,2)=(x_T(i,j-1)-x_T(i,j+1))/(vx_0(i,j-1)-vx_0(i,j+1));
			
			dphi(2,1)=(vx_T(i-1,j)-vx_T(i+1,j))/(x_0(i-1,j)-x_0(i+1,j));
			
			dphi(2,2)=(vx_T(i,j-1)-vx_T(i,j+1))/(vx_0(i,j-1)-vx_0(i,j+1));
            
			if filtro(i,j)==2 % Manual setting to visualize bad integrated points 
				ftle(i-1,j-1)=-Inf;
			else
				ftle(i-1,j-1)=1/(2*T)*log(max(abs(eig(dphi'*dphi))));
			end
		end
	end
end

%% Plotting results
% figure
% plot(t,Y)
% figure
% plot(Y(:,1),Y(:,2))
% figure

xx=linspace(x_0_min,x_0_max,n);
vvx=linspace(vx_0_min,vx_0_max,n);
[x,vx]=ndgrid(xx(2:n-1),vvx(2:n-1));
figure
pcolor(x,vx,ftle)
shading flat

t_ftle=toc;
fprintf('tempo per integrare      \t%.2f s\n',t_integrazione)
fprintf('tempo per calcolare ftle \t%.2f s\n',t_ftle)

% save(['var_' num2str(n) '_' num2str(clock(4)])

nome=['var_xvx_', 'ode00', '_n',num2str(n),'_e',num2str(energy_tol),'_H'];
save(nome)