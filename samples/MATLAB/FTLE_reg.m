tic
clear all
%% Elements for grid definition
n=100;

%% Dimensionless integrating time
T=2;

%% Choice of the mass parameter
mu=0.1;

%% Computation of Lagrangian Points
[xl1,yl1,xl2,yl2,xl3,yl3,xl4,yl4,xl5,yl5] = Lagr(mu);

%% Computation of initial total energy
E_L1=-Omega(xl1,yl1,mu);
C_L1=-2*E_L1; % C_L1 = 3.6869532299 from Szebehely
E=E_L1+0.03715; % Offset as in figure 2.2 "LCS in the ER3BP"

%% Initial conditions range
x_0_min=-0.8;
x_0_max=-0.2;

vx_0_min=-2;
vx_0_max=2;

y_0=0;

% Grid initializing
[x_0,vx_0]=ndgrid(linspace(x_0_min,x_0_max,n),linspace(vx_0_min,vx_0_max,n));
vy_0=sqrt(2*E+2.*Omega(x_0,y_0,mu)-vx_0.^2);
% Kinetic energy computation
E_cin=E+Omega(x_0,y_0,mu);

% Inizializing
x_T=zeros(n,n);
y_T=zeros(n,n);
vx_T=zeros(n,n);
vy_T=zeros(n,n);
filtro=ones(n,n);
E_T=zeros(n,n);
delta_E=zeros(n,n);
a=zeros(n,n); % matrix of numbers of integration steps for each integration
np=0; % number of integrated points

fprintf('integro con n = %i\n',n)

%% Energy tolerance setting
energy_tol=0.1;

%% Setting the options for the integrator
RelTol=1e-12;AbsTol=1e-12; % From Short
% RelTol=1e-13;AbsTol=1e-22; % From JD James Mireles
% RelTol=3e-14;AbsTol=1e-16; % HIGH accuracy from Ross
options=odeset('AbsTol',AbsTol,'RelTol',RelTol);
%% Parallel integration of equations of motion
h=waitbar(0,'','Name','Integration in progress, please wait!');
S=zeros(n,n);
r1=zeros(n,n);
r2=zeros(n,n);
g=zeros(n,n);
for i=1:n
    waitbar(i/n,h,sprintf('Computing i=%i',i));
	parfor j=1:n
        r1(i,j)=sqrt((x_0(i,j)+mu).^2+y_0.^2);
		r2(i,j)=sqrt((x_0(i,j)-1+mu).^2+y_0.^2);
		g(i,j)=((1-mu)./(r1(i,j).^3)+mu./(r2(i,j).^3));
		if E_cin(i,j)>0 && isreal(vy_0(i,j)) % Check for real velocity and positive Kinetic energy
            S(i,j)=g(i,j)*T;
            [s,Y]=ode45(@f_reg,[0 S(i,j)],[x_0(i,j); y_0; vx_0(i,j); vy_0(i,j)],options,mu);
			a(i,j)=length(Y);
%             if s(a(i,j)) < 2
%                 filtro(i,j)=3;
%             end
			% Saving solutions
			x_T(i,j)=Y(a(i,j),1);
			vx_T(i,j)=Y(a(i,j),3);
			y_T(i,j)=Y(a(i,j),2);
			vy_T(i,j)=Y(a(i,j),4);

			% Computation of final total energy and difference with
			% initial one
			E_T(i,j)=Energy(x_T(i,j),y_T(i,j),vx_T(i,j),vy_T(i,j),mu);
            delta_E(i,j)=abs(E_T(i,j)-E);
            if  delta_E(i,j) > energy_tol; % Check of total energy conservation
                fprintf(' Ouch! Wrong Integration: i,j=(%i,%i)\n E_T=%.2f \n delta_E=%f\n\n',i,j,E_T(i,j),delta_E(i,j));
                filtro(i,j)=2; % Saving position of the point
            end
            np=np+1;
        else
			filtro(i,j)=0; % 1 = interesting point; 0 = non-sense point; 2 = bad integration point		
		end
	end
end
close(h);
t_integrazione=toc;
%%
filtro_1=filtro;
for i=2:n-1
    for j=2:n-1
        if filtro(i,j)==2 || filtro (i,j)==3
            filtro_1(i,j)=2;
			filtro_1(i+1,j)=2;
            filtro_1(i-1,j)=2;
            filtro_1(i,j+1)=2;
            filtro_1(i,j-1)=2;
        end
    end
end

fprintf('integato con n = %i\n',n)
fprintf('integato con energy_tol = %f\n',energy_tol)
fprintf('numero punti totali	\t%i\n',n^2)
fprintf('numero punti integrati	\t%i\n',np)
fprintf('tempo per integrare	\t%.2f s\n',t_integrazione)

%% FTLE Computation
fprintf('adesso calcolo ftle\n')
tic
dphi=zeros(2,2);
ftle=zeros(n-2,n-2);
ftle_norm=zeros(n-2,n-2);

ds_x=(x_0_max-x_0_min)/(n-1);
ds_vx=(vx_0_max-vx_0_min)/(n-1);

for i=2:n-1
	for j=2:n-1
		if filtro_1(i,j) && ... % Check for interesting point
				filtro_1(i,j-1) && ...
				filtro_1(i,j+1) && ...
				filtro_1(i-1,j) && ...
				filtro_1(i+1,j)
			% La direzione dello spostamento la decide il denominatore
			
			% TODO spiegarsi teoricamente come mai la matrice pu�
			% essere ridotta a 2x2
			dphi(1,1)=(x_T(i+1,j)-x_T(i-1,j))/(2*ds_x); %(x_0(i-1,j)-x_0(i+1,j));
			
			dphi(1,2)=(x_T(i,j+1)-x_T(i,j-1))/(2*ds_vx); %(vx_0(i,j-1)-vx_0(i,j+1));
	
			dphi(2,1)=(vx_T(i+1,j)-vx_T(i-1,j))/(2*ds_x); %(x_0(i-1,j)-x_0(i+1,j));
            
			dphi(2,2)=(vx_T(i,j+1)-vx_T(i,j-1))/(2*ds_vx); %(vx_0(i,j-1)-vx_0(i,j+1));
    
			if filtro_1(i,j)==2 % Manual setting to visualize bad integrated points 
				ftle(i-1,j-1)=0;
			else
				ftle(i-1,j-1)=(1/abs(T))*log(max(sqrt(abs(eig(dphi*dphi')))));
                ftle_norm(i-1,j-1)=(1/abs(T))*log(norm(dphi));
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

% ora=fstringf %TODO
% save(['var_' num2str(n) '_' num2str(clock(4)])

nome=['var_xvx_', 'ode00', '_n',num2str(n)];
save(nome)