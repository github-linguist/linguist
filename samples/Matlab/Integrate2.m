function [ x_T, y_T, vx_T, e_T, filter, delta_e ] = Integrate_FTLE_Gawlick_ell( x_0, y_0, vx_0, e_0, T, mu, ecc, nu, options)
%Integrate
%   This function performs Runge-Kutta-Fehlberg integration for given
%   initial conditions to compute FTLE to obtain the image in the Gawlick's
%   article "Lagrangian Coherent Structures in the Elliptic Restricted
%   Three-Body Problem".
nx=length(x_0);
ny=length(y_0);
nvx=length(vx_0);
ne=length(e_0);
vy_0=zeros(nx,ny,nvx,ne);
x_T=zeros(nx,ny,nvx,ne);
y_T=zeros(nx,ny,nvx,ne);
vx_T=zeros(nx,ny,nvx,ne);
vy_T=zeros(nx,ny,nvx,ne);
e_T=zeros(nx,ny,nvx,ne);
delta_e=zeros(nx,ny,nvx,ne);
%% Look for phisically meaningful points
filter=zeros(nx,ny,nvx,ne);  %0=meaningless point 1=meaningful point
useful=ones(nx,ny,nvx,ne);
%% Integrate only useful points
useful(:,1,:,1)=0;
useful(:,1,:,3)=0;
useful(:,3,:,1)=0;
useful(:,3,:,3)=0;

%% Integrate only meaningful points
h=waitbar(0,'','Name','Integration in progress, please wait!');
for i=1:nx
	waitbar(i/nx,h,sprintf('Computing i=%i',i));
	for j=1:ny
		parfor k=1:nvx
			for l=1:ne
				if useful(i,j,k,l)
					vy_0(i,j,k,l)=-sqrt(2*(Omega(x_0(i),y_0(j),mu)/(1+ecc*cos(nu)))+2*e_0(l)-vx_0(k)^2);
					if isreal(vy_0(i,j,k,l))
						filter(i,j,k,l)=1;
						
						ci=[x_0(i), y_0(j), vx_0(k), vy_0(i,j,k,l)];
						[t,Y]=ode45(@f_ell,[0 T], ci, options, mu, ecc);
						
						if abs(t(end)) < abs(T) % Consider also negative time
							filter(i,j,k,l)=3
						end
						
						x_T(i,j,k,l)=Y(end,1);
						y_T(i,j,k,l)=Y(end,2);
						vx_T(i,j,k,l)=Y(end,3);
						vy_T(i,j,k,l)=Y(end,4);
						e_T(i,j,k,l)=0.5*(vx_T(i,j,k,l)^2+vy_T(i,j,k,l)^2)-Omega(x_T(i,j,k,l),y_T(i,j,k,l),mu);
						
						% Compute the goodness of the integration
						delta_e(i,j,k,l)=abs(e_T(i,j,k,l)-e_0(l));
					end
				end
			end
		end
	end
end
close(h);