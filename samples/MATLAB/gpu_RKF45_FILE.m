tic
clear
%% Range definition
n=200;

mu=0.1;
[xl1,yl1,xl2,yl2,xl3,yl3,xl4,yl4,xl5,yl5]=Lagr(mu);
C_L1=2*Omega(xl1,yl1,mu);
E_0=-C_L1/2+0.03715;
Y_0=0;

nx=n;
x_0_min=-0.8;
x_0_max=-0.15;
x_0=linspace(x_0_min, x_0_max, nx);
dx=(x_0_max-x_0_min)/(nx-1);

nvx=n;
vx_0_min=-2;
vx_0_max=2;
vx_0=linspace(vx_0_min, vx_0_max, nvx);
dvx=(vx_0_max-vx_0_min)/(nvx-1);

ny=3;
dy=(dx+dvx)/2;
y_0=[Y_0-dy Y_0 Y_0+dy];



ne=3;
de=dy;
e_0=[E_0-de E_0 E_0+de];

%% Definition of arrays of initial conditions

%In this approach, only useful pints are stored and integrated

m=1;
% x=zeros(1,nx*ny*nvx*ne);
% y=zeros(1,nx*ny*nvx*ne);
% vx=zeros(1,nx*ny*nvx*ne);
% e=zeros(1,nx*ny*nvx*ne);
% vy=zeros(1,nx*ny*nvx*ne);
filter=zeros(nx,3,nvx,3);

for i=1:nx
	for j=1:ny
		for k=1:nvx
			for l=1:ne
				v_y=-sqrt(2*Omega(x_0(i),y_0(j),mu)+2*e_0(l)-vx_0(k)^2);
				if ~((j~=2) && (l~=2)) && isreal(v_y)
					x(m)=x_0(i);
					y(m)=y_0(j);
					vx(m)=vx_0(k);
					e(m)=e_0(l);
					vy(m)=v_y;
					filter(i,j,k,l)=1;
					m=m+1;
				end
			end
		end
	end
end

%% Selection of useful points

%% Data transfer to GPU
x_gpu=gpuArray(x);
y_gpu=gpuArray(y);
vx_gpu=gpuArray(vx);
vy_gpu=gpuArray(vy);

%% Integration on GPU
N=1;
t0=0;

[x_f,y_f,vx_f,vy_f]=arrayfun(@RKF45_FILE_gpu,t0,N,x_gpu,y_gpu,vx_gpu,vy_gpu,mu);

%% Data back to CPU and GPU memory cleaning
clear x_gpu y_gpu vx_gpu vy_gpu
x_T=gather(x_f);
clear x_f
y_T=gather(y_f);
clear y_f
vx_T=gather(vx_f);
clear vx_f
vy_T=gather(vy_f);
clear vy_f

%% Construction of matrix for FTLE computation

X_T=zeros(nx,ny,nvx,ne);
Y_T=zeros(nx,ny,nvx,ne);
VX_T=zeros(nx,ny,nvx,ne);
VY_T=zeros(nx,ny,nvx,ne);
E_T=zeros(nx,ny,nvx,ne);
m=1;
for i=1:nx
    for j=1:ny
        for k=1:nvx
            for l=1:ne
                if filter(i,j,k,l)==1
                    X_T(i,j,k,l)=x_T(m);
                    Y_T(i,j,k,l)=y_T(m);
                    VX_T(i,j,k,l)=vx_T(m);
                    VY_T(i,j,k,l)=vy_T(m);
                    E_T(i,j,k,l)=0.5*(VX_T(i,j,k,l)^2+VY_T(i,j,k,l)^2)-Omega(X_T(i,j,k,l),Y_T(i,j,k,l),mu);
                    m=m+1;
                end
            end
        end
    end
end

%% Compute filter for FTLE
filter_ftle=filter;
for i=2:(nx-1)
	for j=2:(ny-1)
		for k=2:(nvx-1)
			for l=2:(ne-1)
				if filter(i,j,k,l)==0 || filter (i,j,k,l)==3
					filter_ftle(i,j,k,l)=0;
					
					filter_ftle(i+1,j,k,l)=0;
					filter_ftle(i-1,j,k,l)=0;
					
					filter_ftle(i,j+1,k,l)=0;
					filter_ftle(i,j-1,k,l)=0;
					
					filter_ftle(i,j,k+1,l)=0;
					filter_ftle(i,j,k-1,l)=0;
					
					filter_ftle(i,j,k,l+1)=0;
					filter_ftle(i,j,k,l-1)=0;
				end
			end
			
		end
	end
end
%% FTLE computation

[ftle, dphi]=Compute_FILE_gpu( X_T, Y_T, VX_T, E_T, dx, dy, dvx, de, N, filter_ftle);

%% Plot results
figure
FTLE=squeeze(ftle(:,2,:,2));
FTLE(1,:)=[];
% FTLE(2,:)=[];
FTLE(:,1)=[];
% FTLE(:,2)=[];
x_0(1)=[];
vx_0(1)=[];
pcolor(x_0, vx_0, FTLE')
shading flat
toc