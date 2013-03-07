clear all
tic
% initialize integration time T, f(x,t), discretization size n ----------------
T = 20;
f_x_t = inline('[v(2);-sin(v(1))]','t','v');
grid_min = -3.4;
grid_max = 3.4;
grid_width = grid_max-grid_min;
n = 35;
grid_spacing = grid_min:(grid_width/(n-1)):grid_max;
advected_x=zeros(n,n);
advected_y=zeros(n,n);
% integrate all initial points for t in [0,T] --------------------------------
for i = 1:n
	for j = 1:n
		[t,x] = ode45(f_x_t,[0,T],[grid_spacing(i),grid_spacing(j)]);
		% store advected positions as they would appear in (x,y) coords ------
		advected_x(n-j+1,i) = x(length(x(:,1)),1);
		advected_y(n-j+1,i) = x(length(x(:,2)),2);
	end
end
sigma=zeros(n,n);
% at each point in interior of grid, store FTLE ------------------------------
for i = 2:n-1
	for j = 2:n-1
		% compute Jacobian phi -----------------------------------------------
		phi(1,1) = (advected_x(i,j+1)-advected_x(i,j-1))/(2*grid_width/(n-1));
		phi(1,2) = (advected_x(i-1,j)-advected_x(i+1,j))/(2*grid_width/(n-1));
		phi(2,1) = (advected_y(i,j+1)-advected_y(i,j-1))/(2*grid_width/(n-1));
		phi(2,2) = (advected_y(i-1,j)-advected_y(i+1,j))/(2*grid_width/(n-1));
		% find max eigenvalue of phi'*phi ------------------------------------
		lambda_max = max(abs(eig(phi'*phi)));
		% store FTLE ---------------------------------------------------------
		sigma(i,j) = log(lambda_max)/abs(T);
	end
end
toc
%% plot FTLE field ------------------------------------------------------------
figure
contourf(grid_spacing,grid_spacing,sigma);
colorbar('location','EastOutside');