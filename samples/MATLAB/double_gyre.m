clear all
tic
% initialize integration time T, f(x,t), discretization size n ----------------
T = 8;
x_min=0;
x_max=2;
y_min=0;
y_max=1;
n=50; % how many points per one measure unit (both in x and in y)
ds=1/(n-1);
x_res=(x_max-x_min)*n;
y_res=(y_max-y_min)*n;
grid_x=linspace(x_min,x_max,x_res);
grid_y=linspace(y_min,y_max,y_res);

advected_x=zeros(x_res,y_res);
advected_y=zeros(x_res,y_res);
% integrate all initial points for t in [0,T] --------------------------------
parfor i = 1:x_res
	for j = 1:y_res
		[t,X] = ode45(@dg,[0,T],[grid_x(i),grid_y(j)]);
		% store advected positions as they would appear in (x,y) coords ------
		advected_x(i,j) = X(length(X(:,1)),1);
		advected_y(i,j) = X(length(X(:,2)),2);
	end
end
%% Compute FTLE
sigma=zeros(x_res,y_res);
% at each point in interior of grid, store FTLE ------------------------------
for i = 2:x_res-1
	for j = 2:y_res-1
		% compute Jacobian phi -----------------------------------------------
		phi(1,1) = (advected_x(i+1,j)-advected_x(i-1,j))/(2*ds);
		phi(1,2) = (advected_x(i,j-1)-advected_x(i,j+1))/(2*ds);
		phi(2,1) = (advected_y(i+1,j)-advected_y(i-1,j))/(2*ds);
		phi(2,2) = (advected_y(i,j-1)-advected_y(i,j+1))/(2*ds);
		% find max eigenvalue of phi'*phi ------------------------------------
		lambda_max = max(abs(eig(phi'*phi)));
		% store FTLE ---------------------------------------------------------
		sigma(i,j) = log(lambda_max)/abs(2*T);
	end
end
toc
%% plot FTLE field ------------------------------------------------------------
figure
contourf(grid_x,grid_y,sigma');
colorbar('location','EastOutside');
axis equal
shading flat
