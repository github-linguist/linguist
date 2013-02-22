function [ value,isterminal,direction ] = distance( t,y,mu )
% DISTANCE compute the distance from the attactors
%   [ value,terminal,direction ] = distance( t,y )

d=1e-2; % FIXME

% TODO mettere if se tolleranza D-d<tol -> value=0
D=sqrt((y(1)+mu).^2+y(2).^2); % distance from the largest primary

value=d-D;
isterminal=1;
direction=0;
end