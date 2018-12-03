function [dx, y] = adapting_structural_model(t, x, u, varargin)
%
% Returns the time derivatives of the states and the output of the
% structural control model with an adapting controller.
%
% Parameters
% ----------
% t : double
%   The current time.
% x : double, size(8, 1)
%   The current state.
% u : double, size(1, 1)
%   The current input.
% varargin : cell array
%   m1, m2, m3, m4, b1, b2, b3, b4 : double
%       The slope of the four gains and the offset of the four gains.
%   aux : cell array containing a single structure
%       The structure contains:
%       pars : double, size(1,9)
%           The controller parameters.
%       timeDelay : logical
%           If true a 1st order Pade approximation of the human's time delay
%           is included.
%       plantFirst : integer
%           The number of the first plant.
%       plantSecond : integer
%           The number of the second plant.
%       m : double, size(2, 1)
%           The slope of the transfer function adaption function.
%       b : double, size(2, 1)
%           The offset of the transfer function adaption function.
%
% Returns
% -------
% dx : double, size(8, 1)
%   The derivatives of the states.
% y : double, size(1, 1)
%   The output, theta.

% MATLAB SUCKS! This is unbelievable. On the first iteration varargin is 1x2
% and after that it is 1x9.
%size(varargin, 2)

% Unpack varargin.
aux = varargin{end}{1};
m = zeros(4, 1);
b = zeros(4, 1);
for i=1:4
    if size(varargin, 2) == 2
        m(i) = varargin{1}(i);
        b(i) = varargin{1}(i + 4);
    elseif size(varargin, 2) == 9
        m(i) = varargin{i};
        b(i) = varargin{i + 4};
    else
        display('Matlab is stupid.')
    end
end

% First compute the gains at this time.
aux.pars(1:4) = m .* t + b;
% Compute the controller.
Yp = human(aux.pars, aux.timeDelay);
% Compute the plant and this time.
c1 = aux.m(1) * t + aux.b(1) + 1e-10;
c2 = aux.m(2) * t + aux.b(2) + 1e-10;
Yc = parallel(c1 * plant(aux.plantFirst), c2 * plant(aux.plantSecond));
% Compute the closed loop system.
Ys = feedback(Yp * Yc, 1);
% Convert to state space.
[A, B, C, D] = tf2ss(Ys.num{1}, Ys.den{1});
% Compute the derivatives of the states and the outputs.
dx = A * x + B * u;
y = C * x + D * u;
