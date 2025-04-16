function Yc = plant(varargin)
% function Yc = plant(varargin)
%
% Returns the system plant given a number.
%
% Parameters
% ----------
% varargin : variable
%   Either supply a single argument {num} or three arguments {num1, num2,
%   ratio}. If a single argument is supplied, then one of the six transfer
%   functions will be chosen from the list. If three arguments are chosen, a
%   parallel sum of the two plants will be returned.
%
%   Option 1
%   --------
%   num : integer, {1, 2, 3, 4, 5, 6}
%       A number between 1 and 6 corresponding to the five plants.
%   Option 2
%   --------
%   num1 : integer, {1, 2, 3, 4, 5, 6}
%       A number between 1 and 6 corresponding to the five plants.
%   num2 : integer, {1, 2, 3, 4, 5, 6}
%       A number between 1 and 6 corresponding to the five plants.
%   percent : double
%       The percentage multiplier of the first plant. Should be between 0
%       and 1. The percentage multiplier of the second plant will be 1 -
%       percent.
%
% Returns
% -------
% Either a single plant or a parallel sum of scaled plants.
%
% Option 1
% --------
% Yc : transfer function
%   1 : 1 / s
%   2 : 1 / s(s + 1)
%   3 : 1 / s(s + 0.2)
%   4 : 10 / (s + 10)
%   5 : 5 / (s + 10)
%   6 : 10 / (s^2 +.2 * s)
%
% Option 2
% --------
% Yc : transfer function
%   Yc = percent * Yc1 + (1 - percent) * Yc2, where Yc1 and Yc2 are plants
%   from the list shown in Option 1.

if size(varargin, 2) > 1
    if 0 <= varargin{3} & varargin{3} <= 1
        Yc = parallel(varargin{3} * choose_plant(varargin{1}), ...
            (1 - varargin{3}) * choose_plant(varargin{2}));
    else
        error('Ratio must be between 0 and 1.')
    end
else
    Yc = choose_plant(varargin{1});
end

function p = choose_plant(num)
if num == 1;
    p = tf(1.0, [1.0, 0.0]);
elseif num == 2;
    p = tf(1.0, [1.0, 1.0, 0.0]);
elseif num == 3;
    p = tf(1.0, [1.0, 0.2, 0.0]);
elseif num == 4;
    p = tf(10.0, [1.0, 10.0]);
elseif num == 5;
    p = tf(5.0, [1.0, 10.0]);
elseif num == 6;
    p = tf(10.0, [1.0, 0.2, 0.0]);
else
    display('Invalid plant number.')
end
