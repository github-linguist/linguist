function [x, y, t] = lane_change(start, width, slope, pathLength, speed, num, ...
                                 type, varargin)
% Generates the time and coordinates for either a single or double lane change
% manuever at a particular speed.
%
% Parameters
% ----------
% start : float
%   The starting point along the x axis in meters.
% width : float
%   The width of the lane deviation.
% slope : float
%   The slope of the lane change.
% pathLength : float
%   The length of path.
% speed : float
%   Speed of travel.
% num : integer
%   Number of time steps.
% type : string
%   Either 'single' or 'double'. A double lane change return to x = 0.
% laneLength : float, optional
%   Length of the lane for a double lane change.
%
% Returns
% -------
% x : matrix, (num, 1)
%   The longitudinal path.
% y : matrix, (num, 1)
%   The lateral path.
% t : matrix, (num, 1)
%   Time.

x = 0:pathLength / num:pathLength;
x = x';
t = x / speed;

y = zeros(length(x), 1);
endOfSlope = width / slope + start;
slopeInd = find((x > start) & (x <= endOfSlope));
y(slopeInd) = slope * (x(slopeInd) - start);
if strcmp(type, 'single')
    theRest = slopeInd(end) + 1:length(y);
    y(theRest) = width * ones(length(theRest), 1);
elseif strcmp(type, 'double');
    if length(varargin) < 1
        error('Double lane change needs length of lane.')
    else
        laneLength = varargin{1};
        startOfSlope = start + laneLength - width / slope;
        lane = find((x > endOfSlope) & (x <= startOfSlope));
        y(lane) = width * ones(length(lane), 1);
        downSlope = find((x > startOfSlope) & (x <= start + laneLength));
        y(downSlope) = slope * (start + laneLength - x(downSlope));
    end
end
