function data = load_bikes(bikes, input)
% function data = load_bikes(bikes, input)
% Returns the data for a set of bicycles at three speeds.
%
% Parameters
% ----------
% bikes : cell array
%   A cell array that lists the bicyle short names.
% input : string
%   'Steer' or 'Roll'
%
% Returns
% -------
% data : structure
%   A structure containg a node for each bicycle and each speed.

speeds = [2.5, 5.0, 7.5];
speedNames = {'Slow', 'Medium', 'Fast'};

% these are the gains that were selected manually by Ron Hess, taken from
% the paper
gains.Benchmark.Slow   = [22.0, -0.090, 23.3, 0.058, 0.195]; % place holder
gains.Browserins.Slow  = [22.0, -0.090, 23.3, 0.058, 0.195];
gains.Browser.Slow     = [20.5, -0.086, 24.1, 0.053, 0.199];
% the gains in the paper give an unstable bike, the uncommented one were the ones from his
% simulink model
gains.Pista.Slow       = [22.3000,-0.1300,15.6410,0.0645,0.1990]; %[22.3, -0.130, 15.6, 0.662, 0.198];
gains.Fisher.Slow      = [23.0, -0.120, 17.7, 0.065, 0.198];
gains.Yellow.Slow      = [18.0, -0.110, 20.2, 0.062, 0.200];
gains.Yellowrev.Slow   = [48.0, -0.070, 27.9, 0.063, 0.191];

gains.Benchmark.Medium  = [ 46.5, -0.052, 12.8, 0.177, 0.097];
gains.Browserins.Medium = [ 48.0, -0.080, 9.03, 0.161, 0.097];
gains.Browser.Medium    = [ 43.0, -0.087, 8.50, 0.173, 0.100];
gains.Pista.Medium      = [ 49.0, -0.080, 8.06, 0.170, 0.101];
gains.Fisher.Medium     = [ 50.5, -0.084, 8.26, 0.168, 0.100];
gains.Yellow.Medium     = [ 39.0, -0.085, 8.61, 0.160, 0.101];
gains.Yellowrev.Medium  = [105.0, -0.070, 8.90, 0.165, 0.100];

gains.Benchmark.Fast   = [ 74.0, -0.063, 6.31, 0.332, 0.065]; % place holder
gains.Browserins.Fast  = [ 74.0, -0.063, 6.31, 0.332, 0.065];
gains.Browser.Fast     = [ 68.0, -0.060, 6.74, 0.330, 0.065];
gains.Pista.Fast       = [ 80.0, -0.058, 5.82, 0.321, 0.066];
gains.Fisher.Fast      = [ 82.0, -0.062, 5.83, 0.315, 0.065];
gains.Yellow.Fast      = [ 61.0, -0.063, 6.34, 0.345, 0.065];
gains.Yellowrev.Fast   = [170.0, -0.050, 6.45, 0.300, 0.066];

% load the data for all speeds for all the bikes
for i = 1:length(bikes)
    for j = 1:length(speeds)
        data.(bikes{i}).(speedNames{j}) = ...
            generate_data(bikes{i}, speeds(j), 'input', input, ...
                          'gains', gains.(bikes{i}).(speedNames{j}));
    end
end
