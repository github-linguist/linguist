function bicycle = bicycle_state_space(bicycle, speed, varargin)
% function bicycle = bicycle_state_space(bicycle, speed, varargin)
%
% Returns the state space system of the Whipple model linearized about the
% nominal configuration and the supplied speed.
%
% Parameters
% ----------
% bicycle : char
%   The name of a bicycle in the parameters directory.
% speed : double
%   The forward speed at which to linearize the model about.
% varargin : char/cell array pairs, optional
%   Specify a subset of states, inputs or outputs by setting one of the
%   following: `states`, `inputs`, `outputs` as a cell array of
%   chars which include the subset variable names. Beaware that not all
%   state, input and output combinations are necessarily possible.
%   Valid state names: 'xP', 'yP', 'psi', 'phi', 'thetaB', 'thetaR', 'delta',
%       'thetaF', 'phiDot', 'thetaRDot', 'deltaDot'
%   Valid input names: 'tPhi', 'tDelta', 'fB'
%   Valid output names: 'xP', 'yP', 'psi', 'phi', 'thetaB', 'thetaR', 'delta',
%       'thetaF', 'xPDot', 'yPDot', 'psiDot', 'phiDot', 'thetaBDot',
%       'thetaRDot', 'deltaDot', 'thetaFDot', 'xQ', 'yQ'
%
% Returns
% -------
% bicycle : ss
%   The state space model of the bicycle.
%
% Notes
% -----
% The variable names are defined as in Meijaard2007.
%
% Examples
% --------
% bicycle = bicycle_state_space('Benchmark', 5.0, ...
%   'states', {'phi', 'phiDot', 'delta', 'deltaDot'}, ...
%   'inputs', {'tDelta'}, ...
%   'outputs', {'delta', 'phi'})

% get the directory which this m-file is in
S = dbstack('-completenames');
[CURRENT_DIRECTORY, ~, ~] = fileparts(S(1).file);

% load the paramaters
par = par_text_to_struct([CURRENT_DIRECTORY filesep 'parameters' ...
    filesep bicycle 'Par.txt']);

% generate the state space matrices
[A, B, C, D] = whipple_pull_force_abcd(par, speed);

% name the states, outputs and inputs
states = {'xP',
          'yP',
          'psi',
          'phi',
          'thetaB',
          'thetaR',
          'delta',
          'thetaF',
          'phiDot',
          'thetaRDot',
          'deltaDot'};

outputs = {'xP',
           'yP',
           'psi',
           'phi',
           'thetaB',
           'thetaR',
           'delta',
           'thetaF',
           'xPDot',
           'yPDot',
           'psiDot',
           'phiDot',
           'thetaBDot',
           'thetaRDot',
           'deltaDot',
           'thetaFDot',
           'xQ',
           'yQ'};

inputs = {'tPhi',
          'tDelta',
          'fB'};

defaultSettings.states = states;
defaultSettings.inputs = inputs;
defaultSettings.outputs = outputs;
% load in user supplied settings
if size(varargin, 2) >= 1
    userSettings = varargin_to_structure(varargin);
else
    userSettings = struct();
end
% combine the defaults with the user settings
settings = overwrite_settings(defaultSettings, userSettings);

% Will the system have the bare minimum states?
minStates = {'phi', 'delta', 'phiDot', 'deltaDot'};
if sum(ismember(settings.states, minStates)) < 4
    error(['You have not specified the minimum set of states. Please ' ...
        'include at least phi, delta, phiDot, and deltaDot'])
end

% Have state derivatives been specified that can't be computed with the
% specified states?
keepStates = find(ismember(states, settings.states));
removeStates = find(~ismember(states, settings.states));
for row = keepStates'
    for col = removeStates'
        if abs(A(row, col)) > 1e-10
            s = sprintf(['It is not possible to compute the derivative ' ...
                'of state %s because it depends on state %s'], ...
                states{row}, states{col});
            error(s)
        end
    end
end

removeInputs = find(~ismember(inputs, settings.inputs));

% Have outputs been specified that can't be computed with the specified
% states and inputs?
keepOutputs = find(ismember(outputs, settings.outputs));
for row = keepOutputs'
    for col = removeStates'
        if abs(C(row, col)) > 1e-10
            s = sprintf(['It is not possible to keep output %s because ' ...
                'it depends on state %s'], outputs{row}, ...
                states{col});
            error(s)
        end
    end
    for col = removeInputs'
        if abs(D(row, col)) > 1e-10
            s = sprintf(['It is not possible to keep output %s because ' ...
                'it depends on input %s'], outputs{row}, ...
                inputs{col});
            error(s)
        end
    end
end

removeOutputs = find(~ismember(outputs, settings.outputs));

A(removeStates, :) = [];
A(:, removeStates) = [];

B(removeStates, :) = [];
B(:, removeInputs) = [];

C(removeOutputs, :) = [];
C(:, removeStates) = [];

D(removeOutputs, :) = [];
D(:, removeInputs) = [];

states(removeStates) = [];
inputs(removeInputs) = [];
outputs(removeOutputs) = [];

% build the ss structure
bicycle = ss(A, B, C, D, ...
             'StateName', states, ...
             'OutputName', outputs, ...
             'InputName', inputs);
