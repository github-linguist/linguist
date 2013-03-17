function [name, order] = convert_variable(variable, output)
% Returns the name and order of the given variable in the output type.
%
% Parameters
% ----------
% variable : string
%   A variable name.
% output : string.
%   Either `moore`, `meijaard`, `data`.
%
% Returns
% -------
% name : string
%   The variable name in the given output type.
% order : double
%   The order of the variable in the list.

[coordinates, speeds, inputs] = get_variables();

columns = {'data', 'meijaard', 'moore'};

if find(ismember(coordinates, variable))

    [order, ~] = find(ismember(coordinates, variable));
    name = coordinates{order, find(ismember(columns, output))};

elseif find(ismember(speeds, variable))

    [order, ~] = find(ismember(speeds, variable));
    name = speeds{order, find(ismember(columns, output))};

elseif find(ismember(inputs, variable))

    [order, ~] = find(ismember(inputs, variable));
    name = inputs{order, find(ismember(columns, output))};

else
    error('Beep: Done typed yo variable name wrong')
end

function [coordinates, speeds, inputs] = get_variables()

coordinates = {'LongitudinalRearContact', 'xP', 'q1';
               'LateralRearContact', 'yP','q2';
               'YawAngle', 'psi','q3';
               'RollAngle', 'phi','q4';
               'PitchAngle', 'thetaB','q5';
               'RearWheelAngle', 'thetaR','q6';
               'SteerAngle', 'delta','q7';
               'FrontWheelAngle', 'thetaF','q8';
               'LongitudinalFrontContact', 'xQ','q9';
               'LateralFrontContact', 'yQ', 'q10'};

speeds = {'LongitudinalRearContactRate', 'xPDot', 'u1';
          'LateralRearContactRate', 'yPDot', 'u2';
          'YawRate', 'psiDot', 'u3';
          'RollRate', 'phiDot', 'u4';
          'PitchRate', 'thetaDot', 'u5';
          'RearWheelRate', 'thetaRDot', 'u6';
          'SteerRate', 'deltaDot', 'u7';
          'FrontWheelRate', 'thetaFDot','u8';
          'LongitudinalFrontContactRate', 'xQDot', 'u9';
          'LateralFrontContactRate', 'yQDot', 'u10'};

inputs = {'RollTorque', 'tPhi', 'T4';
          'RearWheelTorque', 'tThetaR', 'T6';
          'SteerTorque', 'tDelta', 'T7';
          'PullForce', 'fB', 'F'};
