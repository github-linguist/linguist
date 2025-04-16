gains = [76.3808, -0.0516, 7.2456, 0.2632, 0.0708];
wnm = 30;
zetanm = 0.707;

data = generate_data('Rigid', 7.0, 'gains', gains, 'neuroFreq', wnm, ...
    'loopTransfer', 0, 'handlingQuality', 0, 'simulate', 0);

bicycle = ss(data.modelPar.A, data.modelPar.B, data.modelPar.C, ...
    data.modelPar.D);

bicycle.StateName = {'xP', 'yP', 'psi', 'phi', 'thetaB', 'thetaR', 'delta', ...
             'thetaF', 'phiDot', 'thetaRDot', 'deltaDot'};
bicycle.OutputName = {'xP', 'yP', 'psi', 'phi', 'thetaB', 'thetaR', 'delta', ...
             'thetaF', 'xPDot', 'yPDot', 'psiDot', 'phiDot', ...
             'thetaBDot', 'thetaRDot', 'deltaDot', 'thetaFDot', 'xQ', 'yQ'};
bicycle.InputName = {'tPhi', 'tDelta', 'fB'};

inputs = {'fB'};
outputs = [bicycle.OutputName; 'tDelta'];

analytic = system_state_space('lateral', bicycle, gains, [wnm, zetanm], inputs, outputs);

numeric = ss(data.system.A, data.system.B, data.system.C, data.system.D);
%numeric.StateName = data.bicycle.states;
%numeric.InputName = data.bicycle.inputs;
%numeric.OutputName = data.bicycle.outputs;

figure()
pzplot(analytic, numeric)

% plot the transfer function roll-rate/lateral force for both
figure()
hold all
% plot my analytic model
[num, den] = ss2tf(analytic.A, analytic.B, analytic.C, analytic.D, 1);
mine = tf(num(find(strcmp('phiDot', outputs)), :), den);
bode(tf(num(find(strcmp('phiDot', outputs)), :), den))
% plot the data from the simulink model
bode(tf(data.forceTF.PhiDot.num, data.forceTF.PhiDot.den))
[num, den] = ss2tf(numeric.A, numeric.B, numeric.C, numeric.D, 1);
bode(tf(num(12, :), den))

display('Analytic Eigenvalues')
eig(analytic.A)
display('Numeric Eigenvalues')
eig(numeric.A)

% Now see if the heading tracking works.
gains = [76.3808, -0.0516, 7.2456, 0.2632];
wnm = 30;
zetanm = 0.707;

par = par_text_to_struct('parameters/RigidPar.txt');
[A, B, C, D] = whipple_pull_force_ABCD(par, 7.0);
bicycle = ss(A([3, 4, 7, 9, 11], [3, 4, 7, 9, 11]), ...
    B([3. 4, 7, 9, 11], [2, 3]), eye(5), 0);
bicycle.StateName = {'psi', 'phi', 'delta', 'phiDot', 'deltaDot'};
bicycle.OutputName = {'psi', 'phi', 'delta', 'phiDot', 'deltaDot'};
bicycle.InputName = {'tDelta', 'fB'};

inputs = {'fB'};
outputs = [bicycle.OutputName; 'tDelta'];

analytic = system_state_space('heading', bicycle, gains, [wnm, zetanm], inputs, outputs);
% the following two should be the same
analytic.A(end, :)
bottomRow = [-wnm^2 * prod(gains), -wnm^2 * prod(gains(1:3)), ...
    -wnm^2 * gains(1), -wnm^2 * prod(gains(1:2)), 0, -wnm^2, ...
    -2 * wnm * zetanm]
