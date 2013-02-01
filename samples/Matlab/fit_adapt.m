data = load_data('jason_adapt.mat');

t0 = 0;
t1 = 30;
t2 = 60;
t3 = 90;

dataPlantOne = data(1:t1 / data.Ts);
dataAdapting = data(t1 / data.Ts:t2 / data.Ts);
dataPlantTwo = data(t2 / data.Ts:end);

% k1, k2, k3, k4, tau, zetanm, wnm, zetafs, wfs

% ron's guess
guessPlantOne = [4.85, 1.79, 20, 20, 0.2, 0.707, 10, 0.707, 65];
% best solution from ron's guess
%guessPlantOne = [4.1129, 2.1327, 52.3747, 48.6997, 0.2, 0.707, 10, 0.707, 65];
resultPlantOne = find_structural_gains(dataPlantOne, guessPlantOne, 1);
[yh, fit, x0] = compare(dataPlantOne, resultPlantOne.fit);
display(sprintf('The self validation VAF is %f.', fit(1, 1, 1)))

% ron's guess
guessPlantTwo = [3.36, 9.49, 20, 0, 0.2, 0.707, 10, 0.707, 65];
% best solution from ron's guess
%guessPlantTwo = [2.6686, 7.0431, 14.4623, 3.1532, 0.2, 0.707, 10, 0.707, 65];
resultPlantTwo = find_structural_gains(dataPlantTwo, guessPlantTwo, 5);
[yh, fit, x0] = compare(dataPlantTwo, resultPlantTwo.fit);
display(sprintf('The self validation VAF is %f.', fit(1, 1, 1)))

% compute the slope and offset for each gain for initial guesses
kP1 = resultPlantOne.fit.par(1:4);
kP2 = resultPlantTwo.fit.par(1:4);
gainSlopeOffset = [t1 * eye(4), eye(4); t2 * eye(4), eye(4)] \ [kP1; kP2];

aux.pars = guessPlantOne; % this only uses tau through wfs
aux.timeDelay = true;
aux.plantFirst = 1; % 1 / s
aux.plantSecond = 5; % 5 / (s + 10)
% compute the slope and offset of the plant for t1 < t < t2
plantOneSlopeOffset = [t1, 1; t2, 1] \ [1; 0];
plantTwoSlopeOffset = [t1, 1; t2, 1] \ [0; 1];
aux.m = [plantOneSlopeOffset(1); plantTwoSlopeOffset(1)];
aux.b = [plantOneSlopeOffset(2); plantTwoSlopeOffset(2)];

%[dx, y] = adapting_structural_model(45, ones(8, 1), 10, gainSlopeOffset, {aux});

% NOTE: 'FileArgument' has to be a cell array, the is why aux is in
% brackets. Also, if you pass the parameters in as a vector here, as I have,
% they get mapped to a structure and your ode file/function must accept each
% parameter as individual arguments.
mod = idnlgrey('adapting_structural_model', [1, 1, 8], gainSlopeOffset, ...
    zeros(8, 1), 0, 'FileArgument', {aux}, 'InputName', 'thetac', ...
    'OutputName', 'theta');

fit = pem(dataAdapting, mod);

compare(dataAdapting, fit);
