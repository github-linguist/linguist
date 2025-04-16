% This file identifies a series of models for a time varying plant.

% k1, k2, k3, k4, tau, zetanm, wnm, zetafs, wfs

%data = load_data('jason_adapt.mat');
%filename = 'adapt';
%guess.plantOne = [4.85, 1.79, 20, 20, 0.2, 0.707, 10, 0.707, 65];
%guess.plantTwo = [3.36, 9.49, 20, 0, 0.2, 0.707, 10, 0.707, 65];
%plantNum.plantOne = 1;
%plantNum.plantTwo = 5;

data = load_data('adapt_hard.mat');
filename = 'hard-adapt';
guess.plantOne = [1.23, 0.354, 0.2, 20.0, 0.2, 0.707, 10, 0.707, 65];
guess.plantTwo = [4.85, 1.79, 20, 20, 0.2, 0.707, 10, 0.707, 65];
plantNum.plantOne = 6;
plantNum.plantTwo = 1;

t = [0, 30, 40, 50, 60, 90];

sections = {'plantOne', 'adaptOne', 'adaptTwo', 'adaptThree', 'plantTwo'};
for i = 1:length(sections)
    secData.(sections{i}) = data(t(i) / data.Ts + 1:t(i + 1) / data.Ts);
end

% compute the slope and offset for each gain for initial guesses
kP1 = guess.plantOne(1:4)';
kP2 = guess.plantTwo(1:4)';
gainSlopeOffset = [t(2) * eye(4), eye(4); t(5) * eye(4), eye(4)] \ [kP1; kP2];
m = gainSlopeOffset(1:4);
b = gainSlopeOffset(5:8);

for i = 1:length(sections)

    if strcmp(sections{i}, sections{1}) || strcmp(sections{i}, sections{end})
        display('boo')
    else
        if i > 1
            % use the best fit gains from the previous section
            guess.(sections{i}) = result.(sections{i - 1}).fit.par;
        else
            currentGuess = m .* (t(i) + t(i + 1)) / 2 + b;
            guess.(sections{i}) = [currentGuess', guess.plantOne(5:end)];
        end
        percent = ((t(i) + t(i + 1)) / 2 - t(2)) / (t(5) - t(2));
        plantNum.(sections{i}) = {plantNum.plantOne, plantNum.plantTwo, percent};
    end

    result.(sections{i}) = find_structural_gains(secData.(sections{i}), ...
        guess.(sections{i}), plantNum.(sections{i}), 'warning', false, ...
        'randomGuess', true);

    [yh, vaf, x0] = compare(secData.(sections{i}), result.(sections{i}).fit);
    result.(sections{i}).vaf = vaf(1, 1, 1);
    display(sprintf('The self validation VAF is %f.', vaf(1, 1, 1)))

    p = plantNum.(sections{i});
    if size(plantNum.(sections{i}), 2) > 1
        result.(sections{i}).plant = plant(p{:});
    else
        result.(sections{i}).plant = plant(p);
    end
end

save(['data/' filename '-results.mat'], 'sections', 'guess', 'plantNum', 'result')

for i = 1:length(sections)
    result.(sections{i}).fig = figure;
    compare(secData.(sections{i}), result.(sections{i}).fit);
    saveas(result.(sections{i}).fig, ['plots/' filename '-' sections{i} '.png'])
    [yh, fit, x0] = compare(secData.(sections{i}), result.(sections{i}).fit);
    display('-----------------')
    display('The task plant is:')
    display(result.(sections{i}).plant)
    display(sprintf('The order of the closed loop system is %u.', ...
        size(result.(sections{i}).mod.A, 1)))
    display(sprintf('The gain guesses: k1=%f, k2=%f, k3=%f, k4=%f', ...
        result.(sections{i}).mod.par(1:4)))
    display(sprintf('The identified gains: k1=%f+\\-%f, k2=%f+\\-%f, k3=%f+\\-%f, k4=%f+\\-%f', ...
        result.(sections{i}).fit.par(1), result.(sections{i}).uncert(1), ...
        result.(sections{i}).fit.par(2), result.(sections{i}).uncert(2), ...
        result.(sections{i}).fit.par(3), result.(sections{i}).uncert(3), ...
        result.(sections{i}).fit.par(4), result.(sections{i}).uncert(4)))
    display(sprintf('The self validation VAF is %f.', ...
        result.(sections{i}).vaf))
end
