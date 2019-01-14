function create_ieee_paper_plots(data, rollData)
% Creates all of the figures for the IEEE paper.
%
% Parameters
% ----------
% data : structure
%   A structure contating the data from generate_data.m for all of the bicycles
%   and speeds for the IEEE paper.
% rollData : structure
%   The data for a single bicycle at a single speed with roll torque as the
%   input.

global goldenRatio
% used for figure width to height ratio
goldenRatio = (1 + sqrt(5)) / 2;

% create a plot directory if one doesn't already exist
if exist('plots/', 'dir') ~= 7
    mkdir('plots/')
end

% Define some linestyles and colors for each of the six bicycles
linestyles = {'-', '-', '-.', ...
              '--', '-.', '--'};
colors = {'k', ...
          [0.5, 0.5, 0.5], ...
          [0.5, 0.5, 0.5], ...
          'k', ...
          'k', ...
          [0.5, 0.5, 0.5]};

loop_shape_example(data.Benchmark.Medium, 'Steer')
loop_shape_example(rollData, 'Roll')
plot_io_roll(rollData, 'Distance')
plot_io_roll(rollData, 'Time')
open_loop_all_bikes(data, linestyles, colors)
handling_all_bikes(data, rollData, linestyles, colors)
path_plots(data, linestyles, colors)
var = {'delta', 'phi', 'psi', 'Tdelta'};
io = {'output', 'output', 'output', 'input'};
typ = {'Distance', 'Time'};
for i = 1:length(var)
    for j = 1:length(typ)
        plot_io(var{i}, io{i}, typ{j}, data, linestyles, colors)
    end
end
phase_portraits(data.Benchmark.Medium)
eigenvalues(data, linestyles, colors)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function loop_shape_example(bikeData, input)
% Creates the example loop shaping for the bicycle at medium speed.
%
% Parameters
% ----------
% bikeData : structure
%   Contains data for a single bicycle at a single speed.
% input : string
%   'Steer' or 'Roll' depending on what input was used to control the bicycle.

global goldenRatio

% closed loop bode plots
figure()
figWidth = 5.0;
figHeight = figWidth / goldenRatio;
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'OuterPosition', [424, 305 - 50, 518, 465], ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

freq = {0.1, 20.0};

hold all

closedLoops = bikeData.closedLoops;

% make sure all bode plots display 'rad/s' instead of 'rad/sec'
bops = bodeoptions;
bops.FreqUnits = 'rad/s';

if strcmp(input, 'Steer')
    linestyles = {'', '', '-.', '-.', '-', '-.', '-.', '-'};
    gray = [0.6, 0.6, 0.6];
    colors = {'k', 'k', 'k', gray, 'k', 'k', gray, 'k'};
    % the closed delta loop
    deltaNum = closedLoops.Delta.num;
    deltaDen = closedLoops.Delta.den;
    bodeplot(tf(deltaNum, deltaDen), freq, bops);
    % a typical neuromuscular model
    neuroNum = 2722.5;
    neuroDen = [1, 13.96, 311.85, 2722.5];
    bodeplot(tf(neuroNum, neuroDen), freq, bops);
    whichLines = 5:-1:3;
elseif strcmp(input, 'Roll')
    linestyles = {'', '', '-', '-'};
    colors = {'k', 'k', 'k', 'k'};
    whichLines = 4:-1:2;
else
    error('Bad input, use Steer or Roll')
end

% the closed phi dot loop
phiDotNum = closedLoops.PhiDot.num;
phiDotDen = closedLoops.PhiDot.den;
closedBode = bodeplot(tf(phiDotNum, phiDotDen), freq, bops);

hold off

% clean it up
opts = getoptions(closedBode);
if strcmp(input, 'Steer')
    opts.YLim = {[-45, 20], [-360, 90]};
else
    opts.YLim = {[-30, 10], [-180, 90]};
end
opts.PhaseMatching = 'on';
opts.PhaseMatchingValue = 0;
opts.Title.String = '';
setoptions(closedBode, opts)

% find all the lines in the current figure
lines = findobj(gcf, 'type', 'line');
for i = 3:length(lines)
    set(lines(i), 'LineStyle', linestyles{i}, ...
                  'Color', colors{i}, ...
                  'LineWidth', 2.0)
end

% there seems to be a bug such that the xlabel is too low, this is a hack to
% get it to work
raise = 0.05;
plotAxes = findobj(gcf, 'type', 'axes');
set(plotAxes, 'XColor', 'k', 'YColor', 'k')
curPos1 = get(plotAxes(1), 'Position');
curPos2 = get(plotAxes(2), 'Position');
set(plotAxes(1), 'Position', curPos1 + [0, raise, 0, 0])
set(plotAxes(2), 'Position', curPos2 + [0, raise, 0, 0])
xLab = get(plotAxes(1), 'Xlabel');
set(xLab, 'Units', 'normalized')
set(xLab, 'Position', get(xLab, 'Position') + [0, raise + 0.05, 0])

% make the tick labels smaller
set(plotAxes, 'Fontsize', 8)
if strcmp(input, 'Steer')
    legWords = {'$\delta$ Loop',
                'Neuromuscular model from [27]',
                '$\dot{\phi}$ Loop'};
elseif strcmp(input, 'Roll')
    legWords = {'$\dot{\phi}$ Loop'};
end
closeLeg = legend(lines(whichLines), ...
                  legWords, ...
                  'Location', 'Southwest', ...
                  'Interpreter', 'Latex', ...
                  'Fontsize', 8);

% add the annotation showing a 10 dB peak
if strcmp(input, 'Steer')
    axes(plotAxes(2))
    db1 = text(2.7, 5.0, '~10dB');
    db2 = text(2.5, -10.0, '~10dB');
    set([db1, db2], 'Fontsize', 8)
    dArrow1 = annotation('doublearrow', ...
                         [0.7, 0.7], ...
                         [0.755 + raise, 0.818 + raise]);
    annotation('line', [0.69, 0.87], [0.818 + raise, 0.818 + raise])
    dArrow2 = annotation('doublearrow', ...
                         [0.685, 0.685], ...
                         [0.665 + raise, 0.725 + raise]);
    annotation('line', [0.675, 0.87], [0.725 + raise, 0.725 + raise])
    set([dArrow1, dArrow2], 'Head1width', 3, 'Head1length', 3, ...
        'Head2width', 3, 'Head2length', 3)
else
    axes(plotAxes(2))
    db1 = text(0.67, -3.7, '~10dB');
    set(db1, 'Fontsize', 8)
    dArrow = annotation('doublearrow', ...
                        [0.5, 0.5], ...
                        [0.697 + raise, 0.795 + raise]);
    set(dArrow, 'Head1width', 3, 'Head1length', 3, ...
        'Head2width', 3, 'Head2length', 3)
    annotation('line', [0.49, 0.75], [0.795 + raise, 0.795 + raise])
end

filename = ['benchmark' input 'Closed'];
pathToFile = ['plots' filesep filename];
print(gcf, '-deps2c', '-loose', [pathToFile '.eps'])
fix_ps_linestyle([pathToFile '.eps'])

% open loop plots
figure()
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

openLoops = bikeData.openLoops;

hold all

num = openLoops.Phi.num;
den = openLoops.Phi.den;
bodeplot(tf(num, den), freq, bops);

num = openLoops.Psi.num;
den = openLoops.Psi.den;
bodeplot(tf(num, den), freq, bops);

num = openLoops.Y.num;
den = openLoops.Y.den;
openBode = bodeplot(tf(num, den), freq, bops);

hold off

% clean it up
opts = getoptions(openBode);
opts.Title.String = '';
opts.YLim = {[-80, 20], [-540, -60]};
opts.PhaseMatching = 'on';
opts.PhaseMatchingValue = 0;
setoptions(openBode, opts)

% find all the lines in the current figure
lines = findobj(gcf, 'type', 'line');
linestyles = {'', '', '-.', '-', '--', '-.', '-', '--'};
for i = 3:length(lines)
    set(lines(i), 'LineStyle', linestyles{i}, ...
                  'Color', 'k', ...
                  'LineWidth', 2.0)
end


plotAxes = findobj(gcf, 'type', 'axes');
set(plotAxes, 'Fontsize', 8, 'XColor', 'k', 'YColor', 'k')
closeLeg = legend(lines(8:-1:6), ...
                  {'$\phi$ Loop', '$\psi$ Loop','$y$ Loop'}, ...
                  'Location', 'Southwest', ...
                  'Interpreter', 'Latex');

% add zero crossing lines
%axes(plotAxes(1))
%line([0.1, 20], [-180, -180], 'Color', 'k')
axes(plotAxes(2))
line([0.1, 20], [0, 0], 'Color', 'k')

% add some lines and labels for the cross over frequencies
if strcmp(input, 'Steer')
    wc = 2;
    wShift = [0.42, 0.35, 0.175];
else strcmp(input, 'Roll')
    wc = 1.5;
    wShift = [0.31, 0.26, 0.1325];
end
axes(plotAxes(2))
hold on
gray = [0.5, 0.5, 0.5];

line([wc, wc], [-40, 0], 'Color', gray)
text(wc - wShift(1), -43, ['$\omega_c=' num2str(wc) '$'], ...
     'Interpreter', 'Latex', 'Fontsize', 8)

line([wc / 2, wc / 2], [-30, 0], 'Color', gray)
text(wc / 2 - wShift(2), -33, ['$\omega_c/2=' num2str(wc / 2) '$'], ...
     'Interpreter', 'Latex', 'Fontsize', 8)

line([wc / 4, wc / 4], [-20, 0], 'Color', gray)
text(wc / 4 - wShift(3), -23, ['$\omega_c/4=' num2str(wc / 4) '$'], ...
     'Interpreter', 'Latex', 'Fontsize', 8)

hold off

curPos1 = get(plotAxes(1), 'Position');
curPos2 = get(plotAxes(2), 'Position');
set(plotAxes(1), 'Position', curPos1 + [0, raise, 0, 0])
set(plotAxes(2), 'Position', curPos2 + [0, raise, 0, 0])
xLab = get(plotAxes(1), 'Xlabel');
set(xLab, 'Units', 'normalized')
set(xLab, 'Position', get(xLab, 'Position') + [0, raise + 0.05, 0])

filename = ['benchmark' input 'Open.eps'];
pathToFile = ['plots' filesep filename];
print(gcf, '-deps2c', '-loose', pathToFile)
fix_ps_linestyle(pathToFile)

% handling qualities plot
num = bikeData.handlingMetric.num;
den = bikeData.handlingMetric.den;
w = linspace(0.01, 20, 200);
[mag, phase, freq] = bode(tf(num, den), w);
figure()
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

hold on

metricLine = plot(freq, mag(:)', 'k-', 'Linewidth', 2.0);
level1 = line([0, 20], [5, 5]);
level2 = line([0, 20], [8, 8]);
set(level1, 'Color', 'k', 'Linestyle', '--', 'Linewidth', 2.0)
set(level2, 'Color', 'k', 'Linestyle', '--', 'Linewidth', 2.0)

ylim([0, 10]);

ylabel('Handling Quality Metric')
xlabel('Frequency (rad/s)')
text(3, 3, 'Level 1')
text(3, 6.5, 'Level 2')
text(3, 9, 'Level 3')
box on

filename = ['benchmark' input 'Handling.eps'];
pathToFile = ['plots' filesep filename];
print(gcf, '-deps2', '-loose', pathToFile)
fix_ps_linestyle(pathToFile)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function open_loop_all_bikes(data, linestyles, colors)
% Creates open loop Bode plots of all the bikes.

global goldenRatio

bikes = fieldnames(data);

figure()
figWidth = 5.0;
figHeight = figWidth / goldenRatio;
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

freq = {0.1, 20.0};

% make sure all bode plots display 'rad/s' instead of 'rad/sec'
bops = bodeoptions;
bops.FreqUnits = 'rad/s';

hold all
for i = 2:length(bikes)
    num = data.(bikes{i}).Medium.openLoops.Phi.num;
    den = data.(bikes{i}).Medium.openLoops.Phi.den;
    openBode = bodeplot(tf(num, den), freq, bops);
end
hold off

% clean it up
opts = getoptions(openBode);
%opts.Title.String = '$\phi$ Open Loop Bode Diagrams at 5 m/s';
opts.Title.String = '';
%opts.Title.Interpreter = 'Latex';
opts.YLim = {[-30, 10], [-540, -90]};
opts.PhaseMatching = 'on';
opts.PhaseMatchingValue = 0;
setoptions(openBode, opts)

% find all the lines in the current figure
plotAxes = findobj(gcf, 'type', 'axes');
magLines = findobj(plotAxes(2), 'type', 'line');
phaseLines = findobj(plotAxes(1), 'type', 'line');

for i = 2:length(magLines)
    set(magLines(i), ...
        'LineStyle', linestyles{i - 1}, ...
        'Color', colors{i - 1}, ...
        'LineWidth', 1.0)
    set(phaseLines(i), ...
        'LineStyle', linestyles{i - 1}, ...
        'Color', colors{i - 1}, ...
        'LineWidth', 1.0)
end

closeLeg = legend(magLines(2:7), ...
                  {'1', '2', '3', '4', '5', '6'}, ...
                  'Location', 'Southwest', ...
                  'Fontsize', 8);

set(plotAxes, 'YColor', 'k', 'XColor', 'k', 'Fontsize', 8)

% add a zero lines
axes(plotAxes(1))
line([0.1, 20], [-180, -180], 'Color', 'k')
axes(plotAxes(2))
line([0.1, 20], [0, 0], 'Color', 'k')

% raise the axes cause the xlabel is cut off
raise = 0.05;
curPos1 = get(plotAxes(1), 'Position');
curPos2 = get(plotAxes(2), 'Position');
set(plotAxes(1), 'Position', curPos1 + [0, raise, 0, 0])
set(plotAxes(2), 'Position', curPos2 + [0, raise, 0, 0])
xLab = get(plotAxes(1), 'Xlabel');
set(xLab, 'Units', 'normalized')
set(xLab, 'Position', get(xLab, 'Position') + [0, raise + 0.05, 0])

filename = 'openBode.eps';
pathToFile = ['plots' filesep filename];
print(pathToFile, '-deps2c', '-loose')
fix_ps_linestyle(pathToFile)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function handling_all_bikes(data, rollData, linestyles, colors)
% Creates handling quality metric for all bikes.
%
% Parameters
% ----------
% data : structure
%   Contains data for all bikes a the three speeds for steer input.
% rollData : structure
%   Contains the data for the benchmark bike with roll input at medium speed.
% linestyles : cell array
%   Linestyle strings, one for each of the six bikes.
% colors : cell array
%   Colorspecs for each of the six bikes.

global goldenRatio

bikes = fieldnames(data);
figure()
figWidth = 5.0;
figHeight = figWidth / goldenRatio;
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

w = linspace(0.01, 20, 200);
speedNames = fieldnames(data.Browser);
fillColors = {[0.82, 0.82, 0.82]
              [0.68, 0.68, 0.68]
              [0.95, 0.95, 0.95]};
hold all

% plot the background area for each family of curves
for j = 1:length(speedNames)
    % get the max values for the set of curves
    magnitudes = zeros(length(w), length(bikes) - 1);
    for i = 2:length(bikes)
        num = data.(bikes{i}).(speedNames{j}).handlingMetric.num;
        den = data.(bikes{i}).(speedNames{j}).handlingMetric.den;
        [mag, phase, freq] = bode(tf(num, den), w);
        magnitudes(:, i - 1) = mag(:)';
    end
    maxMag = max(magnitudes, [], 2);
    % fill the area under the curve
    area(freq, maxMag, ...
         'Facecolor', fillColors{j}, ...
         'Edgecolor', 'none')
end

% this makes sure that the edges of the fill area don't cover the axes
set(gca, 'Layer', 'top')

% plot the actual curves
for j = 1:length(speedNames)
    metricLines = zeros(length(bikes) - 1, 1);
    for i = 2:length(bikes)
        num = data.(bikes{i}).(speedNames{j}).handlingMetric.num;
        den = data.(bikes{i}).(speedNames{j}).handlingMetric.den;
        [mag, phase, freq] = bode(tf(num, den), w);
        metricLines(i - 1) = plot(freq, mag(:)', ...
                                 'Color', colors{i - 1}, ...
                                 'Linestyle', linestyles{i - 1}, ...
                                 'Linewidth', 2.0);
    end
end

% add the roll input bike
num = rollData.handlingMetric.num;
den = rollData.handlingMetric.den;
[mag, phase, freq] = bode(tf(num, den), w);
rollLine = plot(freq, mag(:)', 'k', 'Linewidth', 2.0, 'Linestyle', ':');

hold off

% move the roll input line down so it shows on the legend
chil = get(gca, 'Children');
legLines = [chil(end:-1:14)', rollLine];
legend(legLines, [{'2.5 m/s', '5.0 m/s', '7.5 m/s'}, ...
        {'1', '2', '3', '4', '5', '6', 'Hands-free @ 5 m/s'}], ...
        'Fontsize', 8)

ylim([0, 20]);
level1 = line([0, 20], [5, 5]);
level2 = line([0, 20], [8, 8]);
set(level1, 'Color', 'k', 'Linestyle', '--', 'Linewidth', 1.0)
set(level2, 'Color', 'k', 'Linestyle', '--', 'Linewidth', 1.0)
ylabel('Handling Quality Metric')
xlabel('Frequency (rad/s)')
text(3.1, 4.3, 'Level 1')
text(1.9, 6.5, 'Level 2')
text(3, 15, 'Level 3')
box on

set(gca, 'YColor', 'k')

filename = 'handling.eps';
pathToFile = ['plots' filesep filename];
print(pathToFile, '-deps2c', '-loose')
fix_ps_linestyle(pathToFile)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function path_plots(data, linestyles, colors)
% Creates a plot of the path tracking for all bikes at all speeds.

global goldenRatio

bikes = fieldnames(data);
speedNames = fieldnames(data.Browser);

figure()
figWidth = 5.0;
figHeight = figWidth / goldenRatio;
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

hold all

% shifts the paths by this many meters
shift = [0, 15, 35];
for j = 1:length(speedNames)
    time = data.(bikes{2}).(speedNames{j}).time;
    path = data.(bikes{2}).(speedNames{j}).path;
    speed = data.(bikes{2}).(speedNames{j}).speed;
    plot(time * speed + shift(j), -path * j, 'k-')
    for i = 2:length(bikes)
        x = data.(bikes{i}).(speedNames{j}).outputs(:, 17);
        x = x + shift(j);
        y = data.(bikes{i}).(speedNames{j}).outputs(:, 18);
        plot(x, -y * j, ...
             'Linestyle', linestyles{i - 1}, ...
             'Color', colors{i - 1}, ...
             'Linewidth', 0.75)
    end
    [minPath, minPathI] = min(-path * j);
    dis = time * speed + shift(j);
    lab = sprintf('%1.1f m/s', speed);
    text(dis(minPathI) - 15, minPath - 0.4, lab)
end

hold off

% change the y tick labels to positive and to reflect the 2 meter width
set(gca, 'YTick', [-7, -6, -4, -2, 0, 1])
set(gca, 'YTickLabel', {'', '2', '2', '2', '0', ''})

xlim([30 200])
box on
legend(['Path', {'1', '2', '3', '4', '5', '6'}], ...
       'Fontsize', 8, 'Location', 'Southeast')
xlabel('Distance (m)')
ylabel('Lateral Deviation (m)')
filename = 'paths.eps';
pathToFile = ['plots' filesep filename];
print(pathToFile, '-deps2c', '-loose')
fix_ps_linestyle(pathToFile)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function plot_io(variable, io, xAxis, data, linestyles, colors)
% Creates a plot of the time histories of a particular output or input variable
% for three different speeds with either time or distance on the x axis.
%
% Parameters
% ----------
% variable : string
%   The name of the variable you'd like to plot.
% io : string
%   'input' for input and 'output' for output.
% data : structure
%   Data for a set of bicycles, the first being the benchmark bicycle.
% xAxis : string
%   'Distance' or 'Time' on the x axis.
% linestyles : cell array
%   An array of linestyle types, one for each bicycle.
% colors : cell array
%   An array of colors, one for each bicycle.

global goldenRatio

if strcmp(io, 'input')
    names = {'Tphi',
             'Tdelta',
             'F'};
    prettyNames = {'$T_\phi$',
                   '$T_\delta$',
                   '$F$'};
    units = {'(N-m)',
             '(N-m)',
             '(N)'};
elseif strcmp(io, 'output')
    names = {'xP',
             'yP',
             'psi',
             'phi',
             'thetaP',
             'thetaR',
             'delta',
             'thetaF',
             'xPDot',
             'ypDot',
             'psiDot',
             'phiDot',
             'thetaPDot',
             'thetaRDot',
             'deltaDot',
             'thetaFDot',
             'xQ',
             'yQ'};
    prettyNames = {'$x_P$',
                   '$y_P$',
                   '$\psi$',
                   '$\phi$',
                   '$\theta_P$',
                   '$\theta_R$',
                   '$\delta$',
                   '$\theta_F$',
                   '$\dot{x}_P$',
                   '$\dot{y}_P$',
                   '$\dot{\psi}$',
                   '$\dot{\phi}$',
                   '$\dot{\theta}_P$',
                   '$\dot{\theta}_R$',
                   '$\dot{\delta}$',
                   '$\dot{\theta}_F$',
                   '$x_Q$',
                   '$y_Q$'};
    units = {'(m)',
             '(m)',
             '(rad)',
             '(rad)',
             '(rad)',
             '(rad)',
             '(rad)',
             '(rad)',
             '(m/s)',
             '(m/s)',
             '(rad/s)',
             '(rad/s)',
             '(rad/s)',
             '(rad/s)',
             '(rad/s)',
             '(rad/s)',
             '(m)',
             '(m)'};
else
    error('Please choose i or o')
end

index = find(ismember(names, variable) == 1);

bikes = fieldnames(data);
speedNames = fieldnames(data.Browser);

figure()
figWidth = 5.0;
figHeight = figWidth / goldenRatio;
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

% find the maximum value of the variable
maxValue = 0;
for i = 2:length(bikes)
    for j = 1:length(speedNames)
        oneSpeed = data.(bikes{i}).(speedNames{j});
        history = oneSpeed.([io 's'])(:, index);
        if max(history) > maxValue
            maxValue = max(history);
        end
    end
end

m = round(maxValue * 100) / 100;
pad = 0.15 * m;
yShift = [0, 2 * (m + pad), 4 * (m + pad)];

% shifts the paths by this many meters along the x axis
xShift = [0, 15, 35];
hold all
for j = 1:length(speedNames)
    for i = 2:length(bikes)
        oneSpeed = data.(bikes{i}).(speedNames{j});
        time = oneSpeed.time;
        speed = oneSpeed.speed;
        distance = time * speed + xShift(j);
        % time history of the value
        history = oneSpeed.([io 's'])(:, index) + yShift(j);
        if strcmp(xAxis, 'Distance')
            xData = distance;
            textX = 165;
        elseif strcmp(xAxis, 'Time')
            xData = time;
            textX = 2;
        else
            error('Choose Time or Distance, no other')
        end
        plot(xData, history, ...
             'Linestyle', linestyles{i - 1}, ...
             'Color', colors{i - 1}, ...
             'Linewidth', 0.75)
    end
    % add labels for the speeds
    text(textX, yShift(j) + 4 * pad, [num2str(speed) ' m/s'])
end

ylim([-m - pad, yShift(3) + m + pad])
set(gca, 'YTick', ...
    [-m, yShift(1), m, ...
     yShift(2) - m, yShift(2), yShift(2) + m, ...
     yShift(3) - m, yShift(3), yShift(3) + m])
ticks = {num2str(-m), '0', num2str(m)};
set(gca, 'YTickLabel', [ticks, ticks ticks])

if strcmp(xAxis, 'Distance')
    xlabel('Distance (m)')
    xLimits = [35, 190];
    xlim(xLimits)
    loc = 'Northwest';
else
    xlabel('Time (s)')
    xLimits = [0, 50];
    xlim(xLimits)
    loc = 'Northeast';
end
l1 = line(xLimits, [yShift(1) + m + pad, yShift(1) + m + pad]);
l2 = line(xLimits, [yShift(2) + m + pad, yShift(2) + m + pad]);
set([l1, l2], 'Color', 'k')
hold off
set(gca, 'Fontsize', 8)
first = [prettyNames{index} ' ' units{index}];
ylabel(first, 'Interpreter', 'Latex')
box on
legend({'1', '2', '3', '4', '5', '6'}, 'Fontsize', 8, 'Location', loc)

% if it is the steer angle plot for distance, add a magnifier for the
% countersteer
if strcmp(variable, 'delta') && strcmp(xAxis, 'Distance')
    % Specify the position and the size of the rectangle
    x_r = 37; y_r = 0; w_r = 4; h_r = 0.01;
    rectangle('Position', [x_r-w_r/2, y_r-h_r/2, w_r, h_r], ...
              'EdgeColor', 'k');
    % Specify the position and the size of the 2. axis
    x_a = 0.2; y_a = 0.29; w_a = 0.15; h_a = w_a * h_r / w_r * 20 / 0.05;
    ax = axes('Units', 'Normalized', ...
              'Position', [x_a, y_a, w_a, h_a], ...
              'XTick', [], ...
              'YTick', [], ...
              'Box', 'on', ...
              'LineWidth', 0.5, ...
              'Color', 'w');
    hold on
    j = 1;
    for i = 2:length(bikes)
        oneSpeed = data.(bikes{i}).(speedNames{j});
        time = oneSpeed.time;
        speed = oneSpeed.speed;
        distance = time * speed + xShift(j);
        % time history of the value
        history = oneSpeed.([io 's'])(:, index) + yShift(j);
        plot(distance, history, ...
             'Linestyle', linestyles{i - 1}, ...
             'Color', colors{i - 1}, ...
             'Linewidth', 0.75)
    end
    hold off
    axis([x_r-w_r/2, x_r+w_r/2, y_r-h_r/2, y_r+h_r/2]);
    text(35.3, -0.003, 'Countersteer', 'Fontsize', 8)
    % bottom left
    annotation('line', [x_a, 0.129], [y_a, 0.235])
    % top left
    annotation('line', [x_a, 0.132], [y_a + h_a, 0.26])
    % bottom right
    annotation('line', [x_a + w_a, 0.15], [y_a, 0.235])
    % top right
    annotation('line', [x_a, 0.15], [y_a + 0.02, 0.26])
end

% save the file
filename = [variable xAxis '.eps'];
print(['plots' filesep filename], '-deps2c', '-loose')
fix_ps_linestyle(['plots' filesep filename])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function plot_io_roll(rollData, xAxis)

global goldenRatio

% closed loop bode plots
figure()
figWidth = 5.0;
figHeight = figWidth / goldenRatio;
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

speed = rollData.speed;
time = rollData.time;
path = rollData.path;
frontWheel = rollData.outputs(:, 18);
rollAngle = rollData.outputs(:, 4);
steerAngle = rollData.outputs(:, 7);
rollTorque = rollData.inputs(:, 1);

% plot the path
subplot(2, 1, 1)
hold all
if strcmp(xAxis, 'Distance')
    plot(speed * time, -path, 'k-', 'Linewidth', 1.0)
    plot(speed * time, -frontWheel, 'k:', 'Linewidth', 1.0)
    xlabel('Distance (m)')
    xlim([30, 150])
elseif strcmp(xAxis, 'Time')
    plot(time, -path, 'k-', 'Linewidth', 1.0)
    plot(time, -frontWheel, 'k:', 'Linewidth', 1.0)
    xlabel('Time (s)')
    xlim([30 / speed, 150 / speed])
else
    error('Bad xAxis, choose Distance or Time')
end
hold off
box on
ylabel('Lateral Deviation (m)')
ylim([-2.2, 0.2])
set(gca, 'YTickLabel', {'2', '1', '0'})
legend({'Path'}, ...
       'Interpreter', 'Latex', ...
       'Fontsize', 8, ...
       'Location', 'Southeast')

subplot(2, 1, 2)
hold all
if strcmp(xAxis, 'Distance')
    plot(speed * time, rollAngle, 'k-', 'Linewidth', 1.0)
    [ax, h1, h2] = plotyy(speed * time, steerAngle, speed * time, rollTorque);
    xlabel('Distance (m)')
    xlim(ax(1), [30, 150])
    xlim(ax(2), [30, 150])
elseif strcmp(xAxis, 'Time')
    plot(time, rollAngle, 'k-', 'Linewidth', 1.0)
    [ax, h1, h2] = plotyy(time, steerAngle, time, rollTorque);
    xlabel('Time (s)')
    xlim(ax(1), [30 / speed, 150 / speed])
    xlim(ax(2), [30 / speed, 150 / speed])
else
    error('Bad xAxis, choose Distance or Time')
end
hold off
box on

set(get(ax(1), 'Ylabel'), ...
    'String', 'Angle (rad)', ...
    'Color', 'k')
set(get(ax(2), 'Ylabel'), ...
    'String', 'Torque (N-m)', ...
    'Color', 'k')
set(ax, 'YColor', 'k', 'Fontsize', 8)
set(h1, 'Linestyle', '--', 'Color', 'k', 'Linewidth', 1.0)
set(h2, 'Linestyle', '-.', 'Color', 'k', 'Linewidth', 1.0)
legend({'$\phi$', '$\delta$', '$T_\phi$'}, ...
       'Interpreter', 'Latex', ...
       'Fontsize', 8, ...
       'Location', 'Northeast')

if strcmp(xAxis, 'Distance')
    axes(ax(2))
    % magnifier rectangle
    x_r = 38; y_r = 0; w_r = 10; h_r = 0.3;
    rectangle('Position', [x_r-w_r/2, y_r-h_r/2, w_r, h_r], ...
              'EdgeColor', 'k');
    % magnify it
    x_a = 0.14; y_a = 0.432; w_a = 0.28; h_a = 0.12;
    inset = axes('Units', 'Normalized', ...
                 'Position', [x_a, y_a, w_a, h_a], ...
                 'Box', 'on', ...
                 'LineWidth', 0.5, ...
                 'Color', [0.8, 0.8, 0.8]);
    hold on
    plot(inset, time, rollAngle, 'k-', 'Linewidth', 1.0)
    [ax, h1, h2] = plotyy(inset, time, steerAngle, time, rollTorque);
    set(ax, 'XTick', [], ...
            'YTick', [], ...
            'YColor', 'k')
    set(h1, 'Linestyle', '--', 'Color', 'k', 'Linewidth', 1.0)
    set(h2, 'Linestyle', '-.', 'Color', 'k', 'Linewidth', 1.0)
    axis(ax(1), [7, 8.5, -0.0025, 0.0025])
    axis(ax(2), [7, 8.5, -0.0025 / 0.02, 0.0025 / 0.02])

    % draw some lines connecting the corners
    annotation('line', [x_a, 0.149], [y_a, 0.287])
    annotation('line', [x_a + w_a, 0.213], [y_a, 0.287])
    annotation('textbox', [0.26, 0.47, 0.1, 0.02], ...
               'String', 'Countersteer', ...
               'Fontsize', 8, ...
               'Edgecolor', 'none')
end

filename = ['roll' xAxis '.eps'];
print(gcf, ['plots' filesep filename], '-deps2c', '-loose')
fix_ps_linestyle(['plots' filesep filename])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function phase_portraits(bikeData)
% Creates four phase portrait plots to demonstrate the effects on the phase
% portraits when adjusting the gains.

global goldenRatio

figure()
figWidth = 6.0;
figHeight = figWidth / goldenRatio;
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'OuterPosition', [424, 305 - 50, 518, 465], ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

% this is the gain multiplier for the non-nominal plot
gainChanges = [1.2, 1, 1, 1, 1;
               1, 1.2, 1, 1, 1;
               1, 1, 1.2, 1, 1;
               1, 1, 1.2, 0.8, 0.8];

loopNames = {'kDelta', 'kPhiDot', 'kPhi', 'kPhi'};
% the x and y indices for each plot
xy = [7, 15;
      4, 12;
      4, 12;
      4, 12];
% where to get the x and y data from
xySource = {'outputs', 'outputsDot', 'outputs', 'outputs'};

% axis labels
xlabels = {'(a) $\delta$ (rad)',
           '(b) $\dot{\phi}$ (rad/s)',
           '(c) $\phi$ (rad)',
           '(d) $\phi$ (rad)'};
ylabels = {'$\dot{\delta}$ (rad/s)',
           '$\ddot{\phi}$ (rad/s$^2$)',
           '$\dot{\phi}$ (rad/s)',
           '$\dot{\phi}$ (rad/s)'};

% legend starts
legends = {'$k_\delta$ = ',
           '$k_{\dot{\phi}}$ = ',
           '$k_\phi$ = ',
           '$k_\phi$ = '};
% how many decimals to show for each legend
floatSpec = {'%1.1f', '%1.3f', '%1.1f', '%1.1f'};

for i = 1:length(loopNames)

    display(sprintf('Calculating phase portrait %d', i))

    % adjust the gains and get the data
    twentyPercent = generate_data('Benchmark', 5.0, ...
                                  'gainMuls', gainChanges(i, :));
    subplot(2, 2, i)
    hold on

    if i == 4
        display('Phase portrait 4 comparison data.')
        nominalData = generate_data('Benchmark', 5.0, ...
                                  'gainMuls', [1, 1, 1, 0.8, 0.8]);
        x = nominalData.(xySource{i})(:, xy(i, 1));
        y = nominalData.(xySource{i})(:, xy(i, 2));
    else
        x = bikeData.(xySource{i})(:, xy(i, 1));
        y = bikeData.(xySource{i})(:, xy(i, 2));
    end
    plot(x, y, 'k-', 'Linewidth', 1.0)

    x = twentyPercent.(xySource{i})(:, xy(i, 1));
    y = twentyPercent.(xySource{i})(:, xy(i, 2));
    plot(x, y, 'k--', 'Linewidth', 1.0)

    hold off

    box on
    axis equal
    xlabel(xlabels{i}, 'Interpreter', 'Latex', 'Fontsize', 8)
    ylabel(ylabels{i}, 'Interpreter', 'Latex', 'Fontsize', 8)

    % make the legend
    leg1 = sprintf(floatSpec{i}, bikeData.modelPar.(loopNames{i}));
    leg2 = sprintf(floatSpec{i}, twentyPercent.modelPar.(loopNames{i}));
    legend({[legends{i} leg1], [legends{i} leg2]} , ...
           'Interpreter', 'Latex', ...
           'Fontsize', 6)
end

plotAxes = findobj(gcf, 'Type', 'Axes');
set(plotAxes, 'Fontsize', 8)

% save the plot
filename = 'phasePortraits.eps';
print(gcf, ['plots' filesep filename], '-deps2c', '-loose')
fix_ps_linestyle(['plots' filesep filename])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function eigenvalues(data, linestyles, colors)

global goldenRatio

figure()
figWidth = 5.0;
figHeight = figWidth / goldenRatio;
set(gcf, ...
    'Color', [1, 1, 1], ...
    'PaperOrientation', 'portrait', ...
    'PaperUnits', 'inches', ...
    'PaperPositionMode', 'manual', ...
    'PaperPosition', [0, 0, figWidth, figHeight], ...
    'PaperSize', [figWidth, figHeight])

bikes = fieldnames(data);
bikes(1) = [];
speeds = 0:0.1:10;
eVals = zeros(length(bikes), length(speeds), 11);
for i = 1:length(bikes)
    % load the bicycle parameters
    pathToParFile = ['parameters' filesep bikes{i} 'Par.txt'];
    par = par_text_to_struct(pathToParFile);
    str = 'Calculating eigenvalues for the %s bicycle and rider.';
    display(sprintf(str, bikes{i}))
    for j = 1:length(speeds)
        % calculate the A, B, C, and D matrices of the bicycle model
        [A, B, C, D] = whipple_pull_force_abcd(par, speeds(j));
        eigenValues = eig(A);
        eVals(i, j, :) = real(eig(A));
    end
end

% reduce to the maximum values
zeroIndices = find(abs(eVals) <= 0.000001);
eVals(zeroIndices) = -100 * ones(size(zeroIndices));
maxEvals = max(eVals, [], 3);

lines = plot(speeds, maxEvals);

for i = 1:length(lines)
    set(lines(i), ...
        'Linestyle', linestyles{i}, ...
        'Color', colors{i}, ...
        'Linewidth', 1)
end

legend({'1', '2', '3', '4', '5', '6'})
xlabel('Speed (m/s)')
ylabel('Maximum real part of the eigenvalue (1/s)')

% set the tick marks differently
%set(gca, 'XTick', 0:0.5:10)
%set(gca, 'XTickLabel', {'0', '', '1', '', ...
                        %'2', '2.5', '3', '', ...
                        %'4', '', '5.0', '', ...
                        %'6', '', '7', '7.5', ...
                        %'8', '', '9', '', '10'})
%
% add some lines and labels for the speeds we looked at
hold on
maxLine = max(maxEvals, [], 1);
minLine = min(maxEvals, [], 1);
lines = zeros(4, 1);
speedInd = find(speeds == 2.5);
lines(1) = line([2.5, 2.5], ...
                [minLine(speedInd) - 0.4, maxLine(speedInd) + 0.4]);
text(2, maxLine(speedInd) + 0.7, '2.5 m/s')
speedInd = find(speeds == 5.0);
lines(2) = line([5.0, 5.0], ...
                [minLine(speedInd) - 0.4, maxLine(speedInd) + 0.4]);
text(4.5, maxLine(speedInd) + 0.7, '5.0 m/s')
speedInd = find(speeds == 7.5);
lines(3) = line([7.5, 7.5], ...
                [minLine(speedInd) - 0.4, maxLine(speedInd) + 0.4]);
text(7, maxLine(speedInd) + 0.7, '7.5 m/s')

lines(4) = line([0, 10], [0, 0]);
hold off

set(lines, 'Color', 'k', 'Linewidth', 2)

% save the plot
filename = 'eigenvalues.eps';
print(gcf, ['plots' filesep filename], '-deps2c', '-loose')
fix_ps_linestyle(['plots' filesep filename])
