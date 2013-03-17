function write_gains(pathToFile, speeds, gains)
% function write_gains(pathToFile, speeds, gains)
%
% Adds the provided gains to the file.
%
% Parameters
% ----------
% pathToFile : string
%   The path to a gain file.
% speeds : matrix(n, 1)
% gains : matrix (n, 5)
%   A matrix of gains where each row corresponds to a speed and the columns
%   correspond to the loops starting at the innermost loop.

contents = importdata(pathToFile);

speedsInFile = contents.data(:, 1);
gainsInFile = contents.data(:, 2:end);

% remove any speeds that are very close to the speeds provided
sameSpeedIndices = [];
for i = 1:length(speedsInFile)
    for j = 1:length(speeds)
        if abs(speedsInFile(i) - speeds(j)) < 1e-3
            sameSpeedIndices = [sameSpeedIndices i];
        end
    end
end
speedsInFile(sameSpeedIndices) = [];
gainsInFile(sameSpeedIndices, :) = [];

% concatenate data
allGains = [gainsInFile; gains];
allSpeeds = [speedsInFile; speeds];

% sort the data
[allSpeeds, order] = sort(allSpeeds);
allGains = allGains(order, :);

% recombine
all = [allSpeeds, allGains];

% rewrite the file
fid = fopen(pathToFile, 'w');
h = contents.colheaders;
fprintf(fid, '%s,%s,%s,%s,%s,%s\n', h{1}, h{2}, h{3}, h{4}, h{5}, h{6});
fprintf(fid, '%1.3f,%1.4f,%1.4f,%1.4f,%1.4f,%1.4f\n', all');
fclose(fid);
