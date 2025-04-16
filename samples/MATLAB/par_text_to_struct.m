function par = par_text_to_struct(pathToFile)
% function par = par_text_to_struct(pathToFile)
% Returns a structure of the parameters that were stored in a csv text file.
%
% Parameters
% ----------
% pathToFile : string
%   Path to a text file containing the benchmark parameters for a single
%   bicycle. The parameters should be on seperate lines and take this form:
%
%   c = 0.08+/-0.01
%   lam = 0.31
%
% Returns
% -------
% par : structure
%   A structure containing the bicycle parameters.

fid = fopen(pathToFile);
data = textscan(fid, '%s %s', 'delimiter', '=');
fclose(fid);
names = strtrim(data{1});
vals = strtrim(regexp(data{2}, '+/-', 'split'));
for i = 1:length(names)
    v = vals{i};
    par.(names{i}) = str2num(v{1});
end
