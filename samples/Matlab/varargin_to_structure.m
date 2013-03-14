function options = varargin_to_structure(arguments)
% function options = varargin_to_structure(arguments)
% Returns a structure from a cell array of pairs.
%
% Parameters
% ----------
% arguments : cell array (1, n)
%   A cell array where n is an even number greater than or equal to 2. The odd
%   cells must be a character string and the even cells can be any data type.
%
% Returns
% -------
% options : structure
%   The fields of the structure correspond to the odd cells in the array and
%   the value for that field corresponds to the even cells.
%
% This is useful for functions that have varargin as an input where the
% variable inputs are keyword pairs.

% make sure there are enough arguments
if length(arguments) <= 1
    error('Please supply 2 or more arguments')
end

% make sure they provided and even number of inputs
if mod(length(arguments), 2) ~= 0
    error('There must be an even numbers of arguments')
end

% store the values in the structure
for i = 1:2:length(arguments)
    % make sure they have character strings as all the odd cells
    if ~ischar(arguments{i})
        error('The odd arguments must be character strings.')
    end
    options.(arguments{i}) = arguments{i + 1};
end
