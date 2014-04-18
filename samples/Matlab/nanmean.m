function m = nanmean(x,dim,minDataFrac)
%IRF.NANMEAN Mean value, ignoring NaNs.
%   M = IRF.NANMEAN(X) returns the sample mean of X, treating NaNs as missing
%   values.  For vector input, M is the mean value of the non-NaN elements
%   in X.  For matrix input, M is a row vector containing the mean value of
%   non-NaN elements in each column.  For N-D arrays, IRF.NANMEAN operates
%   along the first non-singleton dimension.
%
%   IRF.NANMEAN(X,DIM) takes the mean along dimension DIM of X.
%
%   IRF.NANMEAN(X,DIM,MINDATAFRAC) specifies how large at least should be the fraction
%   of data with respect to NaN. If fraction is less, then the resulting value is NaN.
%   MINDATAFRAC=0, any number of NaNs is allowed. MINDATAFRAC=1, no NaNs are allowed.
%
%   See also MEAN
%
%   NANMEAN (without MINDATAFRAC argument) is also part of the Signal Processing Toolbox since Matlab R2013a.


if nargin == 2, minDataFrac = 0; end

% Find NaNs and set them to zero
nans = isnan(x);
x(nans) = 0;

if nargin == 1 % let sum deal with figuring out which dimension to use
    % Count up non-NaNs.
    n = sum(~nans);
    n(n==0) = NaN; % prevent divideByZero warnings
    % Sum up non-NaNs, and divide by the number of non-NaNs.
    m = sum(x) ./ n;
else
    % Count up non-NaNs.
    n = sum(~nans,dim);
    n(n==0) = NaN; % prevent divideByZero warnings
    % Sum up non-NaNs, and divide by the number of non-NaNs.
    m = sum(x,dim) ./ n;
    m(n<size(x,dim)*minDataFrac) = NaN;
end