function out = normest(f)
%NORMEST   Estimate the Inf-norm of an OBJECT
%   NORMEST(F) is an estimate of the Inf-norm of the OBJECT F.

% Call normest of the OBJECT of F
out = normest(f.obj);

end