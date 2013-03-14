function [filtfcn, statefcn] = makeFilter(b, a)
%   FILTFCN = MAKEFILTER(B, A) creates an IIR filtering
%   function and returns it in the form of a function handle,
%   FILTFCN. Each time you call FILTFCN with a new filter 
%   input value, it computes the corresponding new filter 
%   output value, updating its internal state vector at the
%   same time.
%
%   [FILTFCN, STATEFCN] = MAKEFILTER(B, A) also returns a 
%   function (in the form of a function handle, STATEFCN) 
%   that can return the filter's internal state.  The internal
%   state vector is in the form of a transposed direct form 
%   II delay line.

%   Initialize state vector. To keep this example a bit 
%   simpler, assume that a and b have the same length.  
%   Also assume that a(1) is 1.

v = zeros(size(a));

filtfcn =  @iirFilter;
statefcn = @getState;

   function yn = iirFilter(xn)
      % Update the state vector
      v(1) = v(2) + b(1) * xn;
      v(2:end-1) = v(3:end) + b(2:end-1) * xn - ...
         a(2:end-1) * v(1);
      v(end) = b(end) * xn - a(end) * v(1);
      
      % Output is the first element of the state vector.
      yn = v(1);
   end

   function vOut = getState
      vOut = v;
   end
end
