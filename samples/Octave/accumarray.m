########################################################################
##
## Copyright (C) 2007-2024 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{A} =} accumarray (@var{subs}, @var{vals})
## @deftypefnx {} {@var{A} =} accumarray (@var{subs}, @var{vals}, @var{sz})
## @deftypefnx {} {@var{A} =} accumarray (@var{subs}, @var{vals}, @var{sz}, @var{fcn})
## @deftypefnx {} {@var{A} =} accumarray (@var{subs}, @var{vals}, @var{sz}, @var{fcn}, @var{fillval})
## @deftypefnx {} {@var{A} =} accumarray (@var{subs}, @var{vals}, @var{sz}, @var{fcn}, @var{fillval}, @var{issparse})
##
## Create an array by accumulating the elements of a vector into the
## positions defined by their subscripts.
##
## The subscripts are defined by the rows of the matrix @var{subs} and the
## values by @var{vals}.  Each row of @var{subs} corresponds to one of the
## values in @var{vals}.  If @var{vals} is a scalar, it will be used for each
## of the row of @var{subs}.  If @var{subs} is a cell array of vectors, all
## vectors must be of the same length, and the subscripts in the @var{k}th
## vector must correspond to the @var{k}th dimension of the result.
##
## The size of the matrix will be determined by the subscripts
## themselves.  However, if @var{sz} is defined it determines the matrix
## size.  The length of @var{sz} must correspond to the number of columns
## in @var{subs}.  An exception is if @var{subs} has only one column, in
## which case @var{sz} may be the dimensions of a vector and the
## subscripts of @var{subs} are taken as the indices into it.
##
## The default action of @code{accumarray} is to sum the elements with
## the same subscripts.  This behavior can be modified by defining the
## @var{fcn} function.  This should be a function or function handle
## that accepts a column vector and returns a scalar.  The result of the
## function should not depend on the order of the subscripts.
##
## The elements of the returned array that have no subscripts associated
## with them are set to zero.  Defining @var{fillval} to some other value
## allows these values to be defined.  This behavior changes, however,
## for certain values of @var{fcn}.  If @var{fcn} is @code{@@min}
## (respectively, @code{@@max}) then the result will be filled with the
## minimum (respectively, maximum) integer if @var{vals} is of integral
## type, logical false (respectively, logical true) if @var{vals} is of
## logical type, zero if @var{fillval} is zero and all values are
## non-positive (respectively, non-negative), and NaN otherwise.
##
## By default @code{accumarray} returns a full matrix.  If
## @var{issparse} is logically true, then a sparse matrix is returned
## instead.
##
## The following @code{accumarray} example constructs a frequency table
## that in the first column counts how many occurrences each number in
## the second column has, taken from the vector @var{x}.  Note the usage
## of @code{unique}  for assigning to all repeated elements of @var{x}
## the same index (@pxref{XREFunique,,@code{unique}}).
##
## @example
## @group
## @var{x} = [91, 92, 90, 92, 90, 89, 91, 89, 90, 100, 100, 100];
## [@var{u}, ~, @var{j}] = unique (@var{x});
## [accumarray(@var{j}', 1), @var{u}']
##   @result{}  2    89
##       3    90
##       2    91
##       2    92
##       3   100
## @end group
## @end example
##
## Another example, where the result is a multi-dimensional 3-D array and
## the default value (zero) appears in the output:
##
## @example
## @group
## accumarray ([1, 1, 1;
##              2, 1, 2;
##              2, 3, 2;
##              2, 1, 2;
##              2, 3, 2], 101:105)
## @result{} ans(:,:,1) = [101, 0, 0; 0, 0, 0]
## @result{} ans(:,:,2) = [0, 0, 0; 206, 0, 208]
## @end group
## @end example
##
## The sparse option can be used as an alternative to the @code{sparse}
## constructor (@pxref{XREFsparse,,@code{sparse}}).  Thus
##
## @example
## sparse (@var{i}, @var{j}, @var{sv})
## @end example
##
## @noindent
## can be written with @code{accumarray} as
##
## @example
## accumarray ([@var{i}, @var{j}], @var{sv}', [], [], 0, true)
## @end example
##
## @noindent
## For repeated indices, @code{sparse} adds the corresponding value.  To
## take the minimum instead, use @code{min} as an accumulator function:
##
## @example
## accumarray ([@var{i}, @var{j}], @var{sv}', [], @@min, 0, true)
## @end example
##
## The complexity of accumarray in general for the non-sparse case is
## generally O(M+N), where N is the number of subscripts and M is the
## maximum subscript (linearized in multi-dimensional case).  If
## @var{fcn} is one of @code{@@sum} (default), @code{@@max},
## @code{@@min} or @code{@@(x) @{x@}}, an optimized code path is used.
## Note that for general reduction function the interpreter overhead can
## play a major part and it may be more efficient to do multiple
## accumarray calls and compute the results in a vectorized manner.
##
## @seealso{accumdim, unique, sparse}
## @end deftypefn

function A = accumarray (subs, vals, sz = [], fcn = [], fillval = [], issparse = [])

  if (nargin < 2)
    print_usage ();
  endif

  lenvals = length (vals);

  if (iscell (subs))
    subs = cellfun (@vec, subs, "uniformoutput", false);
    ndims = numel (subs);
    if (ndims == 1)
      subs = subs{1};
    endif

    lensubs = cellfun (@length, subs);

    if (any (lensubs != lensubs(1)) || (lenvals > 1 && lenvals != lensubs(1)))
      error ("accumarray: dimension mismatch");
    endif

  else
    ndims = columns (subs);
    if (lenvals > 1 && lenvals != rows (subs))
      error ("accumarray: dimension mismatch");
    endif
  endif

  if (isempty (fcn))
    fcn = @sum;
  elseif (! is_function_handle (fcn))
    error ("accumarray: FCN must be a function handle");
  endif

  if (isempty (fillval))
    fillval = 0;
  endif

  if (isempty (issparse))
    issparse = false;
  endif

  if (issparse)

    ## Sparse case.
    ## Avoid linearizing the subscripts, because it could overflow.

    if (fillval != 0)
      error ("accumarray: FILLVAL must be zero in the sparse case");
    endif

    ## Ensure subscripts are a two-column matrix.
    if (iscell (subs))
      subs = [subs{:}];
    endif

    ## Validate dimensions.
    if (ndims == 1)
      subs(:,2) = 1;
    elseif (ndims != 2)
      error ("accumarray: in the sparse case, needs 1 or 2 subscripts");
    endif

    if (isnumeric (vals) || islogical (vals))
      vals = double (vals);
    else
      error ("accumarray: in the sparse case, values must be numeric or logical");
    endif

    if (fcn != @sum)

      ## Reduce values.  This is not needed if we're about to sum them,
      ## because "sparse" can do that.

      ## Sort indices.
      [subs, idx] = sortrows (subs);
      n = rows (subs);
      ## Identify runs.
      jdx = find (any (diff (subs, 1, 1), 2));
      jdx = [jdx; n];

      vals = cellfun (fcn, mat2cell (vals(:)(idx), diff ([0; jdx])));
      subs = subs(jdx, :);
      mode = "unique";
    else
      mode = "sum";
    endif

    ## Form the sparse matrix.
    if (isempty (sz))
      A = sparse (subs(:,1), subs(:,2), vals, mode);
    elseif (length (sz) == 2)

      ## Row vector case
      if (sz(1) == 1)
        [i, j] = deal (subs(:,2), subs(:,1));
      else
        [i, j] = deal (subs(:,1), subs(:,2));
      endif
      A = sparse (i, j, vals, sz(1), sz(2), mode);
    else
      error ("accumarray: dimensions mismatch");
    endif

  else

    ## Linearize subscripts.
    if (ndims > 1)
      if (isempty (sz))
        if (iscell (subs))
          sz = cellfun ("max", subs);
        else
          sz = max (subs, [], 1);
        endif
      elseif (ndims != length (sz))
        error ("accumarray: dimensions mismatch");
      endif

      ## Convert multidimensional subscripts.
      if (isnumeric (subs))
        subs = num2cell (subs, 1);
      endif
      subs = sub2ind (sz, subs{:}); # creates index cache
    elseif (! isempty (sz) && length (sz) < 2)
      error ("accumarray: needs at least 2 dimensions");
    elseif (! isindex (subs)) # creates index cache
      error ("accumarray: indices must be positive integers");
    endif


    ## Some built-in reductions handled efficiently.

    if (fcn == @sum)
      ## Fast summation.
      if (isempty (sz))
        A = __accumarray_sum__ (subs, vals);
      else
        A = __accumarray_sum__ (subs, vals, prod (sz));
        ## set proper shape.
        A = reshape (A, sz);
      endif

      ## we fill in nonzero fill value.
      if (fillval != 0)
        mask = true (size (A));
        mask(subs) = false;
        A(mask) = fillval;
      endif
    elseif (fcn == @max)
      ## Fast maximization.

      if (isinteger (vals))
        zero = intmin (vals);
      elseif (islogical (vals))
        zero = false;
      elseif (fillval == 0 && all (vals(:) >= 0))
        ## This is a common case - fillval is zero, all numbers
        ## nonegative.
        zero = 0;
      else
        zero = NaN; # Neutral value.
      endif

      if (isempty (sz))
        A = __accumarray_max__ (subs, vals, zero);
      else
        A = __accumarray_max__ (subs, vals, zero, prod (sz));
        A = reshape (A, sz);
      endif

      if (fillval != zero && ! (isnan (fillval) || isnan (zero)))
        mask = true (size (A));
        mask(subs) = false;
        A(mask) = fillval;
      endif
    elseif (fcn == @min)
      ## Fast minimization.

      if (isinteger (vals))
        zero = intmax (vals);
      elseif (islogical (vals))
        zero = true;
      elseif (fillval == 0 && all (vals(:) <= 0))
        ## This is a common case - fillval is zero, all numbers
        ## non-positive.
        zero = 0;
      else
        zero = NaN; # Neutral value.
      endif

      if (isempty (sz))
        A = __accumarray_min__ (subs, vals, zero);
      else
        A = __accumarray_min__ (subs, vals, zero, prod (sz));
        A = reshape (A, sz);
      endif

      if (fillval != zero && ! (isnan (fillval) || isnan (zero)))
        mask = true (size (A));
        mask(subs) = false;
        A(mask) = fillval;
      endif
    else

      ## The general case.  Reduce values.
      n = rows (subs);
      if (numel (vals) == 1)
        vals = vals(ones (1, n), 1);
      else
        vals = vals(:);
      endif

      ## Sort indices.
      [subs, idx] = sort (subs);
      ## Identify runs.
      jdx = find (subs(1:n-1) != subs(2:n));
      if (n != 0) # bug #47287
        jdx = [jdx; n];
      endif
      vals = mat2cell (vals(idx), diff ([0; jdx]));
      ## Optimize the case when function is @(x) {x}, i.e., we just want
      ## to collect the values to cells.
      persistent simple_cell_str = func2str (@(x) {x});
      if (! strcmp (func2str (fcn), simple_cell_str))
        vals = cellfun (fcn, vals);
      endif

      subs = subs(jdx);

      if (isempty (sz))
        sz = max (subs);
        ## If subs is empty, sz will be too, and length will be 0, hence "<= 1"
        if (length (sz) <= 1)
          sz(2) = 1;
        endif
      endif

      ## Construct matrix of fillvals.
      if (iscell (vals))
        A = cell (sz);
      elseif (fillval == 0)
        A = zeros (sz, class (vals));
      else
        A = repmat (fillval, sz);
      endif

      ## Set the reduced values.
      A(subs) = vals;
    endif
  endif

endfunction


%!assert (accumarray ([1; 2; 4; 2; 4], 101:105), [101; 206; 0; 208])
%!assert (accumarray ([1 1 1; 2 1 2; 2 3 2; 2 1 2; 2 3 2], 101:105),
%!                    cat (3, [101 0 0; 0 0 0], [0 0 0; 206 0 208]))

%!assert (accumarray ([1 1 1; 2 1 2; 2 3 2; 2 1 2; 2 3 2], 101:105, [], @(x) sin (sum (x))),
%!        sin (cat (3, [101,0,0;0,0,0],[0,0,0;206,0,208])))

%!assert (accumarray ({[1 3 3 2 3 1 2 2 3 3 1 2], [3 4 2 1 4 3 4 2 2 4 3 4], [1 1 2 2 1 1 2 1 1 1 2 2]}, 101:112),
%!        cat (3, [0 0 207 0; 0 108 0 0; 0 109 0 317], [0 0 111 0; 104 0 0 219; 0 103 0 0]))

%!assert (accumarray ([1 1; 2 1; 2 3; 2 1; 2 3], 101:105, [2 4], @max, NaN),
%!        [101 NaN NaN NaN; 104 NaN 105 NaN])

%!assert (accumarray ([1 1; 2 1; 2 3; 2 1; 2 3], 101:105, [], @prod),
%!        [101 0 0; 10608 0 10815])
%!assert (accumarray ([1 1; 2 1; 2 3; 2 1; 2 3], 101:105, [2 4], @prod, 0, true),
%!        sparse ([1 2 2], [1 1 3], [101 10608 10815], 2, 4))
%!assert (accumarray ([1 1; 2 1; 2 3; 2 1; 2 3], 1, [2 4]), [1 0 0 0; 2 0 2 0])
%!assert (accumarray ([1 1; 2 1; 2 3; 2 1; 2 3], 101:105, [2 4], @(x) length (x) > 1),
%!        [false false false false; true false true false])

%!assert (accumarray ([1; 2], [3; 4], [2, 1], @min, [], 0), [3; 4])
%!assert (accumarray ([1; 2], [3; 4], [2, 1], @min, [], 1), sparse ([3; 4]))
%!assert (accumarray ([1; 2], [3; 4], [1, 2], @min, [], 0), [3, 4])
%!assert (accumarray ([1; 2], [3; 4], [1, 2], @min, [], 1), sparse ([3, 4]))

%!test
%! A = accumarray ([1 1; 2 1; 2 3; 2 1; 2 3], 101:105, [2,4], @(x) {x});
%! assert (A{2},[102; 104]);

%!test
%! subs = ceil (rand (2000, 3)*10);
%! vals = rand (2000, 1);
%! assert (accumarray (subs, vals, [], @max),
%!         accumarray (subs, vals, [], @(x) max (x)));

%!test
%! subs = ceil (rand (2000, 1)*100);
%! vals = rand (2000, 1);
%! assert (accumarray (subs, vals, [100, 1], @min, NaN),
%!         accumarray (subs, vals, [100, 1], @(x) min (x), NaN));

%!test
%! subs = ceil (rand (2000, 2)*30);
%! subsc = num2cell (subs, 1);
%! vals = rand (2000, 1);
%! assert (accumarray (subsc, vals, [], [], 0, true),
%!         accumarray (subs, vals, [], [], 0, true));

%!test
%! subs = ceil (rand (2000, 3)*10);
%! subsc = num2cell (subs, 1);
%! vals = rand (2000, 1);
%! assert (accumarray (subsc, vals, [], @max),
%!         accumarray (subs, vals, [], @max));

%!error accumarray (1:5)
%!error accumarray ([1,2,3],1:2)

## Handle empty arrays
%!test <*47287>
%! ## min, max, and sum are special cases within accumarray so test them.
%! funcs = {@(x) length (x) > 1, @min, @max, @sum};
%! for idx = 1:numel (funcs)
%!   assert (accumarray (zeros (0, 1), [], [0 1] , funcs{idx}), zeros (0, 1));
%!   assert (accumarray (zeros (0, 1), [], [1 0] , funcs{idx}), zeros (1, 0));
%!   assert (accumarray (zeros (0, 1), [], [] , funcs{idx}), zeros (0, 1));
%! endfor

## Matlab returns an array of doubles even though FCN returns cells.  In
## Octave, we do not have that bug, at least for this case.
%!assert (accumarray (zeros (0, 1), [], [0 1] , @(x) {x}), cell (0, 1))

%!error <FCN must be a function handle>
%! accumarray ([1; 2; 3], [1; 2; 3], [3 1], '@(x) {x}')
