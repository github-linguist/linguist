########################################################################
##
## Copyright (C) 1995-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{v} =} var (@var{x})
## @deftypefnx {} {@var{v} =} var (@var{x}, @var{w})
## @deftypefnx {} {@var{v} =} var (@var{x}, @var{w}, @var{dim})
## @deftypefnx {} {@var{v} =} var (@var{x}, @var{w}, @var{vecdim})
## @deftypefnx {} {@var{v} =} var (@var{x}, @var{w}, @qcode{"all"})
## @deftypefnx {} {@var{v} =} var (@dots{}, @var{nanflag})
## @deftypefnx {} {[@var{v}, @var{m}] =} var (@dots{})
## Compute the variance of the elements of the vector @var{x}.
##
## The variance is defined as
## @tex
## $$ {\rm var}(x) = {1\over N-1} \sum_{i=1}^N (x_i - \bar x )^2 $$
## where $\bar{x}$ is the mean value of @var{x} and $N$ is the number of
## elements of @var{x}.
## @end tex
## @ifnottex
##
## @example
## @group
## var (@var{x}) = (1 / (N-1)) * SUM_i ((@var{x}(i) - mean(@var{x}))^2)
## @end group
## @end example
##
## @noindent
## where @math{N} is the number of elements of @var{x}.
## @end ifnottex
##
## If @var{x} is an array, compute the variance along the first non-singleton
## dimensions of @var{x}.
##
## The optional argument @var{w} determines the weighting scheme to use.  Valid
## values are:
##
## @table @asis
## @item 0 [default]:
## Normalize with @math{N-1} (population variance).  This provides the square
## root of the best unbiased estimator of the variance.
##
## @item 1:
## Normalize with @math{N} (sample variance).  This provides the square root of
## the second moment around the mean.
##
## @item a vector:
## Compute the weighted variance with non-negative weights.  The length of
## @var{w} must equal the size of @var{x} in the operating dimension.  NaN
## values are permitted in @var{w}, will be multiplied with the associated
## values in @var{x}, and can be excluded by the @var{nanflag} option.
##
## @item an array:
## Similar to vector weights, but @var{w} must be the same size as @var{x}.  If
## the operating dimension is supplied as @var{vecdim} or @qcode{"all"} and
## @var{w} is not a scalar, @var{w} must be an same-sized array.
## @end table
##
## Note: @var{w} must always be specified before specifying any of the
## following dimension options.  To use the default value for @var{w} you
## may pass an empty input argument [].
##
## The optional variable @var{dim} forces @code{var} to operate over the
## specified dimension, which must be a positive integer-valued number.
## Specifying any singleton dimension in @var{x}, including any dimension
## exceeding @code{ndims (@var{x})}, will result in a variance of 0.
##
## Specifying the dimensions as  @var{vecdim}, a vector of non-repeating
## dimensions, will return the variance calculated over the array slice defined
## by @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it
## is equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will force @code{var} to
## operate on all elements of @var{x}, and is equivalent to @code{var
## (@var{x}(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps NaN values in the calculation.  To
## exclude NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.
## The output will still contain NaN values if @var{x} consists of all NaN
## values in the operating dimension.
##
## The optional second output variable @var{m} contains the mean of the
## elements of @var{x} used to calculate the variance.  If @var{v} is the
## weighted variance, then @var{m} is also the weighted mean.
##
## @seealso{std, mean, cov, skewness, kurtosis, moment}
## @end deftypefn

function [v, m] = var (x, varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  ## initialize variables
  all_flag = false;
  omitnan = false;
  nvarg = numel (varargin);
  varg_chars = cellfun ('ischar', varargin);

  ## Check all char arguments.
  if (nvarg == 3 && ! varg_chars(3))
    print_usage ();
  endif

  if (any (varg_chars))
    for argin = varargin(varg_chars)
      switch (lower (argin{1}))
        case "all"
          all_flag = true;
        case "omitnan"
          omitnan = true;
        case "includenan"
          omitnan = false;
        otherwise
          print_usage ();
      endswitch
    endfor
    varargin(varg_chars) = [];
    nvarg = numel (varargin);
  endif

  ## FIXME: When sparse can broadcast ops then remove sparse checks and hacks.
  x_issparse = issparse (x);
  w = 0;
  weighted = false; # true if weight vector/array used
  vecdim = [];
  vecempty = true;
  vecdim_scalar_vector = [false, false]; # [false, false] for empty vecdim
  szx = size (x);
  ndx = ndims (x);

  ## Check numeric arguments
  if (! (isnumeric (x)))
    error ("var: X must be a numeric vector or matrix");
  endif
  if (isa (x, "single"))
    outtype = "single";
  else
    outtype = "double";
  endif

  if (nvarg > 0)
    if (nvarg > 2 || any (! cellfun ('isnumeric', varargin)))
      print_usage ();
    endif
    ## Process weight input
    if (any (varargin{1} < 0))
      error ("var: weights must not contain any negative values");
    endif
    if (isscalar (varargin{1}))
      w = varargin{1};
      if (! (w == 0 || w == 1) && ! isscalar (x))
        error ("var: normalization scalar must be either 0 or 1");
      endif
    elseif (numel (varargin{1}) > 1)
      weights = varargin{1};
      weighted = true;
    endif
    if (nvarg > 1)
    ## Process dimension input
      vecdim = varargin{2};
      if (! (vecempty = isempty (vecdim)))
        ## Check for empty vecdim, won't change vsv if nonzero size empty
        vecdim_scalar_vector = [isscalar(vecdim), isvector(vecdim)];
      endif
      if (! (vecdim_scalar_vector(2) && all (vecdim > 0))
          || any (rem (vecdim, 1)))
        error ("var: DIM must be a positive integer scalar or vector");
      endif
      if (vecdim_scalar_vector(1) && vecdim > ndx && ! isempty (x))
        ## Scalar dimension larger than ndims(x), variance of any single number
        ## is zero, except for inf, NaN, and empty values of x.
        v = zeros (szx, outtype);
        vn = ! isfinite (x);
        v(vn) = NaN;
        m = x;
        return;
      endif
      if (vecdim_scalar_vector == [0 1] && (! all (diff (sort (vecdim)))))
        error ("var: VECDIM must contain non-repeating positive integers");
      endif
    endif
  endif

  ## Check for conflicting input arguments
  if (all_flag && ! vecempty)
    error ("var: 'all' flag cannot be used with DIM or VECDIM options");
  endif
  if (weighted)
    if (all_flag)
      if (isvector (weights))
        if (numel (weights) != numel (x))
          error ("var: weight vector element count does not match X");
        endif
      elseif (! (isequal (size (weights), szx)))
        error ("var: weight matrix or array does not match X in size");
      endif

    elseif (vecempty)
      ## Find the first non-singleton dimension.
      (dim = find (szx > 1, 1)) || (dim = 1);
      if (isvector (weights))
        if (numel (weights) != szx(dim))
          error ("var: weight vector length does not match operating dimension");
        endif
      elseif (! isequal (size (weights), szx))
          error ("var: weight matrix or array does not match X in size");
      endif
    elseif (vecdim_scalar_vector(1))
      if (isvector (weights))
        if (numel (weights) != szx(vecdim))
          error ("var: weight vector length does not match operating dimension");
        endif
      elseif (! isequal (size (weights), szx))
          error ("var: weight matrix or array does not match X in size");
      endif

    elseif (vecdim_scalar_vector(2) && ! (isequal (size (weights), szx)))
      error ("var: weight matrix or array does not match X in size");
    endif
  endif

  ## Handle special cases of empty or scalar X and exit early.
  if (isempty (x))
    if (vecempty && (ndx == 2 || all (szx == 0)))
      v = NaN (outtype);
      if (nargout > 1)
        m = NaN (outtype);
      endif
      return;
    endif
    if (vecdim_scalar_vector(1))
      szx(vecdim) = 1;
      v = NaN (szx, outtype);
      if (nargout > 1)
        m = NaN (szx, outtype);
      endif
      return;
    endif
  elseif (isscalar (x))
    if (isfinite (x))
      v = zeros (outtype);
    else
      v = NaN (outtype);
    endif
    if (nargout > 1)
      m = x;
    endif
    return;
  endif

  if (nvarg == 0)
    ## Only numeric input argument, no dimensions or weights.
    if (all_flag)
      x = x(:);
      if (omitnan)
        x = x(! isnan (x));
      endif
      n = length (x);
      m = sum (x) ./ n;
      v = sum (abs (x - m) .^ 2) ./ (n - 1 + w);
      if (n == 1)
        v = 0;
      endif
    else
      ## Find the first non-singleton dimension.
      (dim = find (szx > 1, 1)) || (dim = 1);
      n = szx(dim);
      if (omitnan)
        n = sum (! isnan (x), dim);
        xn = isnan (x);
        x(xn) = 0;
      endif
      m = sum (x, dim) ./ n;
      dims = ones (1, ndx);
      dims(dim) = szx(dim);
      if (x_issparse)
        m_exp = repmat (m, dims);
      else
        m_exp = m .* ones (dims);
      endif
      if (omitnan)
        x(xn) = m_exp(xn);
      endif
      v = sumsq (x - m_exp, dim) ./ (n - 1 + w);
      if (numel (n) == 1)
        divby0 = (n .* ones (size (v))) == 1;
      else
        divby0 = n == 1;
      endif
      v(divby0) = 0;
    endif

  elseif (nvarg == 1)
    ## Two numeric input arguments, w or weights given.
    if (all_flag)
      x = x(:);
      if (weighted)
        weights = weights(:);
        wx = weights .* x;
      else
        weights = ones (length (x), 1);
        wx = x;
      endif

      if (omitnan)
        xn = isnan (wx);
        wx = wx(! xn);
        weights = weights(! xn);
        x = x(! xn);
      endif
      n = length (wx);
      m = sum (wx) ./ sum (weights);
      if (weighted)
        v = sum (weights .* (abs (x - m) .^ 2)) ./ sum (weights);
      else
        v = sum (weights .* (abs (x - m) .^ 2)) ./ (n - 1 + w);
        if (n == 1)
          v = 0;
        endif
      endif

    else
      ## Find the first non-singleton dimension.
      (dim = find (szx > 1, 1)) || (dim = 1);
      if (! weighted)
        weights = ones (szx);
        wx = x;
      else
        if (isvector (weights))
          dims = 1:ndx;
          dims([1, dim]) = [dim, 1];
          weights = zeros (szx) + permute (weights(:), dims);
        endif
        wx = weights .* x;
      endif
      n = size (wx, dim);
      if (omitnan)
        xn = isnan (wx);
        n = sum (! xn, dim);
        wx(xn) = 0;
        weights(xn) = 0;
      endif
      m = sum (wx, dim) ./ sum (weights, dim);
      dims = ones (1, ndims (wx));
      dims(dim) = size (wx, dim);
      if (x_issparse)
        m_exp = repmat (m, dims);
      else
        m_exp = m .* ones (dims);
      endif
      if (omitnan)
        x(xn) = m_exp(xn);
      endif
      if (weighted)
        v = sum (weights .* ((x - m_exp) .^ 2), dim) ./ sum (weights, dim);
      else
        v = sumsq (x - m_exp, dim) ./ (n - 1 + w);
        if (numel (n) == 1)
          divby0 = (n .* ones (size (v))) == 1;
        else
          divby0 = n == 1;
        endif
        v(divby0) = 0;
      endif
    endif

  elseif (nvarg == 2)
    ## Three numeric input arguments, both w or weights and dim or vecdim given.
    if (vecdim_scalar_vector(1))
      if (! weighted)
        weights = ones (szx);
        wx = x;
      else
        if (isvector (weights))
          dims = 1:ndx;
          dims([1, vecdim]) = [vecdim, 1];
          weights = zeros (szx) + permute (weights(:), dims);
        endif
        wx = weights .* x;
      endif
      n = size (wx, vecdim);
      if (omitnan)
        n = sum (! isnan (wx), vecdim);
        xn = isnan (wx);
        wx(xn) = 0;
        weights(xn) = 0;
      endif
      m = sum (wx, vecdim) ./ sum (weights, vecdim);
      dims = ones (1, ndims (wx));
      dims(vecdim) = size (wx, vecdim);
      if (x_issparse)
        m_exp = repmat (m, dims);
      else
        m_exp = m .* ones (dims);
      endif
      if (omitnan)
        x(xn) = m_exp(xn);
      endif
      if (weighted)
        v = sum (weights .* ((x - m_exp) .^ 2), vecdim) ...
              ./ sum (weights, vecdim);
      else
        v = sumsq (x - m_exp, vecdim);
        vn = isnan (v);
        v = v ./ (n - 1 + w);
        if (numel (n) == 1)
          divby0 = (n .* ones (size (v))) == 1;
        else
          divby0 = n == 1;
        endif
        v(divby0) = 0;
        v(vn) = NaN;
      endif

    else
      ## Weights and nonscalar vecdim specified

      ## Ignore dimensions in VECDIM larger than actual array.
      remdims = 1 : ndx;    # All dimensions
      vecdim(vecdim > ndx) = [];
      ## Calculate permutation vector
      remdims(vecdim) = [];     # Delete dimensions specified by vecdim
      nremd = numel (remdims);

      ## If all dimensions are given, it is equivalent to the 'all' flag.
      if (nremd == 0)
        x = x(:);
        if (weighted)
          weights = weights(:);
          wx = weights .* x;
        else
          weights = ones (length (x), 1);
          wx = x;
        endif

        if (omitnan)
          xn = isnan (wx);
          wx = wx(! xn);
          weights = weights(! xn);
          x = x(! xn);
        endif
        n = length (wx);
        m = sum (wx) ./ sum (weights);
        if (weighted)
          v = sum (weights .* (abs (x - m) .^ 2)) ./ sum (weights);
        else
          v = sum (weights .* (abs (x - m) .^ 2)) ./ (n - 1 + w);
          if (n == 1)
            v = 0;
          endif
        endif

      else

        ## FIXME: much of the reshaping can be skipped once Octave's sum can
        ##        take a vecdim argument.

        ## Apply weights
        if (weighted)
          wx = weights .* x;
        else
          weights = ones (szx);
          wx = x;
        endif

        ## Permute to push vecdims to back
        perm = [remdims, vecdim];
        wx = permute (wx, perm);
        weights = permute (weights, perm);
        x = permute (x, perm);

        ## Reshape to squash all vecdims in final dimension
        szwx = size (wx);
        sznew = [szwx(1:nremd), prod(szwx(nremd+1:end))];
        wx = reshape (wx, sznew);
        weights = reshape (weights, sznew);
        x = reshape (x, sznew);

        ## Calculate var on final squashed dimension
        dim = nremd + 1;
        n = size (wx, dim);
        if (omitnan)
          xn = isnan (wx);
          n = sum (! xn, dim);
          wx(xn) = 0;
          weights(xn) = 0;
        endif
        m = sum (wx, dim) ./ sum (weights, dim);
        m_exp = zeros (sznew) + m;
        if (omitnan)
          x(xn) = m_exp(xn);
        endif
        if (weighted)
          v = sum (weights .* ((x - m_exp) .^ 2), dim) ./ sum (weights, dim);
        else
          v = sumsq (x - m_exp, dim) ./ (n - 1 + w);
          if (numel (n) == 1)
            divby0 = n .* ones (size (v)) == 1;
          else
            divby0 = n == 1;
          endif
          v(divby0) = 0;
        endif

        ## Inverse permute back to correct dimensions
        v = ipermute (v, perm);
        if (nargout > 1)
          m = ipermute (m, perm);
        endif
      endif
    endif
  endif

  ## Preserve class type
  if (nargout < 2)
    if (strcmp (outtype, "single"))
      v = single (v);
    else
      v = double (v);
    endif
  else
    if (strcmp (outtype, "single"))
      v = single (v);
      m = single (m);
    else
      v = double (v);
      m = double (m);
    endif
  endif

endfunction


%!assert (var (13), 0)
%!assert (var (single (13)), single (0))
%!assert (var ([1,2,3]), 1)
%!assert (var ([1,2,3], 1), 2/3, eps)
%!assert (var ([1,2,3], [], 1), [0,0,0])
%!assert (var ([1,2,3], [], 3), [0,0,0])
%!assert (var (5, 99), 0)
%!assert (var (5, 99, 1), 0)
%!assert (var (5, 99, 2), 0)
%!assert (var ([5 3], [99 99], 2), 1)
%!assert (var ([1:7], [1:7]), 3)
%!assert (var ([eye(3)], [1:3]), [5/36, 2/9, 1/4], eps)
%!assert (var (ones (2,2,2), [1:2], 3), [(zeros (2,2))])
%!assert (var ([1 2; 3 4], 0, 'all'), var ([1:4]))
%!assert (var (reshape ([1:8], 2, 2, 2), 0, [1 3]), [17/3 17/3], eps)
%!assert (var ([1 2 3;1 2 3], [], [1 2]), 0.8, eps)

## Test single input and optional arguments "all", DIM, "omitnan")
%!test
%! x = [-10:10];
%! y = [x;x+5;x-5];
%! assert (var (x), 38.5);
%! assert (var (y, [], 2), [38.5; 38.5; 38.5]);
%! assert (var (y, 0, 2), [38.5; 38.5; 38.5]);
%! assert (var (y, 1, 2), ones (3,1) * 36.66666666666666, 1e-14);
%! assert (var (y, "all"), 54.19354838709678, 1e-14);
%! y(2,4) = NaN;
%! assert (var (y, "all"), NaN);
%! assert (var (y, "all", "includenan"), NaN);
%! assert (var (y, "all", "omitnan"), 55.01533580116342, 1e-14);
%! assert (var (y, 0, 2, "includenan"), [38.5; NaN; 38.5]);
%! assert (var (y, [], 2), [38.5; NaN; 38.5]);
%! assert (var (y, [], 2, "omitnan"), [38.5; 37.81842105263158; 38.5], 1e-14);

## Tests for different weight and omitnan code paths
%!assert (var ([1 NaN 3], [1 2 3], "omitnan"), 0.75, eps)
%!assert (var ([1 2 3], [1 NaN 3], "omitnan"), 0.75, eps)
%!assert (var (magic(3), [1 NaN 3], "omitnan"), [3 12 3], eps)
%!assert (var ([1 NaN 3], [1 2 3], "omitnan", "all"), 0.75, eps)
%!assert (var ([1 NaN 3], [1 2 3], "all", "omitnan"), 0.75, eps)
%!assert (var ([1 2 3], [1 NaN 3], "omitnan", "all"), 0.75, eps)
%!assert (var ([1 NaN 3], [1 2 3], 2, "omitnan"), 0.75, eps)
%!assert (var ([1 2 3], [1 NaN 3], 2, "omitnan"), 0.75, eps)
%!assert (var (magic(3), [1 NaN 3], 1, "omitnan"), [3 12 3], eps)
%!assert (var (magic(3), [1 NaN 3], 2, "omitnan"), [0.75;3;0.75], eps)
%!assert (var ([4 4; 4 6; 6 6], [1 3], 2, 'omitnan'), [0;0.75;0], eps)
%!assert (var ([4 NaN; 4 6; 6 6], [1 2 3], 1, 'omitnan'), [1 0])
%!assert (var ([4 NaN; 4 6; 6 6], [1 3], 2, 'omitnan'), [0;0.75;0], eps)
%!assert (var (3*reshape(1:18, [3 3 2]), [1 2 3], 1, 'omitnan'), ones(1,3,2)*5)
%!assert (var (reshape(1:18, [3 3 2]), [1 2 3], 2, 'omitnan'), 5*ones(3,1,2))
%!assert (var (3*reshape(1:18, [3 3 2]), ones (3,3,2), [1 2], 'omitnan'), ...
%!         60 * ones(1,1,2))
%!assert (var (3*reshape(1:18, [3 3 2]), ones (3,3,2), [1 4], 'omitnan'), ...
%!         6 * ones(1,3,2))
%!assert (var (6*reshape(1:18, [3 3 2]), ones (3,3,2), [1:3], 'omitnan'), 969)
%!test
%! x = reshape(1:18, [3 3 2]);
%! x([2, 14]) = NaN;
%! w = ones (3,3,2);
%! assert (var (16*x, w, [1:3], 'omitnan'), 6519);
%!test
%! x = reshape(1:18, [3 3 2]);
%! w = ones (3,3,2);
%! w([2, 14]) = NaN;
%! assert (var (16*x, w, [1:3], 'omitnan'), 6519);

## Test input case insensitivity
%!assert (var ([1 2 3], "aLl"), 1)
%!assert (var ([1 2 3], "OmitNan"), 1)
%!assert (var ([1 2 3], "IncludeNan"), 1)

## Test dimension indexing with vecdim in n-dimensional arrays
%!test
%! x = repmat ([1:20;6:25], [5, 2, 6, 3]);
%! assert (size (var (x, 0, [3 2])), [10, 1, 1, 3]);
%! assert (size (var (x, 1, [1 2])), [1, 1, 6, 3]);
%! assert (size (var (x, [], [1 2 4])), [1, 1, 6]);
%! assert (size (var (x, 0, [1 4 3])), [1, 40]);
%! assert (size (var (x, [], [1 2 3 4])), [1, 1]);

## Test matrix with vecdim, weighted, matrix weights, omitnan
%!assert (var (3*magic(3)), [63 144 63])
%!assert (var (3*magic(3), 'omitnan'), [63 144 63])
%!assert (var (3*magic(3), 1), [42 96 42])
%!assert (var (3*magic(3), 1, 'omitnan'), [42 96 42])
%!assert (var (3*magic(3), ones(1,3), 1), [42 96 42])
%!assert (var (3*magic(3), ones(1,3), 1, 'omitnan'), [42 96 42])
%!assert (var (2*magic(3), [1 1 NaN], 1, 'omitnan'), [25 16 1])
%!assert (var (3*magic(3), ones(3,3)), [42 96 42])
%!assert (var (3*magic(3), ones(3,3), 'omitnan'), [42 96 42])
%!assert (var (3*magic(3), [1 1 1; 1 1 1; 1 NaN 1], 'omitnan'), [42 36 42])
%!assert (var (3*magic(3), ones(3,3), 1), [42 96 42])
%!assert (var (3*magic(3), ones(3,3), 1, 'omitnan'), [42 96 42])
%!assert (var (3*magic(3), [1 1 1; 1 1 1; 1 NaN 1], 1, 'omitnan'), [42 36 42])
%!assert (var (3*magic(3), ones(3,3), [1 4]), [42 96 42])
%!assert (var (3*magic(3), ones(3,3), [1 4], 'omitnan'), [42 96 42])
%!assert (var (3*magic(3), [1 1 1; 1 1 1; 1 NaN 1],[1 4],'omitnan'), [42 36 42])

## Test results with vecdim in n-dimensional arrays and "omitnan"
%!test
%! x = repmat ([1:20;6:25], [5, 2, 6, 3]);
%! v = repmat (33.38912133891213, [10, 1, 1, 3]);
%! assert (var (x, 0, [3, 2]), v, 1e-14);
%! v = repmat (33.250, [10, 1, 1, 3]);
%! assert (var (x, 1, [3, 2]), v, 1e-14);
%! x(2,5,6,3) = NaN;
%! v(2,1,1,3) = NaN;
%! assert (var (x, 1, [3, 2]), v, 4e-14);
%! v = repmat (33.38912133891213, [10 1 1 3]);
%! v(2,1,1,3) = NaN;
%! assert (var (x, [], [3, 2]), v, 4e-14);
%! v(2,1,1,3) = 33.40177912169048;
%! assert (var (x, [], [3, 2], "omitnan"), v, 4e-14);

## Testing weights vector & arrays
%!assert (var (ones (2,2,2), [1:2], 3), [(zeros (2, 2))])
%!assert (var (magic (3), [1:9], "all"), 6.666666666666667, 1e-14)

## Test exceeding dimensions
%!assert (var (ones (2,2), [], 3), zeros (2,2))
%!assert (var (ones (2,2,2), [], 99), zeros (2,2,2))
%!assert (var (magic (3), [], 3), zeros (3,3))
%!assert (var (magic (3), [], 1), [7, 16, 7])
%!assert (var (magic (3), [], [1 3]), [7, 16, 7])
%!assert (var (magic (3), [], [1 99]), [7, 16, 7])

## Test empty inputs
%!assert (var ([]), NaN)
%!assert (class (var (single ([]))), "single")
%!assert (var ([],[],1), NaN(1,0))
%!assert (var ([],[],2), NaN(0,1))
%!assert (var ([],[],3), [])
%!assert (class (var (single ([]), [], 1)), "single")
%!assert (var (ones (1,0)), NaN)
%!assert (var (ones (1,0), [], 1), NaN(1,0))
%!assert (var (ones (1,0), [], 2), NaN)
%!assert (var (ones (1,0), [], 3), NaN(1,0))
%!assert (class (var (ones (1, 0, "single"), [], 1)), "single")
%!assert (var (ones (0,1)), NaN)
%!assert (var (ones (0,1), [], 1), NaN)
%!assert (var (ones (0,1), [], 2), NaN(0,1))
%!assert (var (ones (0,1), [], 3), NaN(0,1))
%!assert (var (ones (1,3,0,2)), NaN(1,1,0,2))
%!assert (var (ones (1,3,0,2), [], 1), NaN(1,3,0,2))
%!assert (var (ones (1,3,0,2), [], 2), NaN(1,1,0,2))
%!assert (var (ones (1,3,0,2), [], 3), NaN(1,3,1,2))
%!assert (var (ones (1,3,0,2), [], 4), NaN(1,3,0))
%!test
%! [~, m] = var ([]);
%! assert (m, NaN);

## Test optional mean output
%!test <*62395>
%! [~, m] = var (13);
%! assert (m, 13);
%! [~, m] = var (single(13));
%! assert (m, single(13));
%! [~, m] = var ([1, 2, 3; 3 2 1], []);
%! assert (m, [2 2 2]);
%! [~, m] = var ([1, 2, 3; 3 2 1], [], 1);
%! assert (m, [2 2 2]);
%! [~, m] = var ([1, 2, 3; 3 2 1], [], 2);
%! assert (m, [2 2]');
%! [~, m] = var ([1, 2, 3; 3 2 1], [], 3);
%! assert (m, [1 2 3; 3 2 1]);

## Test mean output, weighted inputs, vector dims
%!test <*62395>
%! [~, m] = var (5,99);
%! assert (m, 5);
%! [~, m] = var ([1:7], [1:7]);
%! assert (m, 5);
%! [~, m] = var ([eye(3)], [1:3]);
%! assert (m, [1/6, 1/3, 0.5], eps);
%! [~, m] = var (ones (2,2,2), [1:2], 3);
%! assert (m, ones (2,2));
%! [~, m] = var ([1 2; 3 4], 0, 'all');
%! assert (m, 2.5, eps);
%! [~, m] = var (reshape ([1:8], 2, 2, 2), 0, [1 3]);
%! assert (m, [3.5, 5.5], eps);
%!test
%! [v, m] = var (4 * eye (2), [1, 3]);
%! assert (v, [3, 3]);
%! assert (m, [1, 3]);

## Test mean output, empty inputs, omitnan
%!test <*62395>
%! [~, m] = var ([]);
%! assert (m, NaN);
#%! [~, m] = var ([],[],1);
#%! assert (m, NaN(1,0));
#%! [~, m] = var ([],[],2);
#%! assert (m, NaN(0,1));
#%! [~, m] = var ([],[],3);
#%! assert (m, []);
#%! [~, m] = var (ones (1,3,0,2));
#%! assert (m, NaN(1,1,0,2));

## Test mean output, nD array
%!test <*62395>
%! x = repmat ([1:20;6:25], [5, 2, 6, 3]);
%! [~, m] = var (x, 0, [3 2]);
%! assert (m, mean (x, [3 2]));
%! [~, m] = var (x, 0, [1 2]);
%! assert (m, mean (x, [1 2]));
%! [~, m] = var (x, 0, [1 3 4]);
%! assert (m, mean (x, [1 3 4]));
%!test
%! x = repmat ([1:20;6:25], [5, 2, 6, 3]);
%! x(2,5,6,3) = NaN;
%! [~, m] = var (x, 0, [3 2], "omitnan");
%! assert (m, mean (x, [3 2], "omitnan"));

## Test Inf and NaN inputs
%!test <*63203>
%! [v, m] = var (Inf);
%! assert (v, NaN);
%! assert (m, Inf);
%!test <*63203>
%! [v, m] = var (NaN);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3]);
%! assert (v, NaN);
%! assert (m, Inf);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3]');
%! assert (v, NaN);
%! assert (m, Inf);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3]);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3]');
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3], [], 1);
%! assert (v, [0, NaN, 0]);
%! assert (m, [1, Inf, 3]);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3], [], 2);
%! assert (v, NaN);
%! assert (m, Inf);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3], [], 3);
%! assert (v, [0, NaN, 0]);
%! assert (m, [1, Inf, 3]);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3], [], 1);
%! assert (v, [0, NaN, 0]);
%! assert (m, [1, NaN, 3]);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3], [], 2);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3], [], 3);
%! assert (v, [0, NaN, 0]);
%! assert (m, [1, NaN, 3]);
%!test <*63203>
%! [v, m] = var ([1, 2, 3; 3, Inf, 5]);
%! assert (v, [2, NaN, 2]);
%! assert (m, [2, Inf, 4]);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3; 3, Inf, 5]);
%! assert (v, [2, NaN, 2]);
%! assert (m, [2, Inf, 4]);
%!test <*63203>
%! [v, m] = var ([1, 2, 3; 3, NaN, 5]);
%! assert (v, [2, NaN, 2]);
%! assert (m, [2, NaN, 4]);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3; 3, NaN, 5]);
%! assert (v, [2, NaN, 2]);
%! assert (m, [2, NaN, 4]);
%!test <*63203>
%! [v, m] = var ([Inf, 2, NaN]);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([Inf, 2, NaN]');
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([NaN, 2, Inf]);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([NaN, 2, Inf]');
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([Inf, 2, NaN], [], 1);
%! assert (v, [NaN, 0, NaN]);
%! assert (m, [Inf, 2, NaN]);
%!test <*63203>
%! [v, m] = var ([Inf, 2, NaN], [], 2);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([NaN, 2, Inf], [], 1);
%! assert (v, [NaN, 0, NaN]);
%! assert (m, [NaN, 2, Inf]);
%!test <*63203>
%! [v, m] = var ([NaN, 2, Inf], [], 2);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, 3, NaN; 3, 5, Inf]);
%! assert (v, [2, 2, NaN]);
%! assert (m, [2, 4, NaN]);
%!test <*63203>
%! [v, m] = var ([1, 3, Inf; 3, 5, NaN]);
%! assert (v, [2, 2, NaN]);
%! assert (m, [2, 4, NaN]);

## Test sparse/diagonal inputs
%!test <*63291>
%! [v, m] = var (2 * eye (2));
%! assert (v, [2, 2]);
%! assert (m, [1, 1]);
%!test <*63291>
%! [v, m] = var (4 * eye (2), [1, 3]);
%! assert (v, [3, 3]);
%! assert (m, [1, 3]);
%!test <*63291>
%! [v, m] = var (sparse (2 * eye (2)));
%! assert (full (v), [2, 2]);
%! assert (full (m), [1, 1]);
%!test <*63291>
%! [v, m] = var (sparse (4 * eye (2)), [1, 3]);
%! assert (full (v), [3, 3]);
%! assert (full (m), [1, 3]);
%!test<*63291>
%! [v, m] = var (sparse (eye (2)));
%! assert (issparse (v));
%! assert (issparse (m));
%!test<*63291>
%! [v, m] = var (sparse (eye (2)), [1, 3]);
%! assert (issparse (v));
%! assert (issparse (m));

## Test input validation
%!error <Invalid call> var ()
%!error <Invalid call> var (1, 2, "omitnan", 3)
%!error <Invalid call> var (1, 2, 3, 4)
%!error <Invalid call> var (1, 2, 3, 4, 5)
%!error <Invalid call> var (1, "foo")
%!error <Invalid call> var (1, [], "foo")
%!error <normalization scalar must be either 0 or 1> var ([1 2 3], 2)
%!error <normalization scalar must be either 0 or 1> var ([1 2], 2, "all")
%!error <normalization scalar must be either 0 or 1> var ([1 2],0.5, "all")
%!error <weights must not contain any negative values> var (1, -1)
%!error <weights must not contain any negative values> var (1, [1 -1])
%!error <weights must not contain any negative values> ...
%! var ([1 2 3], [1 -1 0])
%!error <X must be a numeric vector or matrix> var ({1:5})
%!error <X must be a numeric vector or matrix> var ("char")
%!error <X must be a numeric vector or matrix> var (['A'; 'B'])
%!error <DIM must be a positive integer> var (1, [], ones (2,2))
%!error <DIM must be a positive integer> var (1, 0, 1.5)
%!error <DIM must be a positive integer> var (1, [], 0)
%!error <DIM must be a positive integer> var (1, [], 1.5)
%!error <DIM must be a positive integer> var ([1 2 3], [], [-1 1])
%!error <VECDIM must contain non-repeating positive integers> ...
%! var (repmat ([1:20;6:25], [5 2 6 3]), 0, [1 2 2 2])
%!error <weight matrix or array does not match X in size> ...
%! var ([1 2], eye (2))
%!error <weight matrix or array does not match X in size> ...
%! var ([1 2 3 4], [1 2; 3 4])
%!error <weight matrix or array does not match X in size> ...
%! var ([1 2 3 4], [1 2; 3 4], 1)
%!error <weight matrix or array does not match X in size> ...
%! var ([1 2 3 4], [1 2; 3 4], [2 3])
%!error <weight matrix or array does not match X in size> ...
%! var (ones (2, 2), [1 2], [1 2])
%!error <weight matrix or array does not match X in size> ...
%! var ([1 2 3 4; 5 6 7 8], [1 2 1 2 1; 1 2 1 2 1], 1)
%!error <weight matrix or array does not match X in size> ...
%! var (repmat ([1:20;6:25], [5 2 6 3]), repmat ([1:20;6:25], [5 2 3]), [2 3])
%!error <weight vector length does not match> var ([1 2 3; 2 3 4], [1 3 4])
%!error <weight vector length does not match> var ([1 2], [1 2 3])
%!error <weight vector length does not match> var (1, [1 2])
%!error <weight vector length does not match> var ([1 2 3; 2 3 4], [1 3 4], 1)
%!error <weight vector length does not match> var ([1 2 3; 2 3 4], [1 3], 2)
%!error <weight vector length does not match> var ([1 2], [1 2], 1)
%!error <'all' flag cannot be used with DIM or VECDIM options> ...
%! var (1, [], 1, "all")
%!error <weight vector element count does not match X> ...
%! var ([1 2 3; 2 3 4], [1 3], "all")
%!error <weight matrix or array does not match X in size> ...
%! var (repmat ([1:20;6:25], [5 2 6 3]), repmat ([1:20;6:25], [5 2 3]), "all")
