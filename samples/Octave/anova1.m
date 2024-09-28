## Copyright (C) 2021-2022 Andreas Bertsatos <abertsatos@biol.uoa.gr>
## Copyright (C) 2022 Andrew Penn <A.C.Penn@sussex.ac.uk>
##
## This file is part of the statistics package for GNU Octave.
##
## This program is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {statistics} {@var{p} =} anova1 (@var{x})
## @deftypefnx {statistics} {@var{p} =} anova1 (@var{x}, @var{group})
## @deftypefnx {statistics} {@var{p} =} anova1 (@var{x}, @var{group}, @var{displayopt})
## @deftypefnx {statistics} {@var{p} =} anova1 (@var{x}, @var{group}, @var{displayopt}, @var{vartype})
## @deftypefnx {statistics} {[@var{p}, @var{atab}] =} anova1 (@var{x}, @dots{})
## @deftypefnx {statistics} {[@var{p}, @var{atab}, @var{stats}] =} anova1 (@var{x}, @dots{})
##
## Perform a one-way analysis of variance (ANOVA) for comparing the means of two
## or more groups of data under the null hypothesis that the groups are drawn
## from distributions with the same mean. For planned contrasts and/or
## diagnostic plots, use @qcode{anovan} instead.
##
## anova1 can take up to three input arguments:
##
## @itemize
## @item
## @var{x} contains the data and it can either be a vector or matrix.
## If @var{x} is a matrix, then each column is treated as a separate group.
## If @var{x} is a vector, then the @var{group} argument is mandatory.
##
## @item
## @var{group} contains the names for each group.  If @var{x} is a matrix, then
## @var{group} can either be a cell array of strings of a character array, with
## one row per column of @var{x}.  If you want to omit this argument, enter an
## empty array ([]).  If @var{x} is a vector, then @var{group} must be a vector
## of the same length, or a string array or cell array of strings with one row
## for each element of @var{x}.  @var{x} values corresponding to the same value
## of @var{group} are placed in the same group.
##
## @item
## @var{displayopt} is an optional parameter for displaying the groups contained
## in the data in a boxplot.  If omitted, it is 'on' by default.  If group names
## are defined in @var{group}, these are used to identify the groups in the
## boxplot. Use 'off' to omit displaying this figure.
##
## @item
## @var{vartype} is an optional parameter to used to indicate whether the
## groups can be assumed to come from populations with equal variance. When
## @qcode{vartype} is @qcode{"equal"} the variances are assumed to be equal
## (this is the default). When @qcode{vartype} is @qcode{"unequal"} the
## population variances are not assumed to be equal and Welch's ANOVA test is
## used instead.
## @end itemize
##
## anova1 can return up to three output arguments:
##
## @itemize
## @item
## @var{p} is the p-value of the null hypothesis that all group means are equal.
##
## @item
## @var{atab} is a cell array containing the results in a standard ANOVA table.
##
## @item
## @var{stats} is a structure containing statistics useful for performing
## a multiple comparison of means with the MULTCOMPARE function.
## @end itemize
##
## If anova1 is called without any output arguments, then it prints the results
## in a one-way ANOVA table to the standard output. It is also printed when
## @var{displayopt} is 'on'.
##
##
## Examples:
##
## @example
## x = meshgrid (1:6);
## x = x + normrnd (0, 1, 6, 6);
## anova1 (x, [], 'off');
## [p, atab] = anova1(x);
## @end example
##
##
## @example
## x = ones (50, 4) .* [-2, 0, 1, 5];
## x = x + normrnd (0, 2, 50, 4);
## groups = @{"A", "B", "C", "D"@};
## anova1 (x, groups);
## @end example
##
## @seealso{anova2, anovan, multcompare}
## @end deftypefn

function [p, anovatab, stats] = anova1 (x, group, displayopt, vartype)

  ## Check for valid number of input arguments
  if (nargin < 1 || nargin > 4)
    error ("anova1: invalid number of input arguments.");
  endif
  ## Add defaults
  if (nargin < 2)
    group = [];
  endif
  if (nargin < 3)
    displayopt = "on";
  endif
  if (nargin < 4)
    vartype = "equal";
  endif
  plotdata = ! (strcmp (displayopt, 'off'));

  ## Convert group to cell array from character array, make it a column
  if (! isempty (group) && ischar (group))
    group = cellstr (group);
  endif
  if (size (group, 1) == 1)
    group = group';
  endif

  ## If x is a matrix, convert it to column vector and create a
  ## corresponging column vector for groups
  if (length (x) < prod (size (x)))
    [n, m] = size (x);
    x = x(:);
    gi = reshape (repmat ((1:m), n, 1), n*m, 1);
    if (length (group) == 0)          ## no group names are provided
      group = gi;
    elseif (size (group, 1) == m)     ## group names exist and match columns
      group = group(gi,:);
    else
      error ("anova1: columns in X and GROUP length do not match.");
    endif
  endif

  ## Check that x and group are the same size
  if (! all (numel (x) == numel (group)))
    error ("anova1: GROUP must be a vector with the same number of rows as x.");
  endif

  ## Identify NaN values (if any) and remove them from X along with
  ## their corresponding values from group vector
  nonan = ! isnan (x);
  x = x(nonan);
  group = group(nonan, :);

  ## Convert group to indices and separate names
  [group_id, group_names] = grp2idx (group);
  group_id = group_id(:);
  named = 1;

  ## Center data to improve accuracy and keep uncentered data for ploting
  xorig = x;
  mu = mean(x);
  x = x - mu;
  xr = x;

  ## Get group size and mean for each group
  groups = size (group_names, 1);
  xs = zeros (1, groups);
  xm = xs;
  xv = xs;
  for j = 1:groups
    group_size = find (group_id == j);
    xs(j) = length (group_size);
    xm(j) = mean (xr(group_size));
    xv(j) = var (xr(group_size), 0);
  endfor

  ## Calculate statistics
  lx = length (xr);                       ## Number of samples in groups
  gm = mean (xr);                         ## Grand mean of groups
  dfm = length (xm) - 1;                  ## degrees of freedom for model
  dfe = lx - dfm - 1;                     ## degrees of freedom for error
  SSM = xs .* (xm - gm) * (xm - gm)';     ## Sum of Squares for Model
  SST = (xr(:) - gm)' * (xr(:) - gm);     ## Sum of Squares Total
  SSE = SST - SSM;                        ## Sum of Squares Error
  if (dfm > 0)
      MSM = SSM / dfm;                    ## Mean Square for Model
  else
      MSM = NaN;
  endif
  if (dfe > 0)
      MSE = SSE / dfe;                    ## Mean Square for Error
  else
      MSE = NaN;
  endif
  ## Calculate F statistic
  if (SSE != 0)                     ## Regular Matrix case.
    switch (lower (vartype))
      case "equal"
        ## Assume equal variances (Fisher's One-way ANOVA)
        F = (SSM / dfm) / MSE;
      case "unequal"
        ## Accomodate for unequal variances (Welch's One-way ANOVA)
        ## Calculate the sampling variance for each group (i.e. the square of the SEM)
        sv = xv ./ xs;
        ## Calculate weights as the reciprocal of the sampling variance
        w = 1 ./ sv;
        ## Calculate the origin
        ori = sum (w .* xm) ./ sum (w);
        ## Calculate Welch's F statistic
        F = (groups - 1)^-1 * sum (w .* (xm - ori).^2) /...
            (1 + ((2 * (groups - 2)/(groups^2 - 1)) * ...
            sum ((1 - w / sum (w)).^2 .* (xs - 1).^-1)));
        ## Welch's test does not use a pooled error term
        MSE = NaN;
        ## Correct the error degrees of freedom
        dfe = (3 /(groups^2 - 1) * sum ((1 - w / sum (w)).^2 .* (xs-1).^-1))^-1;
      otherwise
        error ("anova1: invalid fourth (vartype) argument to anova1.");
    endswitch
    p = 1 - fcdf (F, dfm, dfe);     ## Probability of F given equal means.
  elseif (SSM == 0)                 ## Constant Matrix case.
    F = 0;
    p = 1;
  else                              ## Perfect fit case.
    F = Inf;
    p = 0;
  end

  ## Create results table (if requested)
  if (nargout > 1)
    switch (lower (vartype))
      case "equal"
        anovatab = {"Source", "SS", "df", "MS", "F", "Prob>F"; ...
                    "Groups", SSM, dfm, MSM, F, p; ...
                    "Error", SSE, dfe, MSE, "", ""; ...
                    "Total", SST, dfm + dfe, "", "", ""};
      case "unequal"
        anovatab = {"Source", "F", "df", "dfe", "F", "Prob>F"; ...
                    "Groups", SSM, dfm, dfe, F, p};
    endswitch
  endif
  ## Create stats structure (if requested) for MULTCOMPARE
  if (nargout > 2)
    if (length (group_names) > 0)
        stats.gnames = group_names;
    else
        stats.gnames = strjust (num2str ((1:length (xm))'), 'left');
    end
    stats.n = xs;
    stats.source = 'anova1';
    stats.vartype = vartype;
    stats.means = xm + mu;
    stats.vars = xv;
    stats.df = dfe;
    stats.s = sqrt (MSE);
  endif
  ## Print results table on screen if no output argument was requested
  if (nargout == 0 || plotdata)
    switch (lower (vartype))
      case "equal"
        printf("\n                      ANOVA Table\n\n");
        printf("Source        SS      df        MS       F      Prob>F\n");
        printf("------------------------------------------------------\n");
        printf("Groups  %10.4f %5.0f %10.4f %8.2f %9.4f\n", SSM, dfm, MSM, F, p);
        printf("Error   %10.4f %5.0f %10.4f\n", SSE, dfe, MSE);
        printf("Total   %10.4f %5.0f\n\n", SST, dfm + dfe);
      case "unequal"
        printf("\n           Welch's ANOVA Table\n\n");
        printf("Source        F     df     dfe     Prob>F\n");
        printf("-----------------------------------------\n");
        printf("Groups  %8.2f %5.0f %7.2f %10.4f\n\n", F, dfm, dfe, p);
    endswitch
  endif
  ## Plot data using BOXPLOT (unless opted out)
  if (plotdata)
    boxplot (x, group_id, "Notch", "on", "Labels", group_names);
  endif
endfunction


%!demo
%! x = meshgrid (1:6);
%! randn ("seed", 15);    # for reproducibility
%! x = x + normrnd (0, 1, 6, 6);
%! anova1 (x, [], 'off');

%!demo
%! x = meshgrid (1:6);
%! randn ("seed", 15);    # for reproducibility
%! x = x + normrnd (0, 1, 6, 6);
%! [p, atab] = anova1(x);

%!demo
%! x = ones (50, 4) .* [-2, 0, 1, 5];
%! randn ("seed", 13);    # for reproducibility
%! x = x + normrnd (0, 2, 50, 4);
%! groups = {"A", "B", "C", "D"};
%! anova1 (x, groups);

%!demo
%! y = [54 87 45; 23 98 39; 45 64 51; 54 77 49; 45 89 50; 47 NaN 55];
%! g = [1  2  3 ; 1  2  3 ; 1  2  3 ; 1  2  3 ; 1  2  3 ; 1  2  3 ];
%! anova1 (y(:), g(:), "on", "unequal");

## testing against GEAR.DAT data file and results for one-factor ANOVA from
## https://www.itl.nist.gov/div898/handbook/eda/section3/eda354.htm
%!test
%! data = [1.006, 0.996, 0.998, 1.000, 0.992, 0.993, 1.002, 0.999, 0.994, 1.000, ...
%!         0.998, 1.006, 1.000, 1.002, 0.997, 0.998, 0.996, 1.000, 1.006, 0.988, ...
%!         0.991, 0.987, 0.997, 0.999, 0.995, 0.994, 1.000, 0.999, 0.996, 0.996, ...
%!         1.005, 1.002, 0.994, 1.000, 0.995, 0.994, 0.998, 0.996, 1.002, 0.996, ...
%!         0.998, 0.998, 0.982, 0.990, 1.002, 0.984, 0.996, 0.993, 0.980, 0.996, ...
%!         1.009, 1.013, 1.009, 0.997, 0.988, 1.002, 0.995, 0.998, 0.981, 0.996, ...
%!         0.990, 1.004, 0.996, 1.001, 0.998, 1.000, 1.018, 1.010, 0.996, 1.002, ...
%!         0.998, 1.000, 1.006, 1.000, 1.002, 0.996, 0.998, 0.996, 1.002, 1.006, ...
%!         1.002, 0.998, 0.996, 0.995, 0.996, 1.004, 1.004, 0.998, 0.999, 0.991, ...
%!         0.991, 0.995, 0.984, 0.994, 0.997, 0.997, 0.991, 0.998, 1.004, 0.997];
%! group = [1:10] .* ones (10,10);
%! group = group(:);
%! [p, tbl] = anova1 (data, group, "off");
%! assert (p, 0.022661, 1e-6);
%! assert (tbl{2,5}, 2.2969, 1e-4);
%! assert (tbl{2,3}, 9, 0);
%! assert (tbl{4,2}, 0.003903, 1e-6);
%! data = reshape (data, 10, 10);
%! [p, tbl, stats] = anova1 (data, [], "off");
%! assert (p, 0.022661, 1e-6);
%! assert (tbl{2,5}, 2.2969, 1e-4);
%! assert (tbl{2,3}, 9, 0);
%! assert (tbl{4,2}, 0.003903, 1e-6);
%! means = [0.998, 0.9991, 0.9954, 0.9982, 0.9919, 0.9988, 1.0015, 1.0004, 0.9983, 0.9948];
%! N = 10 * ones (1, 10);
%! assert (stats.means, means, 1e-6);
%! assert (length (stats.gnames), 10, 0);
%! assert (stats.n, N, 0);

## testing against one-way ANOVA example dataset from GraphPad Prism 8
%!test
%! y = [54 87 45; 23 98 39; 45 64 51; 54 77 49; 45 89 50; 47 NaN 55];
%! g = [1  2  3 ; 1  2  3 ; 1  2  3 ; 1  2  3 ; 1  2  3 ; 1  2  3 ];
%! [p, tbl] = anova1 (y(:), g(:), "off", "equal");
%! assert (p, 0.00004163, 1e-6);
%! assert (tbl{2,5}, 22.573418, 1e-6);
%! assert (tbl{2,3}, 2, 0);
%! assert (tbl{3,3}, 14, 0);
%! [p, tbl] = anova1 (y(:), g(:), "off", "unequal");
%! assert (p, 0.00208877, 1e-8);
%! assert (tbl{2,5}, 15.523192, 1e-6);
%! assert (tbl{2,3}, 2, 0);
%! assert (tbl{2,4}, 7.5786897, 1e-6);
