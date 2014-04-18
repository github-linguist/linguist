% DIFF_VAL  Find differents values of a vector or a matrix.
%       DIFF_VAL(X) returns differing elements of X in ascending order.
function val=diff_val(vec);

if length(vec)<=1 val=vec; return, end
val=sort(vec);
val(find(diff(val)==0))=[];