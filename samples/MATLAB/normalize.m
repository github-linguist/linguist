function [ d, d_mean, d_std ] = normalize( d )
    d_mean = mean(d);
    d = d - repmat(d_mean, size(d,1), 1);
    d_std = std(d);
    d = d./ repmat(d_std, size(d,1), 1);
end
