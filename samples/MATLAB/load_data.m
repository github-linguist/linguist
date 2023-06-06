function data = load_data(filename, varargin)
% function data = load_data(filename, varargin)
%
% Returns an iddata object with the input thetac and output theta for the
% given file.
%
% Parameters
% ----------
% filename : char
%   Filename for the data file.
% varargin : char value pairs, optional
%   sampleTime : double, default=0.0005
%   detread : boolean, default=true
%   directory : char, default='data'

parser = inputParser;
parser.addRequired('filename');
parser.addParamValue('sampleTime', 0.0005);
parser.addParamValue('detrend', true);
parser.addParamValue('directory', 'data');
parser.parse(filename, varargin{:});
args = parser.Results;

raw = load([args.directory filesep filename]);

data = iddata(raw.theta, raw.theta_c, args.sampleTime, ...
             'InterSample', 'foh', ...
             'InputName', {'thetac'}, ...
             'OutputName', {'theta'});

if args.detrend
    data = detrend(data);
end
