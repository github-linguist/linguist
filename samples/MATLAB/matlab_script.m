% Matlab example script

%Call matlab_function function which resides in the same directory

value1 = 5 % semicolon at end of line is not mandatory, only suppresses output to command line.
value2 = 3

% Calculate sum of value1 and value2
result = matlab_function(value1,value2);

disp('called from script')
disp(result);
