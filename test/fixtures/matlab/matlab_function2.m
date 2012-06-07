   function ret = matlab_function2(A,B)
% Simple function that combines two values using function handles and displays
% the return value

% create function handles
fun1=@interface;
fun2=@implementation;
fun3=@property;
fun4=@synthesize;

% use function handles
ret = fun1(A)+fun2(A)+fun3(B)+fun4(B);

% Display the return value
disp('Return value in function');
disp(ret);


function A=interface(A)
% simple sub-function with same name Objective-C @keyword
A=2*A;

function A=implementation(A)
% simple sub-function with same name Objective-C @keyword
A=A^2;

function B=property(B)
% simple sub-function with same name Objective-C @keyword
B=2*B;

function B=synthesize(B)
% simple sub-function with same name Objective-C @keyword
B=B^2;