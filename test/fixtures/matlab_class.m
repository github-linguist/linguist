classdef matlab_class
    properties
        R;
        G;
        B;
    end
    methods
        function obj = matlab_class(r,g,b)
            obj.R = r;
            obj.G = g;
            obj.B = b;
        end
        function disp(obj)
            disp(['Red: ' num2str(obj.R) ...
                ', Green: ' num2str(obj.G) ...
                ', Blue: ' num2str(obj.B)]);
        end
    end
    enumeration
        red     (1,0,0)
        green   (0,1,0)
        blue    (0,0,1)
        cyan    (0,1,1)
        magenta (1,0,1)
        yellow  (1,1,0)
        black   (0,0,0)
        white   (1,1,1)
    end
end