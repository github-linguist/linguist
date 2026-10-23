classdef (Abstract) SignalSource < handle
    methods (Abstract)
        values = read(obj, count)
    end
end
