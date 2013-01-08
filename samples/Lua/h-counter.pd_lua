
-- A simple counting object that increments an internal counter whenever it receives a bang at its first inlet, or changes to whatever number it receives at its second inlet.

local HelloCounter = pd.Class:new():register("h-counter")

function HelloCounter:initialize(sel, atoms)
	self.inlets = 2
	self.outlets = 1
	self.num = 0
	return true
end

function HelloCounter:in_1_bang()
	self:outlet(1, "float", {self.num})
	self.num = self.num + 1
end

function HelloCounter:in_2_float(f)
	self.num = f
end
