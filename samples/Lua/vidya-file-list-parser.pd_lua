
local FileListParser = pd.Class:new():register("vidya-file-list-parser")

function FileListParser:initialize(sel, atoms)

	-- 1. Base filename
	-- 2. File extension
	-- 3. Number of files in batch
	self.inlets = 3
	
	-- 1. To [list trim]-[binfile]
	-- 2. To [vidya-file-modder]'s filename variables
	-- 3. Sends a bang to [vidya-file-modder], triggering the object's mechanisms
	self.outlets = 3
	
	-- File extension
	self.extension = "jpg"
	
	-- Number of the last file in the batch
	self.batchlimit = 0
	
	return true

end

function FileListParser:in_1_symbol(s)

	for i = 0, self.batchlimit do
		self:outlet(2, "list", {s, i})
		self:outlet(1, "read", {s .. i .. "." .. self.extension})
		self:outlet(1, "info", {})
		self:outlet(3, "bang", {})
	end

end

function FileListParser:in_2_list(d)
	self.extension = d[1]
end

function FileListParser:in_3_float(f)
	self.batchlimit = f
end