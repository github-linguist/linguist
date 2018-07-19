#!/usr/bin/env lua
-- allow dictionary file and sample size to be specified on command line
local dictfile = arg[1] or "unixdict.txt"
local sample_size = arg[2] or 5;

-- read dictionary
local f = assert(io.open(dictfile, "r"))
local dict = {}
for line in f:lines() do
  dict[line] = line:reverse()
end
f:close()

-- find the semordnilaps
local semordnilaps = {}
for fwd, rev in pairs(dict) do
  if dict[rev] and fwd < rev then
    table.insert(semordnilaps, {fwd,rev})
  end
end

-- print the report
print("There are " .. #semordnilaps .. " semordnilaps in " .. dictfile .. ".  Here are " .. sample_size .. ":")

math.randomseed( os.time() )
for i = 1, sample_size do
  local j
  repeat
    j = math.random(1,#semordnilaps)
  until semordnilaps[j]
  local f, r = unpack(semordnilaps[j])
  semordnilaps[j] = nil
  print(f .. " -> " .. r)
end
