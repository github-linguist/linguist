function setmetatables(t,mts) --takes a table and a list of metatables
  return setmetatable(t,{__index = function(self, k)
    --collisions are resolved in this implementation by simply taking the first one that comes along.
    for i, mt in ipairs(mts) do
      local member = mt[k]
      if member then return member end
    end
  end})
end

camera = {}
mobilephone = {}
cameraphone = setemetatables({},{camera,mobilephone})
