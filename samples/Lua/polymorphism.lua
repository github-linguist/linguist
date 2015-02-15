-- Point
local Point = {x = 0, y = 0}

function Point:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Point:print()
    print("Point(" .. self.x .. ", " .. self.y .. ")")
end

function Point:copy()
    return Point:new{x = self.x, y = self.y}
end

-- Circle
local Circle = Point:new()
Circle.r = 0

function Circle:print()
    print("Circle(" .. self.x .. ", " .. self.y .. ", " .. self.r .. ")")
end

function Circle:copy()
    return Circle:new{x = self.x, y = self.y, r = self.r}
end
