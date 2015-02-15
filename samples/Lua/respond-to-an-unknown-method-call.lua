local object={print=print}
setmetatable(object,{__index=function(t,k)return function() print("You called the method",k)end end})
object.print("Hi") -->Hi
object.hello() -->You called the method hello
