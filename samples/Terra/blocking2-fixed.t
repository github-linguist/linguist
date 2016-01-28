C = terralib.includec("stdio.h")

terra min(a: int, b: int) : int
  if a < b then return a
  else return b end
end
std = terralib.includec("stdlib.h")
function Image(PixelType)
  local struct ImageImpl {
    data : &PixelType,
    N : int
  }
  terra ImageImpl:init(N: int): {} --returns nothing
    self.data = [&PixelType](
      std.malloc(N*N*sizeof(PixelType)))
    self.N = N
  end
  terra ImageImpl:get(x: int, y: int) : PixelType
    return self.data[x*self.N + y]
  end
  

  terra ImageImpl:set(x: int, y: int, v : PixelType)
    self.data[x*self.N + y] = v
  end
  terra ImageImpl:save(i : rawstring) 
    for i = 0, 2 do for j = 0, 2 do
      C.printf("%d %d %f\n", i,j,self:get(i,j))
    end end
  end
  terra ImageImpl:load(i : rawstring) self:init(16)
    for i = 0, 4 do for j = 0, 4 do
    	self:set(i,j,(i*4+j)%3)
    	--C.printf("%f\n",self:get(i,j))
    end end
  end
  terra ImageImpl:free() end

  --omitted methods for: set, save, load, free
  return ImageImpl
end
GreyscaleImage = Image(float)
terra laplace(img: &GreyscaleImage, 
              out: &GreyscaleImage) : {}
  --shrink result, do not calculate boundaries
  var newN = img.N - 2 
  out:init(newN)
  for i = 0,newN do
    for j = 0,newN do
      var v = img:get(i+0,j+1) + img:get(i+2,j+1)
            + img:get(i+1,j+2) + img:get(i+1,j+0)
            - 4 * img:get(i+1,j+1)
      out:set(i,j,v)
    end
  end
end
terra runlaplace(input: rawstring, 
                 output: rawstring) : {}
    var i: GreyscaleImage, o : GreyscaleImage
    i:load(input)
    laplace(&i,&o)
    o:save(output)
    i:free(); o:free()
end

runlaplace("myinput","myoutput")
 terralib.saveobj("runlaplace.o", 
                 {runlaplace = runlaplace})
