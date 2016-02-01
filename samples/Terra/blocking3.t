C = terralib.includec("stdio.h")

function blockedloop(N,blocksizes,bodyfn)
  local function generatelevel(n,ii,jj,bb)
    if n > #blocksizes then
      return bodyfn(ii,jj)
    end
    local blocksize = blocksizes[n]
    return quote
      for i = ii,min(ii+bb,N),blocksize do
        for j = jj,min(jj+bb,N),blocksize do
          [ generatelevel(n+1,i,j,blocksize) ]
        end
      end
    end
  end
  return generatelevel(1,0,0,N)
end

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
    self.data = 
      [&PixelType](std.malloc(N*N*sizeof(PixelType)))
    self.N = N
  end
  terra ImageImpl:get(x: int, y: int) : PixelType
    return self.data[x*self.N + y]
  end
  

  terra ImageImpl:set(x: int, y: int, v : PixelType)
    self.data[x*self.N + y] = v
  end
  terra ImageImpl:save(i : rawstring) 
    for i = 0, 8 do for j = 0, 8 do
      C.printf("%d %d %f\n", i,j,self:get(i,j))
    end end
  end
  terra ImageImpl:load(i : rawstring) self:init(16)
    for i = 0, 10 do for j = 0, 10 do
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
  out:init(newN);
    [blockedloop(newN,{4,2,1}, function(i,j)
    return quote
      var v = img:get(i+0,j+1) + img:get(i+2,j+1)
            + img:get(i+1,j+2) + img:get(i+1,j+0)
            - 4 * img:get(i+1,j+1)
      out:set(i,j,v)
    end
  end)]
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
