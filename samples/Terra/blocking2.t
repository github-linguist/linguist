terra min(a : int, b : int)
	return terralib.select(a < b, a, b)
end
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

IO = terralib.includec("stdio.h")
stdlib = terralib.includec("stdlib.h")

terra main()
	
	var a : int[8][8]
	var c = 0
	var N = 8
	[blockedloop(N, {4,2,1}, function(i,j)
		return quote 
			--IO.printf("%d %d\n",i,j)
			a[i][j] = c
			c = c + 1
		end
	end)]
	for i = 0,N do
		for j = 0,N do
			IO.printf("%d\t",a[i][j])
		end
		IO.printf("\n")
	end
end

main()



function Image(Spectrum)
	local struct ImageImpl {
		data : &Spectrum,
		N : int
	}
	terra ImageImpl:init(N : int)
		self.data = [&float](stdlib.malloc(N*N*sizeof(Spectrum)))
		self.N = N
	end
	ImageImpl.methods.pixel = macro(function(self,x,y)
		return `self.data[x*self.N + y]
	end)
	return ImageImpl
end

GreyScaleImage = Image(float)

terra laplace(input : &GreyScaleImage, output : &GreyScaleImage)
	var newN = input.N - 2 --shrink result since we do not calculate boundaries
	output:init(newN);
	[blockedloop(newN,{32,1},function(i,j)
		return quote
			output:pixel(i,j) =
              input:pixel(i+0,j+1)
            + input:pixel(i+2,j+1)
            + input:pixel(i+1,j+2)
            + input:pixel(i+1,j+0)
            - 4 * input:pixel(i+1,j+1)
		end
	end)]
end

laplace:compile()
laplace:printpretty()