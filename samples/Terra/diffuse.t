
local C = terralib.includecstring [[
#include<stdio.h>
#include<stdlib.h>
#ifndef _WIN32
#include <sys/time.h>
static double CurrentTimeInSeconds() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec + tv.tv_usec / 1000000.0;
}
#else
#include <time.h>
static double CurrentTimeInSeconds() {
	return time(NULL);
}
#endif

int CalcTime(int * times, double * start) {
	if(*times == 0) {
		*start = CurrentTimeInSeconds();
	} else {
		double elapsed = CurrentTimeInSeconds() - *start;
		if(elapsed > 0.1f && *times >= 1) {
			*start = elapsed / *times;
			return 0;
		}
	}
	(*times)++;
	return 1;
}
]]

alignedloads = global(0)
loads = global(0)
NI,NJ = 1,1
V = 4
local VP = &vector(float,V)
terra uload(d : &float)
	--if d:as(uint64) % 16 == 0 then
	--	alignedloads = alignedloads + 1
	--end
	--loads = loads + 1
	return terralib.attrload(VP(d),{ align = 4 })
end
terra ustore(d : &float, v : vector(float,V))
	terralib.attrstore(VP(d),v, { align = 4 })
end

terra diffuse_reference(output : &float, N : int, M : int, stride : int, x : &float, x0 : &float, a : float)
	var invD = 1.f / (1 + 4.f*a)
	var nextline = stride - N
	for i = 0, M do
		for j = 0,N do
			var r = 
			   (x0[0]
			+  (x[ - stride]
			+  x[stride]
			+  x[ - 1]
			+  x[1] )*a) * invD
			output[0] = r
			x0 = x0 + 1
			x = x + 1
			output = output + 1
		end
		x0 = x0 + nextline --- stride
		x = x + nextline   --- stride
		output = output + nextline --- stride
	end

end

terra diffuse(output : &float, N : int, M : int, stride : int, x : &float, x0 : &float, a : float)
	var invD = 1.f / (1 + 4.f*a)
	var nextline = stride - N
	for i = 0, M do
		
		for j = 0,N,V do
			var r = 
			   (uload(x0)
			+  (uload(x - stride)
			+  uload(x + stride)
			+  uload(x - 1)
			+  uload(x + 1) )*a) * invD
			--C.printf("%d %d\n",i,j)
			--llvmprefetch(x + 4 * stride,0,3,1)
			--llvmprefetch(x0 + 3 * stride,0,3,1)
			
			ustore(output,r)
			x0 = x0 + V
			x = x + V
			output = output + V
		end
		
		x0 = x0 + nextline --- stride
		x = x + nextline   --- stride
		output = output + nextline --- stride
	end

end

llvmprefetch = terralib.intrinsic("llvm.prefetch",{&uint8,int,int,int} -> {})

terra diffuse2(output : &float, N : int, M : int, stride : int, x : &float, x0 : &float, a : float,xi : &float)
	var invD = 1.f / (1 + 4.f*a)
	var nextline = stride - N
	var xi0,xi1,xi2 = xi - stride, xi, xi + stride
	var x02 = x0
	for i = 0, M + 1 do

		if i < M then
			for j = 0,N,V do
				var r = 
				   (uload(x0)
				+  (uload(x - stride)
				+  uload(x + stride)
				+  uload(x - 1)
				+  uload(x + 1) )*a) * invD
				--C.printf("%d %d\n",i,j)
				--llvmprefetch(x + 2 * stride,0,3,1)
				ustore(xi,r)
				x0 = x0 + V
				x = x + V
				xi = xi + V
			end
			
			x0 = x0 + nextline --- stride
			x = x + nextline   --- stride
			xi = xi + nextline --- stride
			if i % 3 == 1 then
				xi = xi - stride*3
			end
		end

		if i >= 1 then
			for j = 0,N,V do
				var r = 
				   (uload(x02)
				+  (uload(xi0)
				+  uload(xi2)
				+  uload(xi1 - 1)
				+  uload(xi1 + 1) )*a) * invD
				--C.printf("%d %d\n",i,j)
				ustore(output,r)
				x02 = x02 + V
				xi0 = xi0 + V
				xi1 = xi1 + V
				xi2 = xi2 + V
				output = output + V
			end
			
			x02 = x02 + nextline --- stride
			xi0,xi1,xi2 = xi1 - N,xi2 - N, xi0 - N
			output = output + nextline --- stride
		end
	end

end


terra doit(thea : float)
	var enlarge = 1
	var N = 1024/8
	var M = 1024*8
	var N1 = (N + 2*enlarge)
	var M1 = (M + 2*enlarge)

	var NA = N1
	var x = [&float](C.malloc(NA*M1*sizeof(float)))
	var x0 = [&float](C.malloc(NA*M1*sizeof(float)))

	var ro1 = [&float](C.malloc(NA*M1*sizeof(float)))
	var ro2 = [&float](C.malloc(NA*M1*sizeof(float)))

	var to1 = [&float](C.malloc(NA*M1*sizeof(float)))
	var to2 = [&float](C.malloc(NA*M1*sizeof(float)))

	
	for i = 0, N1 do
		for j = 0, N1 do
			x[i*N1+j] = C.rand() % 10
			x0[i*N1+j] = C.rand() % 10
			ro1[i*N1+j] = -42
			ro2[i*N1+j] = -43
			to1[i*N1+j] = -44
			to2[i*N1+j] = -45
		end
	end
	var stride = N1
	var start = enlarge*stride + enlarge

	diffuse_reference(ro1 + start,N,M,stride,x + start,x0 + start,thea)
	diffuse_reference(ro2 + start,N,M,stride,ro1 + start,x0 + start,thea)


	
	var times  = 0
	var mytime : double
	
	var fuse = true
	while C.CalcTime(&times,&mytime) ~= 0 do
		if not fuse then
			diffuse(to1 + start,N,M,stride,x + start,x0 + start,thea)
			diffuse(to2 + start,N,M,stride,to1 + start,x0 + start,thea)
		else
			diffuse2(to2 + start,N,M,stride,x + start,x0 + start,thea,to1 + start)
		end
	end

	C.printf("times = %d\n",times)
	for i = enlarge+1, M1-enlarge-1 do
		for j = enlarge+1,N1-enlarge-1 do
			if ro2[i*stride+j] ~= to2[i*stride+j] then
				C.printf("wrong! %d %d %f %f\n",i,j,ro2[i*stride+j],to2[i*stride+j])
				goto out
			else
				--C.printf("right! %d %d %f %f\n",i,j,ro1[i*N1+j],to1[i*N1+j])
			end
		end
	end
	::out::
	var togiga = 1.0/(1024*1024*1024)
	var pixels = N*M
	C.printf("%f %f %f\n", mytime, 2*pixels*4*3 *togiga / mytime, 2*6*pixels * togiga / mytime)

end

terra getloads()
	return alignedloads,loads
end

doit(1)
diffuse:disas()
doit(1)

local al,loads = terralib.unpackstruct(getloads())
print(al/loads)