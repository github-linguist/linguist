# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2012-2013 Alexis Laferrière <alexis.laf@xymus.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This module illustrates some uses of the FFI, specifically
# how to use extern methods. Which means to implement a Nit method in C.
module extern_methods

redef enum Int
	# Returns self'th fibonnaci number
	# implemented here in C for optimization purposes
	fun fib : Int import fib `{
		if ( recv < 2 )
			return recv;
		else
			return Int_fib( recv-1 ) + Int_fib( recv-2 );
	`}

	# System call to sleep for "self" seconds
	fun sleep `{
		sleep( recv );
	`}

	# Return atan2l( self, x ) from libmath
	fun atan_with( x : Int ) : Float `{
		return atan2( recv, x );
	`}

	# This method callback to Nit methods from C code
	# It will use from C code:
	# * the local fib method
	# * the + operator, a method of Int
	# * to_s, a method of all objects
	# * String.to_cstring, a method of String to return an equivalent char*
	fun foo import fib, +, to_s, String.to_cstring `{
		long recv_fib = Int_fib( recv );
		long recv_plus_fib = Int__plus( recv, recv_fib );

		String nit_string = Int_to_s( recv_plus_fib );
		char *c_string = String_to_cstring( nit_string );

		printf( "from C: self + fib(self) = %s\n", c_string );
	`}

	# Equivalent to foo but written in pure Nit
	fun bar do print "from Nit: self + fib(self) = {self+self.fib}"
end

print 12.fib

print "sleeping 1 second..."
1.sleep

print 100.atan_with( 200 )
8.foo
8.bar

