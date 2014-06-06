/*
===========================================================================
The Open Game Libraries.
Copyright (C) 2007-2010 Lusito Software

Author:  Santo Pfingsten (TTK-Bandit)
Purpose: Math namespace
-----------------------------------------

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.
===========================================================================
*/

#ifndef __OG_MATH_INL__
#define __OG_MATH_INL__

namespace og {

/*
==============================================================================

  Math

==============================================================================
*/

/*
================
Math::Abs
================
*/
OG_INLINE int Math::Abs( int i ) {
#if 1
	if ( i & 0x80000000 )
		return 0x80000000 - (i & MASK_SIGNED);
	return i;
#else
   int y = x >> 31;
   return ( ( x ^ y ) - y );
#endif
}

/*
================
Math::Fabs
================
*/
OG_INLINE float Math::Fabs( float f ) {
#if 1
	uInt *pf = reinterpret_cast<uInt*>(&f);
	*(pf) &= MASK_SIGNED;
	return f;
#else
	return fabsf( f );
#endif
}

/*
================
Math::Round
================
*/
OG_INLINE float Math::Round( float f ) {
	return floorf( f + 0.5f );
}

/*
================
Math::Floor
================
*/
OG_INLINE float Math::Floor( float f ) {
	return floorf( f );
}

/*
================
Math::Ceil
================
*/
OG_INLINE float Math::Ceil( float f ) {
	return ceilf( f );
}

/*
================
Math::Ftoi

ok since this is SSE, why should the other ftoi be the faster one ?
and: we might need to add a check for SSE extensions..
because sse isn't *really* faster (I actually read that GCC does not handle
SSE extensions perfectly. I'll find the link and send it to you when you're online)
================
*/
OG_INLINE int Math::Ftoi( float f ) {
	//! @todo	needs testing
	// note: sse function cvttss2si
#if OG_ASM_MSVC
	int i;
#if defined(OG_FTOI_USE_SSE)
	if( SysInfo::cpu.general.SSE ) {
		__asm cvttss2si	eax, f
		__asm mov		i, eax
		return i;
	} else
#endif
	{
		__asm fld		f
		__asm fistp		i
		//__asm mov eax, i // do we need this ? O_o
	}
	return i;
#elif OG_ASM_GNU
	int i;
#if defined(OG_FTOI_USE_SSE)
	if( SysInfo::cpu.general.SSE ) {
		__asm__ __volatile__( "cvttss2si %1    \n\t"
			: "=m" (i)
			: "m" (f)
		);
	} else
#endif
	{
		__asm__ __volatile__( "flds %1    \n\t"
							  "fistpl %0  \n\t"
			: "=m" (i)
			: "m" (f)
		);
	}
	return i;
#else
	// we use c++ cast instead of c cast (not sure why id did that)
	return static_cast<int>(f);
#endif
}

/*
================
Math::FtoiFast
================
*/
OG_INLINE int Math::FtoiFast( float f ) {
#if OG_ASM_MSVC
	int i;
	__asm fld		f
	__asm fistp		i
	//__asm mov eax, i // do we need this ? O_o
	return i;
#elif OG_ASM_GNU
	int i;
	__asm__ __volatile__( "flds %1    \n\t"
						  "fistpl %0  \n\t"
		: "=m" (i)
		: "m" (f)
	);
	return i;
#else
	// we use c++ cast instead of c cast (not sure why id did that)
	return static_cast<int>(f);
#endif
}

/*
================
Math::Ftol
================
*/
OG_INLINE long Math::Ftol( float f ) {
#if OG_ASM_MSVC
	long i;
	__asm fld		f
	__asm fistp		i
	//__asm mov eax, i // do we need this ? O_o
	return i;
#elif OG_ASM_GNU
	long i;
	__asm__ __volatile__( "flds %1    \n\t"
						  "fistpl %0  \n\t"
		: "=m" (i)
		: "m" (f)
	);
	return i;
#else
	// we use c++ cast instead of c cast (not sure why id did that)
	return static_cast<long>(f);
#endif
}

/*
================
Math::Sign
================
*/
OG_INLINE float Math::Sign( float f )	{
	if ( f > 0.0f )
		return 1.0f;
	if ( f < 0.0f )
		return -1.0f;
	return 0.0f;
}

/*
================
Math::Fmod
================
*/
OG_INLINE float Math::Fmod( float numerator, float denominator ) {
	return fmodf( numerator, denominator );
}

/*
================
Math::Modf
================
*/
OG_INLINE float Math::Modf( float f, float& i ) {
	return modff( f, &i );
}
OG_INLINE float Math::Modf( float f ) {
	float i;
	return modff( f, &i );
}

/*
================
Math::Sqrt
================
*/
OG_INLINE float Math::Sqrt( float f ) {
	return sqrtf( f );
}

/*
================
Math::InvSqrt

Cannot be 0.0f
================
*/
OG_INLINE float Math::InvSqrt( float f ) {
	OG_ASSERT( f != 0.0f );
	return 1.0f / sqrtf( f );
}

/*
================
Math::RSqrt

Can be 0.0f
================
*/
OG_INLINE float Math::RSqrt( float f ) {
	float g = 0.5f * f;
	int i = *reinterpret_cast<int *>(&f);

	// do a guess
	i = 0x5f375a86 - ( i>>1 );
	f = *reinterpret_cast<float *>(&i);

	// Newtons calculation
	f = f * ( 1.5f - g * f * f );
	return f;
}

/*
================
Math::Log/Log2/Log10

Log of 0 is bad.
I've also heard you're not really
supposed to do log of negatives, yet
they work fine.
================
*/
OG_INLINE float Math::Log( float f ) {
	OG_ASSERT( f != 0.0f );
	return logf( f );
}
OG_INLINE float Math::Log2( float f ) {
	OG_ASSERT( f != 0.0f );
	return INV_LN_2 * logf( f );
}
OG_INLINE float Math::Log10( float f ) {
	OG_ASSERT( f != 0.0f );
	return INV_LN_10 * logf( f );
}

/*
================
Math::Pow
================
*/
OG_INLINE float Math::Pow( float base, float exp ) {
	return powf( base, exp );
}

/*
================
Math::Exp
================
*/
OG_INLINE float Math::Exp( float f ) {
	return expf( f );
}

/*
================
Math::IsPowerOfTwo
================
*/
OG_INLINE bool Math::IsPowerOfTwo( int x ) {
	// This is the faster of the two known methods
	// with the x > 0 check moved to the beginning
	return x > 0 && ( x & ( x - 1 ) ) == 0;
}

/*
================
Math::HigherPowerOfTwo
================
*/
OG_INLINE int Math::HigherPowerOfTwo( int x ) {
	x--;
	x |= x >> 1;
	x |= x >> 2;
	x |= x >> 4;
	x |= x >> 8;
	x |= x >> 16;
	return x + 1;
}

/*
================
Math::LowerPowerOfTwo
================
*/
OG_INLINE int Math::LowerPowerOfTwo( int x ) {
	return HigherPowerOfTwo( x ) >> 1;
}

/*
================
Math::FloorPowerOfTwo
================
*/
OG_INLINE int Math::FloorPowerOfTwo( int x ) {
	return IsPowerOfTwo( x ) ? x : LowerPowerOfTwo( x );
}

/*
================
Math::CeilPowerOfTwo
================
*/
OG_INLINE int Math::CeilPowerOfTwo( int x ) {
	return IsPowerOfTwo( x ) ? x : HigherPowerOfTwo( x );
}

/*
================
Math::ClosestPowerOfTwo
================
*/
OG_INLINE int Math::ClosestPowerOfTwo( int x ) {
	if ( IsPowerOfTwo( x ) )
		return x;
	int high = HigherPowerOfTwo( x );
	int low = high >> 1;
	return ((high-x) < (x-low)) ? high : low;
}

/*
================
Math::Digits
================
*/
OG_INLINE int Math::Digits( int x ) {
	int digits = 1;
	int step = 10;
	while (step <= x) {
		digits++;
		step *= 10;
	}
	return digits;
}

/*
================
Math::Sin/ASin
================
*/
OG_INLINE float Math::Sin( float f ) {
	return sinf( f );
}
OG_INLINE float Math::ASin( float f ) {
	if ( f <= -1.0f )
		return -HALF_PI;
	if ( f >= 1.0f )
		return HALF_PI;
	return asinf( f );
}

/*
================
Math::Cos/ACos
================
*/
OG_INLINE float Math::Cos( float f ) {
	return cosf( f );
}
OG_INLINE float Math::ACos( float f ) {
	if ( f <= -1.0f )
		return PI;
	if ( f >= 1.0f )
		return 0.0f;
	return acosf( f );
}

/*
================
Math::Tan/ATan
================
*/
OG_INLINE float Math::Tan( float f ) {
	return tanf( f );
}
OG_INLINE float Math::ATan( float f ) {
	return atanf( f );
}
OG_INLINE float Math::ATan( float f1, float f2 ) {
	return atan2f( f1, f2 );
}

/*
================
Math::SinCos
================
*/
OG_INLINE void Math::SinCos( float f, float &s, float &c ) {
#if OG_ASM_MSVC
	// sometimes assembler is just waaayy faster
	_asm {
		fld		f
		fsincos
		mov		ecx, c
		mov		edx, s
		fstp	dword ptr [ecx]
		fstp	dword ptr [edx]
	}
#elif OG_ASM_GNU
	asm ("fsincos" : "=t" (c), "=u" (s) : "0" (f));
#else
	s = Sin(f);
	c = Sqrt( 1.0f - s * s ); // faster than calling Cos(f)
#endif
}

/*
================
Math::Deg2Rad
================
*/
OG_INLINE float Math::Deg2Rad( float f ) {
	return f * DEG_TO_RAD;
}

/*
================
Math::Rad2Deg
================
*/
OG_INLINE float Math::Rad2Deg( float f ) {
	return f * RAD_TO_DEG;
}

/*
================
Math::Square
================
*/
OG_INLINE float Math::Square( float v ) {
	return v * v;
}

/*
================
Math::Cube
================
*/
OG_INLINE float Math::Cube( float v ) {
	return v * v * v;
}

/*
================
Math::Sec2Ms
================
*/
OG_INLINE int Math::Sec2Ms( int sec ) {
	return sec * 1000;
}

/*
================
Math::Ms2Sec
================
*/
OG_INLINE int Math::Ms2Sec( int ms ) {
	return FtoiFast( ms * 0.001f );
}

}

#endif
