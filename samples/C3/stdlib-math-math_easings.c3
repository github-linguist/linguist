// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_easings.c3
// Permalink: https://github.com/c3lang/c3c/blob/499c82b/lib/std/math/math_easings.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::easing;

/* Easing equations ported from Robert Penner's equations */

/*
 *
 * TERMS OF USE - EASING EQUATIONS
 *
 * Open source under the BSD License.
 *
 * Copyright © 2001 Robert Penner
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this list of
 * conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, this list
 * of conditions and the following disclaimer in the documentation and/or other materials
 * provided with the distribution.
 *
 * Neither the name of the author nor the names of contributors may be used to endorse
 * or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 *  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

// Linear Easing functions
fn float linear_none(float t, float b, float c, float d) @inline => c * t / d + b;
fn float linear_in(float t, float b, float c, float d) @inline => c * t / d + b;
fn float linear_out(float t, float b, float c, float d) @inline => c * t / d + b;
fn float linear_inout(float t, float b, float c, float d) @inline => c * t / d + b;

// Sine Easing functions
fn float sine_in(float t, float b, float c, float d) @inline => -c * math::cos(t / d * (float)math::PI_2) + c + b;
fn float sine_out(float t, float b, float c, float d) @inline => c * math::sin(t / d * (float)math::PI_2) + b;
fn float sine_inout(float t, float b, float c, float d) @inline => (-c / 2) * (math::cos((float)math::PI * t / d) - 1) + b;

// Circular Easing functions
fn float circ_in(float t, float b, float c, float d) @inline => -c * (math::sqrt(1 - sq(t / d)) - 1) + b;
fn float circ_out(float t, float b, float c, float d) @inline => c * math::sqrt(1 - sq(t / d - 1)) + b;
fn float circ_inout(float t, float b, float c, float d) @inline
{
	t /= d / 2;
	return t < 1
		? (-c / 2) * (math::sqrt(1 - sq(t)) - 1) + b
		: (c / 2) * (math::sqrt(1 - sq(t - 2)) + 1) + b;
}

// Cubic Easing functions
fn float cubic_in(float t, float b, float c, float d) @inline => c * cube(t / d) + b;
fn float cubic_out(float t, float b, float c, float d) @inline =>  c * (cube(t / d - 1) + 1) + b;
fn float cubic_inout(float t, float b, float c, float d) @inline
{
	t /= d / 2;
	return t < 1
		? (c / 2) * cube(t) + b
		: c / 2 * (cube(t - 2) + 2) + b;
}

// Quadratic Easing functions
fn float quad_in(float t, float b, float c, float d) @inline => c * sq(t / d) + b;
fn float quad_out(float t, float b, float c, float d) @inline
{
	t /= d;
	return -c * t * (t - 2) + b;
}

fn float quad_inout(float t, float b, float c, float d) @inline
{
	t /= d / 2;
	return t < 1
		? (c / 2) * sq(t) + b
		: (-c / 2) * ((t - 1) * (t - 3) - 1) + b;
}

// Exponential Easing functions
fn float expo_in(float t, float b, float c, float d) @inline => t ? b : c * math::pow(2.0f, 10 * (t / d - 1)) + b;
fn float expo_out(float t, float b, float c, float d) @inline
{
	return (t == d) ? b + c : c * (-math::pow(2.0f, -10 * t / d) + 1) + b;
}
fn float expo_inout(float t, float b, float c, float d) @inline                                                                        // Ease: Exponential In Out
{
	if (t == 0) return b;
	if (t == d) return b + c;
	t /= d / 2;
	return t < 1
		? (c / 2) * math::pow(2.0f, 10 * (t - 1)) + b
		: (c / 2) * (-math::pow(2.0f, -10 * (t - 1)) + 2) + b;
}

// Back Easing functions
fn float back_in(float t, float b, float c, float d, float s = 1.70158f) @inline
{
	t /= d;
	return c * sq(t) * ((s + 1) * t - s) + b;
}

fn float back_out(float t, float b, float c, float d, float s = 1.70158f) @inline
{
	t = t / d - 1;
	return c * (sq(t) * ((s + 1) * t + s) + 1) + b;
}

fn float back_inout(float t, float b, float c, float d, float s = 1.70158f) @inline
{
	s *= 1.525f;
	t /= d / 2;
	if (t < 1)
	{
		return (c / 2) * sq(t) * ((s + 1) * t - s) + b;
	}
	t -= 2.0f;
	return (c / 2) * (sq(t) * ((s + 1) * t + s) + 2) + b;
}

// Bounce Easing functions
fn float bounce_out(float t, float b, float c, float d) @inline
{
	t /= d;
	switch
	{
		case t < 1 / 2.75f:
			return c * 7.5625f * sq(t) + b;
		case t < 2 / 2.75f:
			t -= 1.5f / 2.75f;
			return c * (7.5625f * sq(t) + 0.75f) + b;
		case t < 2.5f / 2.75f:
			t -= 2.25f / 2.75f;
			return c * (7.5625f * sq(t) + 0.9375f) + b;
		default:
			t -= 2.625f / 2.75f;
			return c * (7.5625f * sq(t) + 0.984375f) + b;
	}
}

fn float bounce_in(float t, float b, float c, float d) @inline => c - bounce_out(d - t, 0, c, d) + b;
fn float bounce_inout(float t, float b, float c, float d) @inline
{
	return t < d / 2
		? bounce_in(t * 2, 0, c, d) * 0.5f + b
		: bounce_out(t * 2 - d, 0, c, d) * 0.5f + b;
}

// Elastic Easing functions
fn float elastic_in(float t, float b, float c, float d) @inline
{
	if (t == 0) return b;
	t /= d;
	if (t == 1) return b + c;

	float p = d * 0.3f;
	float a = c;
	float s = p / 4;
	t -= 1;
	return -a * math::pow(2.0f, 10 * t) * math::sin((t * d - s) * (2 * (float)math::PI) / p) + b;
}

fn float elastic_out(float t, float b, float c, float d) @inline
{
	if (t == 0) return b;
	t /= d;
	if (t == 1) return b + c;

	float p = d * 0.3f;
	float a = c;
	float s = p / 4;

	return a * math::pow(2.0f, -10 * t) * math::sin((t * d - s) * (2 * (float)math::PI) / p) + c + b;
}

fn float elastic_inout(float t, float b, float c, float d) @inline
{
	if (t == 0) return b;
	t /= d / 2;
	if (t == 2) return b + c;

	float p = d * (0.3f * 1.5f);
	float a = c;
	float s = p / 4;

	t -= 1;
	return t < 0
		? -0.5f * a * math::pow(2.0f, 10 * t) * math::sin((t * d - s) * (2 * (float)math::PI)/p) + b
		:  a * math::pow(2.0f, -10 * t) * math::sin((t * d - s) * (2 * (float)math::PI) / p) * 0.5f + c + b;
}

macro sq(x) @private => x * x;
macro cube(x) @private => x * x * x;

