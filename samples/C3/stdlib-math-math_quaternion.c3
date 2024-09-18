// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_quaternion.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/math/math_quaternion.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::quaternion(<Real>);
import std::math::vector;
union Quaternion
{
	struct
	{
		Real i, j, k, l;
	}
	Real[<4>] v;
}

const Quaternion IDENTITY = { 0, 0, 0, 1 };

macro Quaternion Quaternion.add(Quaternion a, Quaternion b) => Quaternion { .v = a.v + b.v };
macro Quaternion Quaternion.add_each(Quaternion a, Real b) => Quaternion { .v = a.v + b };
macro Quaternion Quaternion.sub(Quaternion a, Quaternion b) => Quaternion { .v = a.v - b.v };
macro Quaternion Quaternion.sub_each(Quaternion a, Real b) => Quaternion { .v = a.v - b };
macro Quaternion Quaternion.scale(Quaternion a, Real s) => Quaternion { .v = a.v * s };
macro Quaternion Quaternion.normalize(Quaternion q) => { .v = q.v.normalize() };
macro Real Quaternion.length(Quaternion q) => q.v.length();
macro Quaternion Quaternion.lerp(Quaternion q1, Quaternion q2, Real amount) => { .v = q1.v.lerp(q2.v, amount) };
macro Matrix4f Quaternion.to_matrixf(Quaternion* q) => into_matrix(q, Matrix4f);
macro Matrix4 Quaternion.to_matrix(Quaternion* q) => into_matrix(q, Matrix4);
fn Quaternion Quaternion.nlerp(Quaternion q1, Quaternion q2, Real amount) => { .v = q1.v.lerp(q2.v, amount).normalize() };

fn Quaternion Quaternion.invert(q)
{
	Real length_sq = q.v.dot(q.v);
	if (length_sq <= 0) return q;
	Real inv_length = 1 / length_sq;
	return { q.v[0] * -inv_length, q.v[1] * -inv_length, q.v[2] * -inv_length, q.v[3] * inv_length };
}

fn Quaternion Quaternion.slerp(q1, Quaternion q2, Real amount)
{
	Quaternion result = {};

	Real[<4>] q2v = q2.v;
	Real cos_half_theta = q1.v.dot(q2v);

	if (cos_half_theta < 0)
	{
		q2v = -q2v;
		cos_half_theta = -cos_half_theta;
	}

	if (cos_half_theta >= 1) return q1;

	Real[<4>] q1v = q1.v;
	if (cos_half_theta > 0.95f) return { .v = q1v.lerp(q2v, amount) };

	Real half_theta = math::cos(cos_half_theta);
	Real sin_half_theta = math::sqrt(1 - cos_half_theta * cos_half_theta);
	if (math::abs(sin_half_theta) < 0.001f)
	{
		return { .v = (q1v + q2v) * 0.5f };
	}
	Real ratio_a = math::sin((1 - amount) * half_theta) / sin_half_theta;
	Real ratio_b = math::sin(amount * half_theta) / sin_half_theta;
	return { .v = q1v * ratio_a + q2v * ratio_b };
}

fn Quaternion Quaternion.mul(a, Quaternion b)
{
	return { a.i * b.l + a.l * b.i + a.j * b.k - a.k * b.j,
			 a.j * b.l + a.l * b.j + a.k * b.i - a.i * b.k,
			 a.k * b.l + a.l * b.k + a.i * b.j - a.j * b.i,
			 a.l * b.l - a.i * b.i - a.j * a.j - a.k * a.k };
}

macro into_matrix(Quaternion* q, $Type) @private
{
    Quaternion rotation = q.normalize();
    var x = rotation.i;
    var y = rotation.j;
    var z = rotation.k;
    var w = rotation.l;

    return $Type {
        1 - 2*y*y - 2*z*z, 2*x*y - 2*z*w, 2*x*z + 2*y*w, 0,
        2*x*y + 2*z*w, 1 - 2*x*x - 2*z*z, 2*y*z - 2*x*w, 0,
        2*x*z - 2*y*w, 2*y*z + 2*x*w , 1 - 2*x*x - 2*y*y, 0,
        0.0, 0.0, 0.0, 1.0,
    };
}