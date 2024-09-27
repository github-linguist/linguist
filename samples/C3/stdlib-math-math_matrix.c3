// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math_matrix.c3
// Permalink: https://github.com/c3lang/c3c/blob/2b0d289/lib/std/math/math_matrix.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::math::matrix(<Real>);
import std::math::vector;

struct Matrix2x2
{
	union
	{
		struct
		{
			Real m00, m01;
			Real m10, m11;
		}
		Real[4] m;
	}
}

struct Matrix3x3
{
	union
	{
		struct
		{
			Real m00, m01, m02;
			Real m10, m11, m12;
			Real m20, m21, m22;
		}
		Real[9] m;
	}
}

struct Matrix4x4
{
	union
	{
		struct
		{
			Real m00, m01, m02, m03;
			Real m10, m11, m12, m13;
			Real m20, m21, m22, m23;
			Real m30, m31, m32, m33;
		}
		Real[16] m;
	}
}

fn Real[<2>] Matrix2x2.apply(&self, Real[<2>] vec)
{
	return {
		self.m00 * vec[0] + self.m01 * vec[1],
		self.m10 * vec[0] + self.m11 * vec[1],
	};
}

fn Real[<3>] Matrix3x3.apply(&self, Real[<3>] vec)
{
	return {
		self.m00 * vec[0] + self.m01 * vec[1] + self.m02 * vec[2],
		self.m10 * vec[0] + self.m11 * vec[1] + self.m12 * vec[2],
		self.m20 * vec[0] + self.m21 * vec[1] + self.m22 * vec[2],
	};
}

fn Real[<4>] Matrix4x4.apply(&self, Real[<4>] vec)
{
	return {
		self.m00 * vec[0] + self.m01 * vec[1] + self.m02 * vec[2] + self.m03 * vec[3],
		self.m10 * vec[0] + self.m11 * vec[1] + self.m12 * vec[2] + self.m13 * vec[3],
		self.m20 * vec[0] + self.m21 * vec[1] + self.m22 * vec[2] + self.m23 * vec[3],
		self.m30 * vec[0] + self.m31 * vec[1] + self.m32 * vec[2] + self.m33 * vec[3],
	};
}


fn Matrix2x2 Matrix2x2.mul(&self, Matrix2x2 b)
{
	return {
		self.m00 * b.m00 + self.m01 * b.m10, self.m00 * b.m01 + self.m01 * b.m11,
		self.m10 * b.m00 + self.m11 * b.m10, self.m10 * b.m01 + self.m11 * b.m11,
	};
}

fn Matrix3x3 Matrix3x3.mul(&self, Matrix3x3 b)
{
	return {
		self.m00 * b.m00 + self.m01 * b.m10 + self.m02 * b.m20,
		self.m00 * b.m01 + self.m01 * b.m11 + self.m02 * b.m21,
		self.m00 * b.m02 + self.m01 * b.m12 + self.m02 * b.m22,

		self.m10 * b.m00 + self.m11 * b.m10 + self.m12 * b.m20,
		self.m10 * b.m01 + self.m11 * b.m11 + self.m12 * b.m21,
		self.m10 * b.m02 + self.m11 * b.m12 + self.m12 * b.m22,
			
		self.m20 * b.m00 + self.m21 * b.m10 + self.m22 * b.m20,
		self.m20 * b.m01 + self.m21 * b.m11 + self.m22 * b.m21,
		self.m20 * b.m02 + self.m21 * b.m12 + self.m22 * b.m22,
	};
}

fn Matrix4x4 Matrix4x4.mul(Matrix4x4* a, Matrix4x4 b)
{
	return {
		a.m00 * b.m00 + a.m01 * b.m10 + a.m02 * b.m20 + a.m03 * b.m30,
		a.m00 * b.m01 + a.m01 * b.m11 + a.m02 * b.m21 + a.m03 * b.m31,
		a.m00 * b.m02 + a.m01 * b.m12 + a.m02 * b.m22 + a.m03 * b.m32,
		a.m00 * b.m03 + a.m01 * b.m13 + a.m02 * b.m23 + a.m03 * b.m33,
			
		a.m10 * b.m00 + a.m11 * b.m10 + a.m12 * b.m20 + a.m13 * b.m30,
		a.m10 * b.m01 + a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31,
		a.m10 * b.m02 + a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32,
		a.m10 * b.m03 + a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33,
			
		a.m20 * b.m00 + a.m21 * b.m10 + a.m22 * b.m20 + a.m23 * b.m30,
		a.m20 * b.m01 + a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31,
		a.m20 * b.m02 + a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32,
		a.m20 * b.m03 + a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33,
			
		a.m30 * b.m00 + a.m31 * b.m10 + a.m32 * b.m20 + a.m33 * b.m30,
		a.m30 * b.m01 + a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31,
		a.m30 * b.m02 + a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32,
		a.m30 * b.m03 + a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33,
	};
}

fn Matrix2x2 Matrix2x2.component_mul(&self, Real s) => matrix_component_mul(self, s);
fn Matrix3x3 Matrix3x3.component_mul(&self, Real s) => matrix_component_mul(self, s);
fn Matrix4x4 Matrix4x4.component_mul(&self, Real s) => matrix_component_mul(self, s);

fn Matrix2x2 Matrix2x2.add(&self, Matrix2x2 mat2) => matrix_add(self, mat2);
fn Matrix3x3 Matrix3x3.add(&self, Matrix3x3 mat2) => matrix_add(self, mat2);
fn Matrix4x4 Matrix4x4.add(&self, Matrix4x4 mat2) => matrix_add(self, mat2);

fn Matrix2x2 Matrix2x2.sub(&self, Matrix2x2 mat2) => matrix_sub(self, mat2);
fn Matrix3x3 Matrix3x3.sub(&self, Matrix3x3 mat2) => matrix_sub(self, mat2);
fn Matrix4x4 Matrix4x4.sub(&self, Matrix4x4 mat2) => matrix_sub(self, mat2);

fn Matrix4x4 look_at(Real[<3>] eye, Real[<3>] target, Real[<3>] up) => matrix_look_at(Matrix4x4, eye, target, up);


fn Matrix2x2 Matrix2x2.transpose(&self)
{
	return {
		self.m00, self.m10,
		self.m01, self.m11
	};
}

fn Matrix3x3 Matrix3x3.transpose(&self)
{
	return {
		self.m00, self.m10, self.m20,
		self.m01, self.m11, self.m21,
		self.m02, self.m12, self.m22,
	};
}

fn Matrix4x4 Matrix4x4.transpose(&self)
{
	return {
		self.m00, self.m10, self.m20, self.m30,
		self.m01, self.m11, self.m21, self.m31,
		self.m02, self.m12, self.m22, self.m32,
		self.m03, self.m13, self.m23, self.m33,
	};
}


fn Real Matrix2x2.determinant(&self)
{
	return self.m00 * self.m11 - self.m01 * self.m10;
}

fn Real Matrix3x3.determinant(&self)
{
	return
		self.m00 * (self.m11 * self.m22 - self.m21 * self.m12) -
		self.m01 * (self.m10 * self.m22 - self.m20 * self.m12) +
		self.m02 * (self.m10 * self.m21 - self.m20 * self.m11);
}

fn Real Matrix4x4.determinant(&self)
{
	return
		self.m00 * (self.m11 * (self.m22 * self.m33 - self.m32 * self.m23) -
				   self.m12 * (self.m21 * self.m33 - self.m31 * self.m23) +
				   self.m13 * (self.m21 * self.m32 - self.m31 * self.m22) ) -
		self.m01 * (self.m10 * (self.m22 * self.m33 - self.m32 * self.m23) -
				   self.m12 * (self.m20 * self.m33 - self.m30 * self.m23) +
				   self.m13 * (self.m20 * self.m32 - self.m30 * self.m22) ) +
		self.m02 * (self.m10 * (self.m21 * self.m33 - self.m31 * self.m23) -
				   self.m11 * (self.m20 * self.m33 - self.m30 * self.m23) +
				   self.m13 * (self.m20 * self.m31 - self.m30 * self.m21) ) -
		self.m03 * (self.m10 * (self.m21 * self.m32 - self.m31 * self.m22) -
				   self.m11 * (self.m20 * self.m32 - self.m30 * self.m22) +
				   self.m12 * (self.m20 * self.m31 - self.m30 * self.m21) );
}


fn Matrix2x2 Matrix2x2.adjoint(&self)
{
	return { self.m00, -self.m01, -self.m10, self.m11 };
}

fn Matrix3x3 Matrix3x3.adjoint(&self)
{
	return {
		(self.m11 * self.m22 - self.m21 * self.m12),
		-(self.m10 * self.m22 - self.m20 * self.m12),
		(self.m10 * self.m21 - self.m20 * self.m11),
			
		-(self.m01 * self.m22 - self.m21 * self.m02),
		(self.m00 * self.m22 - self.m20 * self.m02),
		-(self.m00 * self.m21 - self.m20 * self.m01),
			
		(self.m01 * self.m12 - self.m11 * self.m02),
		-(self.m00 * self.m12 - self.m10 * self.m02),
		(self.m00 * self.m11 - self.m10 * self.m01),
	};
}

fn Matrix4x4 Matrix4x4.adjoint(&self)
{
	return {
		(self.m11 * (self.m22 * self.m33 - self.m32 * self.m23) -
		 self.m12 * (self.m21 * self.m33 - self.m31 * self.m23) +
		 self.m13 * (self.m21 * self.m32 - self.m31 * self.m22)),
		-(self.m10 * (self.m22 * self.m33 - self.m32 * self.m23) -
		  self.m12 * (self.m20 * self.m33 - self.m30 * self.m23) +
		  self.m13 * (self.m20 * self.m32 - self.m30 * self.m22)),
		(self.m10 * (self.m21 * self.m33 - self.m31 * self.m23) -
		 self.m11 * (self.m20 * self.m33 - self.m30 * self.m23) +
		 self.m13 * (self.m20 * self.m31 - self.m30 * self.m21)),
		-(self.m10 * (self.m21 * self.m32 - self.m31 * self.m22) -
		  self.m11 * (self.m20 * self.m32 - self.m30 * self.m22) +
		  self.m12 * (self.m20 * self.m31 - self.m30 * self.m21)),
			
		-(self.m01 * (self.m22 * self.m33 - self.m32 * self.m23) -
		  self.m02 * (self.m21 * self.m33 - self.m31 * self.m23) +
		  self.m03 * (self.m21 * self.m32 - self.m31 * self.m22)),
		(self.m00 * (self.m22 * self.m33 - self.m32 * self.m23) -
		 self.m02 * (self.m20 * self.m33 - self.m30 * self.m23) +
		 self.m03 * (self.m20 * self.m32 - self.m30 * self.m22)),
		-(self.m00 * (self.m21 * self.m33 - self.m31 * self.m23) -
		  self.m01 * (self.m20 * self.m33 - self.m30 * self.m23) +
		  self.m03 * (self.m20 * self.m31 - self.m30 * self.m21)),
		(self.m00 * (self.m21 * self.m32 - self.m31 * self.m22) -
		 self.m01 * (self.m20 * self.m32 - self.m30 * self.m22) +
		 self.m02 * (self.m20 * self.m31 - self.m30 * self.m21)),
			
		(self.m01 * (self.m12 * self.m33 - self.m32 * self.m13) -
		 self.m02 * (self.m11 * self.m33 - self.m31 * self.m13) +
		 self.m03 * (self.m11 * self.m32 - self.m31 * self.m12)),
		-(self.m00 * (self.m12 * self.m33 - self.m32 * self.m13) -
		  self.m02 * (self.m10 * self.m33 - self.m30 * self.m13) +
		  self.m03 * (self.m10 * self.m32 - self.m30 * self.m12)),
		(self.m00 * (self.m11 * self.m33 - self.m31 * self.m13) -
		 self.m01 * (self.m10 * self.m33 - self.m30 * self.m13) +
		 self.m03 * (self.m10 * self.m31 - self.m30 * self.m11)),
		-(self.m00 * (self.m11 * self.m32 - self.m31 * self.m12) -
		  self.m01 * (self.m10 * self.m32 - self.m30 * self.m12) +
		  self.m02 * (self.m10 * self.m31 - self.m30 * self.m11)),
			
		-(self.m01 * (self.m12 * self.m23 - self.m22 * self.m13) -
		  self.m02 * (self.m11 * self.m23 - self.m21 * self.m13) +
		  self.m03 * (self.m11 * self.m22 - self.m21 * self.m12)),
		(self.m00 * (self.m12 * self.m23 - self.m22 * self.m13) -
		 self.m02 * (self.m10 * self.m23 - self.m20 * self.m13) +
		 self.m03 * (self.m10 * self.m22 - self.m20 * self.m12)),
		-(self.m00 * (self.m11 * self.m23 - self.m21 * self.m13) -
		  self.m01 * (self.m10 * self.m23 - self.m20 * self.m13) +
		  self.m03 * (self.m10 * self.m21 - self.m20 * self.m11)),
		(self.m00 * (self.m11 * self.m22 - self.m21 * self.m12) -
		 self.m01 * (self.m10 * self.m22 - self.m20 * self.m12) +
		 self.m02 * (self.m10 * self.m21 - self.m20 * self.m11)),
	};
}


fn Matrix2x2! Matrix2x2.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST?;
	Matrix2x2 adj = self.adjoint();
	return adj.component_mul(1 / det).transpose();
}

fn Matrix3x3! Matrix3x3.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST?;
	Matrix3x3 adj = self.adjoint();
	return adj.component_mul(1 / det).transpose();
}

fn Matrix4x4! Matrix4x4.inverse(&self)
{
	Real det = self.determinant();
	if (det == 0) return MatrixError.MATRIX_INVERSE_DOESNT_EXIST?;
	Matrix4x4 adj = self.adjoint();
	return adj.component_mul(1 / det).transpose();
}


fn Matrix3x3 Matrix3x3.translate(&self, Real[<2>] v)
{
	return self.mul({
		1, 0, v[0],
		0, 1, v[1],
		0, 0, 1,
	});
}

fn Matrix4x4 Matrix4x4.translate(&self, Real[<3>] v)
{
	return self.mul({
		1, 0, 0, v[0],
		0, 1, 0, v[1],
		0, 0, 1, v[2],
		0, 0, 0, 1,
	});
}

// r in radians
fn Matrix3x3 Matrix3x3.rotate(&self, Real r)
{
	return self.mul({
		math::cos(r), -math::sin(r), 0,
		math::sin(r), math::cos(r), 0,
		0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_z(&self, Real r)
{
	return self.mul({
		math::cos(r), -math::sin(r), 0, 0,
		math::sin(r), math::cos(r), 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_y(&self, Real r)
{
	return self.mul({
		math::cos(r), 0, -math::sin(r), 0,
		0, 1, 0, 0,
		math::sin(r), 0, math::cos(r), 0,
		0, 0, 0, 1,
	});
}

// r in radians
fn Matrix4x4 Matrix4x4.rotate_x(&self, Real r)
{
	return self.mul({
		1, 0, 0, 0,
		0, math::cos(r), -math::sin(r), 0,
		0, math::sin(r), math::cos(r), 0,
		0, 0, 0, 1,
	});
}


fn Matrix3x3 Matrix3x3.scale(&self, Real[<2>] v)
{
	return self.mul({
		v[0], 0, 0,
		0, v[1], 0,
		0, 0, 1,
	});
}

fn Real Matrix2x2.trace(&self) => self.m00 + self.m11;
fn Real Matrix3x3.trace(&self) => self.m00 + self.m11 + self.m22;
fn Real Matrix4x4.trace(&self) => self.m00 + self.m11 + self.m22 + self.m33;

fn Matrix4x4 Matrix4x4.scale(&self, Real[<3>] v)
{
	return self.mul({
		v[0], 0, 0, 0,
		0, v[1], 0, 0,
		0, 0, v[2], 0,
		0, 0, 0, 1,
	});
}

fn Matrix4x4 ortho(Real left, Real right, Real top, Real bottom, Real near, Real far)
{
	Real width = right - left;
	Real height = top - bottom;
	Real depth = far - near;
	return {
		2 / width, 0, 0, 0,
		0, 2 / height, 0, 0,
		0, 0, -2 / depth, 0,
		-(right + left) / width, -(top + bottom) / height, -(far + near) / depth, 1
	};
}

// fov in radians
fn Matrix4x4 perspective(Real fov, Real aspect_ratio, Real near, Real far)
{
	Real f = (Real)math::tan(math::PI * 0.5 - 0.5 * fov);
	Real range_inv = (Real)1.0 / (near - far);

	return {
		f / aspect_ratio, 0, 0, 0,
		0, f, 0, 0,
		0, 0, (near + far) * range_inv,  near * far * range_inv * 2,
		0, 0, -1, 0,
	};
}

const Matrix2x2 IDENTITY2 = { .m = { [0] = 1, [3] = 1 } };
const Matrix3x3 IDENTITY3 = { .m = { [0] = 1, [4] = 1, [8] = 1 } };
const Matrix4x4 IDENTITY4 = { .m = { [0] = 1, [5] = 1, [10] = 1, [15] = 1 } };

macro matrix_component_mul(mat, val) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return $typeof(*mat) { .m = val * ($Type)mat.m };
}

macro matrix_add(mat, mat2) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return $typeof(*mat) { .m = ($Type)mat.m + ($Type)mat2.m };
}

macro matrix_sub(mat, mat2) @private
{
	var $Type = Real[<$typeof(mat.m).len>];
	return $typeof(*mat) { .m = ($Type)mat.m - ($Type)mat2.m };
}

macro matrix_look_at($Type, eye, target, up) @private
{
	var vz = (eye - target).normalize();
	var vx = up.cross(vz).normalize();
	var vy = vz.cross(vx);

	return $Type {
		vx[0], vx[1], vx[2], - (Real)vx.dot(eye),
		vy[0], vy[1], vy[2], - (Real)vy.dot(eye),
		vz[0], vz[1], vz[2], - (Real)vz.dot(eye),
		0.0, 0.0, 0.0, 1
	};
}
