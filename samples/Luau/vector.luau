--// Vector
--// Written by Demo (R0BL0XIAN_D3M0)
--// [https://www.roblox.com/users/289025524/profile]
--// 11/09/2023

--// Types
type TVector = Vector2 | Vector3

--[=[
	@class Vector

	A collection of very useful vector-related functions.
]=]
local Vector = {}

--// Functions

--[=[
	@within Vector

	@param vector Vector2 -- The `Vector2` coordinate.

	@return Vector3 -- Return the newly created `Vector3`.

	Create a `Vector3` from a `Vector2` within the XY plane.
]=]
function Vector.FromVector2XY(vector: Vector2): Vector3
	return (Vector3.new(vector.X, vector.Y, 0))
end

--[=[
	@within Vector

	@param vector Vector2 -- The `Vector2` coordinate.

	@return Vector3 -- Return the newly created `Vector3`.

	Create a `Vector3` from a `Vector2` within the XZ plane, unlike `FromVector2XY`.
]=]
function Vector.FromVector2XZ(vector: Vector2): Vector3
	return (Vector3.new(vector.X, 0, vector.Y))
end

--[=[
	@within Vector

	@param vector Vector2 -- The initial `Vector2` coordinate.

	@param _vector Vector2 -- The secondary `Vector2` coordinate.

	@return number -- Return the computed angle in a numerical form or nil.

	Compute the angle between two vectors in radians.
]=]
function Vector.RetrieveAngleRadian(vector: Vector2, _vector: Vector2): number?
	if vector.Magnitude == 0 then
		return nil
	end

	return (math.acos(vector:Dot(_vector)))
end

--[=[
	@within Vector

	@param vector Vector2 -- The initial `Vector2` coordinate.

	@param _vector Vector2 -- The secondary `Vector2` coordinate.

	@return number -- Return the computed angle in a numerical form.

	Compute the angle between two vectors.
]=]
function Vector.AngleBetweenVectors(vector: Vector2, _vector: Vector2): number
	local newVector: Vector2 = (_vector.Magnitude * vector)
	local _newVector: Vector2 = (vector.Magnitude * _vector)

	return (2 * (math.atan2((_newVector - newVector).Magnitude, (newVector + _newVector).Magnitude)))
end

--[=[
	@within Vector

	@param vector Vector3 -- The original `Vector3` coordinate.

	@param amount number -- The primary amount.

	@return Vector3 -- Return the rounded `Vector3`.

	Round the specified `Vector3` to the nearest number.
]=]
function Vector.Round(vector: Vector3, amount: number): Vector3
	return (
		Vector3.new(
			((math.round(vector.X / amount)) * amount),
			((math.round(vector.Y / amount)) * amount),
			((math.round(vector.Z / amount)) * amount)
		)
	)
end

--[=[
	@within Vector

	@param vector TVector -- The vector coordinate (`Vector2` or `Vector3`).

	@param maxMagnitude number -- The maximum magnitude.

	@return number -- Return the clamped magnitude.

	Clamp the magnitude of a vector so it is only a certain length.
]=]
function Vector.ClampMagnitude(vector: TVector, maxMagnitude: number): number
	return ((vector.Magnitude > maxMagnitude) and (vector.Unit * maxMagnitude) or vector)
end

--[=[
	@within Vector

	@param vector TVector -- The initial vector coordinate (`Vector2` or `Vector3`).

	@param _vector TVector -- The secondary vector coordinate (`Vector2` or `Vector3`).

	@return number -- Return the radianed angle.

	Finds the angle in radians between two vectors.
]=]
function Vector.AngleBetween(vector: TVector, _vector: TVector): number
	return (math.acos(math.clamp(vector.Unit:Dot(_vector.Unit), -1, 1)))
end

--[=[
	@within Vector

	@param vector TVector -- The initial vector coordinate (`Vector2` or `Vector3`).

	@param _vector TVector -- The secondary vector coordinate (`Vector2` or `Vector3`).

	@param axisVector Vector -- The axis vector coordinate (`Vector` or `Vector3`).

	@return number -- Return the radianed angle.

	Finds the angle in radians between two vectors and returns a signed value.
]=]
function Vector.AngleBetweenSigned(vector: TVector, _vector: TVector, axisVector: TVector): number
	local angle: number = Vector.AngleBetween(vector, _vector)

	return (angle * (math.sign(axisVector:Dot(vector:Cross(_vector)))))
end

--[=[
	@within Vector

	@return Vector3 -- Return the random `Vector3` coordinate.

	Return a random unit vector (could be used for equal distribution around a sphere).
]=]
function Vector.RetrieveRandomUnitVector(): Vector3
	local randomX: number = (2 * ((math.random()) - 0.5))
	local randomY: number = (6.2831853071796 * (math.random()))
	local randomZ: number = ((1 - (randomX * randomX)) ^ 0.5)

	local X: number = randomX
	local Y: number = (randomZ * (math.cos(randomZ)))
	local Z: number = (randomZ * (math.sin(randomY)))

	return (Vector3.new(X, Y, Z))
end

return Vector