fn CalculateVolumeAndCentreOfMass obj =
(
	local Volume= 0.0
	local Centre= [0.0, 0.0, 0.0]
	local theMesh = snapshotasmesh obj
	local numFaces = theMesh.numfaces
	for i = 1 to numFaces do
	(
		local Face= getFace theMesh i
		local vert2 = getVert theMesh Face.z
		local vert1 = getVert theMesh Face.y
		local vert0 = getVert theMesh Face.x
		local dV = Dot (Cross (vert1 - vert0) (vert2 - vert0)) vert0
		Volume+= dV
		Centre+= (vert0 + vert1 + vert2) * dV
	)
	delete theMesh
	Volume /= 6
	Centre /= 24
	Centre /= Volume
	#(Volume,Centre)
)
