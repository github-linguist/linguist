Points :=[{x:  5.0, y: 5.0}
		, {x:  5.0, y: 8.0}
		, {x:-10.0, y: 5.0}
		, {x:  0.0, y: 5.0}
		, {x: 10.0, y: 5.0}
		, {x:  8.0, y: 5.0}
		, {x: 10.0, y:10.0}]
Square :=[{x: 0.0, y: 0.0}, {x:10.0, y: 0.0}
		, {x:10.0, y: 0.0}, {x:10.0, y:10.0}
		, {x:10.0, y:10.0}, {x: 0.0, y:10.0}
		, {x: 0.0, y:10.0}, {x: 0.0, y: 0.0}]
Sq_Hole:=[{x: 0.0, y: 0.0}, {x:10.0, y: 0.0}
		, {x:10.0, y: 0.0}, {x:10.0, y:10.0}
		, {x:10.0, y:10.0}, {x: 0.0, y:10.0}
		, {x: 0.0, y:10.0}, {x: 0.0, y: 0.0}
		, {x: 2.5, y: 2.5}, {x: 7.5, y: 2.5}
		, {x: 7.5, y: 2.5}, {x: 7.5, y: 7.5}
		, {x: 7.5, y: 7.5}, {x: 2.5, y: 7.5}
		, {x: 2.5, y: 7.5}, {x: 2.5, y: 2.5}]
Strange:=[{x: 0.0, y: 0.0}, {x: 2.5, y: 2.5}
		, {x: 2.5, y: 2.5}, {x: 0.0, y:10.0}
		, {x: 0.0, y:10.0}, {x: 2.5, y: 7.5}
		, {x: 2.5, y: 7.5}, {x: 7.5, y: 7.5}
		, {x: 7.5, y: 7.5}, {x:10.0, y:10.0}
		, {x:10.0, y:10.0}, {x:10.0, y: 0.0}
		, {x:10.0, y: 0.0}, {x: 2.5, y: 2.5}]
Exagon :=[{x: 3.0, y: 0.0}, {x: 7.0, y: 0.0}
		, {x: 7.0, y: 0.0}, {x:10.0, y: 5.0}
		, {x:10.0, y: 5.0}, {x: 7.0, y:10.0}
		, {x: 7.0, y:10.0}, {x: 3.0, y:10.0}
		, {x: 3.0, y:10.0}, {x: 0.0, y: 5.0}
		, {x: 0.0, y: 5.0}, {x: 3.0, y: 0.0}]
Polygons := {"Square":Square, "Sq_Hole":Sq_Hole, "Strange":Strange, "Exagon":Exagon}
For j, Poly in Polygons
	For i, Point in Points
		If (point_in_polygon(Point,Poly))
			s.= j " does contain point " i "`n"
		Else
			s.= j " doesn't contain point " i "`n"
Msgbox %s%

point_in_polygon(Point,Poly) {
	n:=Poly.MaxIndex()
	count:=0
	loop, %n% {
		if (ray_intersects_segment(Point,Poly[A_Index],Poly[mod(A_Index,n)+1])) {
			count++
		}
	}
	if (mod(count,2)) { ; true = inside, false = outside
		return true		; P is in the polygon
	} else {
		return false	; P isn't in the polygon
	}
}

ray_intersects_segment(P,A,B) {
	;P = the point from which the ray starts
	;A = the end-point of the segment with the smallest y coordinate
	;B = the end-point of the segment with the greatest y coordinate
	if (A.y > B.y) {
		temp:=A
		A:=B
		B:=temp
	}
	if (P.y = A.y or P.y = B.y) {
		P.y += 0.000001
	}
	if (P.y < A.y or P.y > B.y) {
		return false
	} else if (P.x > A.x && P.x > B.x) {
		return false
	} else {
		if (P.x < A.x && P.x < B.x) {
			return true
		} else {
			if (A.x != B.x) {
				m_red := (B.y - A.y)/(B.x - A.x)
			} else {
				m_red := "inf"
			}
			if (A.x != P.x) {
				m_blue := (P.y - A.y)/(P.x - A.x)
			} else {
				m_blue := "inf"
			}
			if (m_blue >= m_red) {
				return true
			} else {
				return false
			}
		}
	}
}
