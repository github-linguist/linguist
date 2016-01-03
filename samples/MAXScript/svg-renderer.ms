-- Taken from a 3-part tutorial from Autodesk's MAXScript reference
-- Source: http://help.autodesk.com/view/3DSMAX/2016/ENU/?guid=__files_GUID_6B5EDC11_A154_4AA7_A972_A11AC36949E9_htm

fn ColourToHex col = (
	local theComponents = #(bit.intAsHex col.r, bit.intAsHex col.g, bit.intAsHex col.b)
	local theValue = "#"
	for i in theComponents do 
		theValue += (if i.count == 1 then "0" else "") + i
	theValue
)

local st = timestamp()
local theFileName = (getDir #userscripts + "\\PolygonRendering3.svg")
local theSVGfile = createFile theFileName
format "<svg xmlns=\"http://www.w3.org/2000/svg\"\n" to:theSVGfile
format "\t\txmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" to:theSVGfile

local theViewTM =  viewport.getTM()
theViewTM.row4 = [0,0,0]

local theViewTM2 = viewport.getTM()
local theViewSize = getViewSize()
local theViewScale = getViewSize()
theViewScale.x /= 1024.0
theViewScale.y /= 1024.0
	
local theStrokeThickness = 3
	
gw.setTransform (matrix3 1)	
for o in Geometry where not o.isHiddenInVpt and classof o != TargetObject do (
	local theStrokeColour = white
	local theFillColour = o.wirecolor
	
	local theMesh = snapshotAsMesh o
	for f = 1 to theMesh.numfaces do (
		local theNormal = normalize (getFaceNormal theMesh f)
		
		if (theNormal*theViewTM).z > 0 do
		(
			local theFace = getFace theMesh f
			local v1 = gw.transPoint (getVert theMesh theFace.x)
			local v2 = gw.transPoint (getVert theMesh theFace.y)
			local v3 = gw.transPoint (getVert theMesh theFace.z)
			
			v1.x /= theViewScale.x 
			v1.y /= theViewScale.y 
			v2.x /= theViewScale.x 
			v2.y /= theViewScale.y
			v3.x /= theViewScale.x
			v3.y /= theViewScale.y
			
			format "\t<polygon points='%,%  %,%  %,%' \n" v1.x v1.y v2.x v2.y v3.x v3.y to:theSVGfile
			format "\tstyle='stroke:%; fill:%; stroke-width:%'/>\n" (ColourToHex theStrokeColour) (ColourToHex theFillColour) theStrokeThickness to:theSVGfile			
		)--end if normal positive
	)--end f loop
)--end o loop

format "</svg>\n" to:theSVGfile
close theSVGfile
local theSVGMap = VectorMap vectorFile:theFileName alphasource:0
local theBitmap = bitmap theViewSize.x theViewSize.y
renderMap theSVGMap into:theBitmap filter:true
display theBitmap
format "Render Time: % sec.\n" ((timestamp()-st)/1000.0)
