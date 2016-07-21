-- Taken from an example from Autodesk's MAXScript reference:
-- http://help.autodesk.com/view/3DSMAX/2016/ENU/?guid=__files_GUID_84E24969_C175_4389_B9A6_3B2699B66785_htm

macroscript MoveToSurface
    category: "HowTo"
(
    fn g_filter o = superclassof o == Geometryclass
    fn find_intersection z_node node_to_z = (
        local testRay = ray node_to_z.pos [0,0,-1]
        local nodeMaxZ = z_node.max.z
        testRay.pos.z = nodeMaxZ + 0.0001 * abs nodeMaxZ
        intersectRay z_node testRay
    )
    
    on isEnabled return selection.count > 0
    
    on Execute do (
        target_mesh = pickObject message:"Pick Target Surface:" filter:g_filter
        
        if isValidNode target_mesh then (
            undo "MoveToSurface" on (
                for i in selection do (
                    int_point = find_intersection target_mesh i
                    if int_point != undefined then i.pos = int_point.pos
                )--end i loop
            )--end undo
        )--end if
    )--end execute
)--end script
