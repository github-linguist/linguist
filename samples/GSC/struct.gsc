
function init() {}
function delete() {}

/@
"Name: get( <kvp_value> , [kvp_key] )"
"Summary: Returns a struct with the specified kvp."
"MandatoryArg: <kvp_value> : kvp value"
"OptionalArg: [kvp_key] : defaults to targetname"
"Example: struct::get( "some_value", "targetname" );"
"SPMP: both"
@/
function get( kvp_value, kvp_key = "targetname" ){}

/@
"Name: spawn( [v_origin], [v_angles] )"
"Summary: Returns a new struct."
"OptionalArg: [v_origin] : optional origin"
"OptionalArg: [v_angles] : optional angles"
"Example: s = struct::spawn( self GetTagOrigin( "tag_origin" ) );"
@/
function spawn( v_origin = (0, 0, 0), v_angles = (0, 0, 0) ){}

/@
"Name: get_array( <kvp_value> , [kvp_key] )"
"Summary: Returns an array of structs with the specified kvp."
"MandatoryArg: <kvp_value> : kvp value"
"OptionalArg: [kvp_key] : defaults to targetname"
"Example: fxemitters = struct::get_array( "streetlights", "targetname" )"
"SPMP: both"
@/
function get_array( kvp_value, kvp_key = "targetname" ){}

/@
"Name: get_script_bundle( <str_type>, <str_name> )"
"Summary: Returns a struct with the specified script bundle definition. This is the GDT data for the bundle."
"MandatoryArg: <str_type> : The type of the script bundle"
"MandatoryArg: <str_name> : The name of the script bundle"
"Example: struct::get_script_bundle( "scene", "my_scene" );"
"SPMP: both"
@/
function get_script_bundle( str_type, str_name ){}

/@
"Name: delete_script_bundle( <str_type>, <str_name> )"
"Summary: Deletes the specified script bundle definition. This is the GDT data for the bundle."
"MandatoryArg: <str_type> : The type of the script bundle"
"MandatoryArg: <str_name> : The name of the script bundle"
"Example: struct::delete_script_bundle( "scene", "my_scene" );"
"SPMP: both"
@/
function delete_script_bundle( str_type, str_name ){}

/@
"Name: get_script_bundles( <str_type> )"
"Summary: Returns all of the script bundle definition structs for the specified type."
"MandatoryArg: <str_type> : The type of the script bundle"
"Example: struct::get_script_bundles( "scene" );"
"SPMP: both"
@/
function get_script_bundles( str_type ){}

/@
"Name: get_script_bundle_list( <str_type>, <str_name> )"
"Summary: Returns a string array with the items specified by the script bundle list."
"MandatoryArg: <str_type> : The type of the script bundle in the list"
"MandatoryArg: <str_name> : The name of the script bundle list"
"Example: struct::get_script_bundle_list( "collectible", "completecollectibleslist" );"
"SPMP: both"
@/
function get_script_bundle_list( str_type, str_name ){}

/@
"Name: get_script_bundle_instances( <str_type>, [str_name] )"
"Summary: Returns an array of all the script bundle instances with the specified script bundle definition and name."
"MandatoryArg: <str_type> : The type of the script bundle"
"MandatoryArg: [str_name] : The name of the script bundle"
"Example: struct::get_script_bundle_instances( "scene", "my_scene" );"
"SPMP: both"
@/
function get_script_bundle_instances( str_type, str_name = "" ){}

