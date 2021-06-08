// Created by Crowbar 0.68

$modelname "maplabs\maplabber.mdl"

$staticprop

$bodygroup "Body"
{
	studio "maplabber.smd"
}

$lod 12
{
	replacemodel "maplabber.smd" "maplabber_lod.smd"
	nofacial
}

$surfaceprop "glassbottle"
$contents "solid"

$cdmaterials "models\maplabs\"

$texturegroup "skinfamilies"
{
	// vertex ones get glass-b (more transparent)
	// unlit  ones get glass   (more tranlucent)
	{ "labber-glass"   "labber-juice"   } // 0 lime unlit
	{ "labber-glass-b" "labber-juice-b" } // 1 lime vertex
	{ "labber-glass"   "labber-juice-c" } // 2 chartreuse unlit
	{ "labber-glass-b" "labber-juice-d" } // 3 chartreuse vertex
	{ "labber-glass"   "labber-juice-e" } // 4 white unlit
	{ "labber-glass-b" "labber-juice-f" } // 5 white vertex
	{ "labber-glass-b" "labber-juice-g" } // 6 gaypride vertex
}

$definebone "static_prop" "" 0 0 0 0 0 0 0 0 0 0 0 0

$sequence "idle" {
	"maplabber.smd"
	fadein 0.2
	fadeout 0.2
	fps 30
}

$collisionmodel "maplabber_hb.smd"
{
	$mass 1
	$inertia 1
	$damping 0
	$rotdamping 0
	$rootbone " "
	// $concave
	$maxconvexpieces 1

}

$collisiontext
{
	break 
	{
		"model"		"maplabs/maplabber-chunk-a"
		"fadetime"		"30"
	}
	break 
	{
		"model"		"maplabs/maplabber-chunk-b"
		"fadetime"		"30"
	}
	break 
	{
		"model"		"maplabs/maplabber-chunk-c"
		"fadetime"		"30"
	}
}

$keyvalues
{
	prop_data 
	{
		"base"		"Glass.Small"
	}
	
	// ### Removed effect: can be added via hammer, doesn't need to be in model
	//world_interactions
	//{
	//	"onbreak" "explode_fire"
	//}
	
	
	// ### Planned effect: a "slimesplosion"
	//particles {
	//	effect {
	//		name			"antlion_gib_02_slime"
	//		attachment_type		start_at_origin
	//	}
	//}
}
