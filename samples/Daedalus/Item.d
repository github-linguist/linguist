INSTANCE ITAR_KDF_M (C_Item)
{
	name 					=	"Lekka szata ognia";

	mainflag 				=	ITEM_KAT_ARMOR;
	flags 					=	0;

	protection [PROT_EDGE]	=	100;
	protection [PROT_BLUNT] = 	100;
	protection [PROT_POINT] = 	100;
	protection [PROT_FIRE] 	= 	50;
	protection [PROT_MAGIC] = 	50;

	value 					=	VALUE_ITAR_KDF_H;

	wear 					=	WEAR_TORSO;

	visual 					=	"ItAr_KdF_L.3ds";
	visual_change 			=	"Hum_KdfL_Armor.asc";
	visual_skin 			=	0;
	material 				=	MAT_LEATHER;

	on_equip				=	Equip_ITAR_KDF_H;
	on_unequip				=	UnEquip_ITAR_KDF_H;

	description				=	name;

	TEXT[1]					=	NAME_Prot_Edge;
	COUNT[1]				=	 protection	[PROT_EDGE];

	TEXT[2]					=	NAME_Prot_Point;
	COUNT[2]				= 	protection	[PROT_POINT];

	TEXT[3] 				=	NAME_Prot_Fire;
	COUNT[3]				= 	protection	[PROT_FIRE];

	TEXT[4]					=	NAME_Prot_Magic;
	COUNT[4]				= 	protection	[PROT_MAGIC];

	TEXT[5]					=	NAME_Value;
	COUNT[5]				= 	value;
};
