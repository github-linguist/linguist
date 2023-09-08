// ===========================================================================
// Rezilla_DITL.r
//                       Created: 2004-02-25 10:57:56
//             Last modification: 2004-02-25 10:58:34
// Author: Bernard Desgraupes
// e-mail: <bdesgraupes@users.sourceforge.net>
// www: <http://rezilla.sourceforge.net/>
// (c) Copyright: Bernard Desgraupes 2003-2004
// All rights reserved.
// ===========================================================================


//#include "Dialogs.r"
#include	<Carbon/Carbon.r>

resource 'DITL' (10000, "Nav Services Open", purgeable) {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{8, 16, 32, 248},
		CheckBox {
			enabled,
			"VCS aware"
		},
		/* [2] */
		{8, 248, 32, 392},
		CheckBox {
			enabled,
			"Read-only"
		}
	}
};

resource 'DITL' (10001, "Nav Create NewMap", purgeable) {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{3, 110, 19, 250},
		RadioButton {
			enabled,
			"in resource fork"
		},
		/* [2] */
		{3, 255, 19, 395},
		RadioButton {
			enabled,
			"in data fork"
		},
		/* [3] */
		{3, 8, 19, 108},
		StaticText {
			disabled,
			"New map"
		}
	}
};

resource 'DITL' (10002, "Nav Open Map", purgeable) {
	{	/* array DITLarray: 5 elements */
		/* [1] */
		{3, 58, 19, 138},
		RadioButton {
			enabled,
			"any fork"
		},
		/* [2] */
		{3, 158, 19, 278},
		RadioButton {
			enabled,
			"resource fork"
		},
		/* [3] */
		{3, 288, 19, 388},
		RadioButton {
			enabled,
			"data fork"
		},
		/* [4] */
		{23, 58, 39, 158},
		CheckBox {
			enabled,
			"Read Only"
		},
		/* [5] */
		{3, 8, 19, 58},
		StaticText {
			disabled,
			"From:"
		}
	}
};

resource 'DITL' (10003, "Nav Save Map", purgeable) {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{3, 58, 19, 158},
		RadioButton {
			enabled,
			"same fork"
		},
		/* [2] */
		{3, 168, 19, 288},
		RadioButton {
			enabled,
			"resource fork"
		},
		/* [3] */
		{3, 298, 19, 398},
		RadioButton {
			enabled,
			"data fork"
		},
		/* [4] */
		{3, 8, 19, 58},
		StaticText {
			disabled,
			"To:"
		}
	}
};

