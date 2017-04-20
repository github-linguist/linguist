// Script header for module 'KeyboardMovement'

#define KeyboardMovement_VERSION 101

enum KeyboardMovement_Modes {
	eKeyboardMovement_None, 
	eKeyboardMovement_Tapping, 
	eKeyboardMovement_Pressing
};

struct KeyboardMovement {
	import static function SetMode(KeyboardMovement_Modes mode);
};
