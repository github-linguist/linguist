default {
    state_entry() {
        llSetPrimitiveParams([PRIM_NAME, "RosettaCode DeathStar"]);
        llSetPrimitiveParams([PRIM_DESC, llGetObjectName()]);
        llSetPrimitiveParams([PRIM_TYPE, PRIM_TYPE_SPHERE, PRIM_HOLE_CIRCLE, <0.0, 1.0, 0.0>, 0.0, <0.0, 0.0, 0.0>, <0.12, 1.0, 0.0>]);
        llSetPrimitiveParams([PRIM_ROTATION, <-0.586217, 0.395411, -0.586217, 0.395411>]);
        llSetPrimitiveParams([PRIM_TEXTURE, ALL_SIDES, TEXTURE_BLANK, ZERO_VECTOR, ZERO_VECTOR, 0.0]);
        llSetPrimitiveParams([PRIM_TEXT, llGetObjectName(), <1.0, 1.0, 1.0>, 1.0]);
        llSetPrimitiveParams([PRIM_COLOR, ALL_SIDES, <0.5, 0.5, 0.5>, 1.0]);
        llSetPrimitiveParams([PRIM_BUMP_SHINY, ALL_SIDES, PRIM_SHINY_HIGH, PRIM_BUMP_NONE]);
        llSetPrimitiveParams([PRIM_SIZE, <10.0, 10.0, 10.0>]);
        llSetPrimitiveParams([PRIM_OMEGA, <0.0, 0.0, 1.0>, 1.0, 1.0]);
    }
}
