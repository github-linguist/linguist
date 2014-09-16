#include <version.hqf>

#define SET(VAR,VALUE) private #VAR; VAR = VALUE;
#define CONV(VAR,ARRAY,POOL) VAR = ARRAY select (POOL find VAR);

#define ALL_HITPOINTS_MAN [ \
  "HitHead", "HitBody", \
  "HitLeftArm", "HitRightArm", \
  "HitLeftLeg","HitRightLeg" \
]

#define ALL_HITPOINTS_VEH [ \
  "HitBody", "HitHull", "HitEngine", "HitFuel", \
  "HitTurret", "HitGun", \
  "HitLTrack", "HitRTrack", \
  "HitLFWheel", "HitRFWheel", "HitLF2Wheel", "HitRF2Wheel", "HitLMWheel", "HitRMWheel", "HitLBWheel", "HitRBWheel", \
  "HitAvionics", "HitHRotor", "HitVRotor", \
  "HitRGlass", "HitLGlass", "HitGlass1", "HitGlass2", "HitGlass3", "HitGlass4", "HitGlass5", "HitGlass6" \
]
