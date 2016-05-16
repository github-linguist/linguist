/*
tap_tubewarmth - tap_tubewarmth - A port of Tom Szilagyi's TAP TubeWarmth LADSPA plugin to a Csound User-Defined Opcode

DESCRIPTION
This is a port of Tom Szilagyi's TAP TubeWarmth LADSPA plugin to a Csound User-Defined Opcode.  

From Tom Szilagyi's description:

"TAP TubeWarmth adds the character of vacuum tube amplification to your audio tracks by emulating the sonically desirable nonlinear characteristics of triodes. In addition, this plugin also supports emulating analog tape saturation."

More information about the original plugin here:

http://tap-plugins.sourceforge.net/ladspa/tubewarmth.html

SYNTAX
aout tap_tubewarmth ain, kdrive, kblend

PERFORMANCE
ain - audio input signal

kdrive - Values between 2 and 5 are a good starting point for a variety of source materials. Since audio tracks can vary quite a bit in average and peak levels, experiment with this setting and use your ears to get the sound you want. (It's quite easy if you know how real tube amps sound like...) If the drive level is set too high, the signal will most likely sound distorted. If it's too low, you may not hear the effect working. 

kblend - controls the colour of the TubeWarmth sound. When set all the way to the right (+10 or default position), the plugin emulates the sound of triode tube distortion. The result is asymmetrical, producing mostly second harmonics and some third. When set all the way to the left (-10), the plugin emulates the sound of analog tape. The result is symmetrical and produces mostly third harmonics and some second. With high drive settings, moving the blend control to the left increases the apparent loudness of low-level signals dramatically. This is because the zero-attack, zero-release compression effect is increased under these conditions. Use the blend control to set the sound of the plugin anywhere between Tape and Tube sound.

(The above descriptions of drive and blend were taken from Tom Szilagyi's Description of his plugin)

CREDITS
Original LADSPA Code by Tom Szilagyi, Csound UDO code by Steven Yi (2006.08.31)
*/

	opcode tap_tubewarmth,a,akk

setksmps 1

ain, kdrive, kblend xin

kdrive	 	limit kdrive, 0.1, 10
kblend 		limit kblend, -10, 10

kprevdrive init 0
kprevblend init 0

krdrive 	init 0
krbdr 		init 0
kkpa 		init 0
kkpb 		init 0
kkna 		init 0
kknb 		init 0
kap 		init 0
kan 		init 0
kimr 		init 0
kkc 		init 0
ksrct 		init 0
ksq 		init 0
kpwrq 		init 0

#define TAP_EPS # 0.000000001 # 
#define TAP_M(X) # $X = (($X > $TAP_EPS || $X < -$TAP_EPS) ? $X : 0) #
#define TAP_D(A) # 
if ($A > $TAP_EPS) then
	$A = sqrt($A)
elseif ($A < $TAP_EPS) then
	$A = sqrt(-$A)
else
	$A = 0
endif
#

if (kprevdrive != kdrive || kprevblend != kblend) then

krdrive = 12.0 / kdrive;
krbdr = krdrive / (10.5 - kblend) * 780.0 / 33.0;

kkpa = 2.0 * (krdrive*krdrive) - 1.0
$TAP_D(kkpa)
kkpa = kkpa + 1.0;

kkpb = (2.0 - kkpa) / 2.0;
kap = ((krdrive*krdrive) - kkpa + 1.0) / 2.0;

kkc = 2.0 * (krdrive*krdrive) - 1.0
$TAP_D(kkc)
kkc = 2.0 * kkc - 2.0 * krdrive * krdrive
$TAP_D(kkc)

kkc = kkpa / kkc

ksrct = (0.1 * sr) / (0.1 * sr + 1.0);
ksq = kkc*kkc + 1.0

kknb = ksq
$TAP_D(kknb)
kknb = -1.0 * krbdr / kknb

kkna = ksq
$TAP_D(kkna)
kkna = 2.0 * kkc * krbdr / kkna

kan = krbdr*krbdr / ksq

kimr = 2.0 * kkna + 4.0 * kan - 1.0
$TAP_D(kimr)
kimr = 2.0 * kknb + kimr


kpwrq = 2.0 / (kimr + 1.0)

kprevdrive = kdrive
kprevblend = kblend

endif

aprevmed 	init 0
amed 		init 0
aprevout	init 0

kin downsamp ain

if (kin >= 0.0) then
	kmed = kap + kin * (kkpa - kin)
	$TAP_D(kmed)
	amed = (kmed + kkpb) * kpwrq
else
	kmed = kap - kin * (kkpa + kin)
	$TAP_D(kmed)
	amed = (kmed + kkpb) * kpwrq * -1
endif

aout = ksrct * (amed - aprevmed + aprevout)

kout downsamp aout
kmed downsamp amed


if (kout < -1.0) then
	aout = -1.0
	kout = -1.0
endif

$TAP_M(kout)
$TAP_M(kmed)

aprevmed = kmed
aprevout = kout

#undef TAP_D
#undef TAP_M
#undef TAP_EPS

xout aout

	endop
 
 
