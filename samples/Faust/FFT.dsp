// Radix 2 FFT, decimation in time, real and imag parts interleaved

declare name	"FFT"; // Faust Fourier Transform :-)
declare author	"JOS";
declare license "STK-4.3";

import("stdfaust.lib");

N=32; // FFT size (power of 2)
// Number of frequency bins (including dc and SR/2) is N/2+1

No2 = N>>1;
signal = amp * cosine with {
  cosine = select2(k==0,
		 select2(k==No2,
			2.0*os.oscrc(f(k)), // 2x since negative-frequencies not displayed
			1-1':+~*(-1) // Alternating sequence: 1, -1, 1, -1
			),
		   1.0); // make sure phase is zero (freq jumps around)
  f(k) = float(k) * ma.SR / float(N); // only test FFT bin frequencies
  k = hslider("[2] FFT Bin Number",N/4,0,No2,0.001) : int <: _,dpy : attach;
  dpy = hbargraph("[3] Measured FFT Bin Number",0,No2);
  amp = hslider("[4] Amplitude",0.1,0,1,0.001);
};

process = signal : dm.fft_spectral_level_demo(N) <: _,_;
