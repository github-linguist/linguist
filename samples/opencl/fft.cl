double run_fftw(int n,const float * x,float * y)
{
  fftwf_plan p1 = fftwf_plan_dft_1d(n,(fftwf_complex *)x,(fftwf_complex *)y,
                      FFTW_FORWARD,FFTW_ESTIMATE);
  const int nops = 10;
  double t = cl::realTime();
  for (int op = 0;op < nops;op++) {
    fftwf_execute(p1);
  }
  t = (cl::realTime() - t)/(double)nops;
  fftwf_destroy_plan(p1);
  return t;
}
