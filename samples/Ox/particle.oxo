nldge::ParticleLogLikeli()
{	decl it, ip,
		 mss, mbas, ms, my, mx, vw, vwi, dws,
		 mhi, mhdet, loglikeli, mData,
		 vxm, vxs, mxm=<>, mxsu=<>, mxsl=<>,
		 time, timeall, timeran=0, timelik=0, timefun=0, timeint=0, timeres=0;

	mData = GetData(m_asY);
	mhdet = sqrt((2*M_PI)^m_cY * determinant(m_mMSbE.^2));		// covariance determinant
	mhi   = invert(m_mMSbE.^2);					// invert covariance of measurement shocks

	ms 	  = m_vSss + zeros(m_cPar, m_cS);			// start particles
	mx 	  = m_vXss + zeros(m_cPar, m_cX);			// steady state of state and policy

	loglikeli = 0;							// init likelihood
																								//timeall=timer();
	for(it = 0; it < sizer(mData); it++)
	{
		mss = rann(m_cPar, m_cSS) * m_mSSbE;			// state noise
		fg(&ms, ms, mx, mss);					// transition prior as proposal
		mx = m_oApprox.FastInterpolate(ms); 			// interpolate
		fy(&my, ms, mx, zeros(m_cPar, m_cMS));			// evaluate importance weights
		my -= mData[it][];					// observation error

		vw = exp(-0.5 * outer(my,mhi,'d')' )/mhdet;		// vw = exp(-0.5 * sumr(my*mhi .*my ) )/mhdet;

		vw = vw .== .NaN .? 0 .: vw;				// no policy can happen for extrem particles
		dws = sumc(vw);
		if(dws==0) return -.Inf;				// or extremely wrong parameters
		loglikeli += log(dws/m_cPar)	;			// loglikelihood contribution
																										//timelik += (timer()-time)/100;
																										//time=timer();
		vwi = resample(vw/dws)-1;				// selection step in c++
		ms = ms[vwi][];						// on normalized weights
		mx = mx[vwi][];
																	}
	return loglikeli;
}
