/** Replicate Imai, Jain and Ching Econometrica 2009 (incomplete).

**/
#include "IJCEmet2009.h"

Kapital::Kapital(L,const N,const entrant,const exit,const KP){
	StateVariable(L,N);
	this.entrant = entrant;
	this.exit = exit;
	this.KP = KP;
	actual = Kbar*vals/(N-1);
	upper = log(actual~.Inf);
	}

Kapital::Transit(FeasA) {
	decl ent =CV(entrant), stayout = FeasA[][exit.pos], tprob, sigu = CV(KP[SigU]);
	if (!v && !ent) return { <0>, ones(stayout) };
	tprob = ent ? probn( (upper-CV(KP[Kbe]))/sigu )
	           : probn( (upper-(CV(KP[Kb0])+CV(KP[Kb2])*upper[v])) / sigu );
	tprob = tprob[1:] - tprob[:N-1];
	return { vals, tprob.*(1-stayout)+(1.0~zeros(1,N-1)).*stayout };
	}

FirmEntry::Run() {
	Initialize();
	GenerateSample();
	BDP->BayesianDP();
	}

FirmEntry::Initialize() {
	Rust::Initialize(Reachable,0);
	sige = new StDeviations("sige",<0.3,0.3>,0);
	entrant = new LaggedAction("entrant",d);
	KP = new array[Kparams];
		KP[Kbe] = new Positive("be",0.5);
		KP[Kb0] = new Free("b0",0.0);
		KP[Kb1] = new Determined("b1",0.0);
		KP[Kb2] = new Positive("b2",0.4);
		KP[SigU] = new Positive("sigu",0.4);
	EndogenousStates(K = new Kapital("K",KN,entrant,d,KP),entrant);
	SetDelta(new Probability("delta",0.85));
	kcoef = new Positive("kcoef",0.1);
	ecost = new Negative("ec",-0.4);
	CreateSpaces();
	}

FirmEntry::GenerateSample() {
	Volume = LOUD;
	EM = new ValueIteration(0);
//	EM -> Solve(0,0);
	data = new DataSet(0,EM);
	data->Simulate(DataN,DataT,0,FALSE);
	data->Print("firmentry.xls");
	BDP = new ImaiJainChing("FMH",data,EM,ecost,sige,kcoef,KP,delta);
	}

/** Capital stock can be positive only for incumbents.
**/
FirmEntry::Reachable()	{ return CV(entrant)*CV(K) ? 0 : new FirmEntry() ;	}

/** The one period return.
<DD>
<pre>U = </pre>
</DD>
**/
FirmEntry::Utility()  {
	decl ent = CV(entrant),
		 u =
		     ent*CV(ecost)+(1-ent)*CV(kcoef)*AV(K)
		   | 0.0;
	return u;
	}
