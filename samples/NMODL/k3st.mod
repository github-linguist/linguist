: Three state kinetic scheme for HH-like potassium channel
: Steady-state v-dependent state transitions have been fit
: Needs v-dependent time constants from tables created under hoc
NEURON {
    SUFFIX k3st
    USEION k READ ek WRITE ik
    RANGE g, gbar
    RANGE tau1_rec, tau2_rec
}
UNITS { (mV) = (millivolt) }
PARAMETER {
    gbar = 33 (millimho/cm2)
    d1 = -38 (mV)
    k1 = 0.151 (/mV)
    d2 = -25 (mV)
    k2 = 0.044 (/mV)
}

ASSIGNED {
    v     (mV)
    ek    (mV)
    g     (millimho/cm2)
    ik    (milliamp/cm2)
    kf1 (/ms)
    kb1 (/ms)
    kf2 (/ms)
    kb2 (/ms)
    tau1_rec
    tau2_rec
}

STATE { c1 c2 o }

BREAKPOINT {
    SOLVE kin METHOD sparse
    g = gbar*o
    ik = g*(v - ek)*(1e-3)
}

INITIAL { SOLVE kin STEADYSTATE sparse }

KINETIC kin {
    rates(v)
    ~ c1 <-> c2 (kf1, kb1)
    ~ c2 <-> o (kf2, kb2)
    CONSERVE c1 + c2 + o = 1
}

FUNCTION_TABLE tau1(v(mV)) (ms)
FUNCTION_TABLE tau2(v(mV)) (ms)

PROCEDURE rates(v(millivolt)) {
    LOCAL K1, K2
    K1 = exp(k2*(d2 - v) - k1*(d1 - v))
    kf1 = K1/(tau1(v)*(1+K1))
    kb1 = 1/(tau1(v)*(1+K1))
    K2 = exp(-k2*(d2 - v))
    kf2 = K2/(tau2(v)*(1+K2))
    kb2 = 1/(tau2(v)*(1+K2))
    tau1_rec = tau1(v)
    tau2_rec = tau2(v)
}
