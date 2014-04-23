:- module(quantum_numbers, [quantum_number/3,
                            quantum_number/1,
                            quantum_number_mf/3,
                            orbital_quantum_number/1,
                            conserved_quantum_number/1,
                            flavour_quantum_number/1]).
:- use_module(lorentz_group, [lorentz_covariant/1]).
:- use_module(particles, []).
:- use_module(utils, [call_semidet_ground/2 as particle_call_semidet]).

:- meta_predicate particle_call_semidet(1, ?).

:- multifile quantum_number_mf/3.

lorenz_group:lorentz_covariant_mf(spin).

%%	quantum_number(+NumberType, +AntiParticle, ?AntiNumber) is semidet.
quantum_number(NumberType, Particle, Number) :-
    particle_call_semidet(particles:not_anti_particle, Particle),
    quantum_number_mf(NumberType, Particle, Number).

quantum_number(NumberType, anti(AntiParticle), AntiNumber) :-
    particles:proper_anti_particle(Particle, anti(AntiParticle)),
    quantum_number_mf(NumberType, Particle, Number),
    (   lorentz_covariant(NumberType)
    ->  AntiNumber = Number
    ;   AntiNumber is -Number
    ).

quantum_number(Q) :- particle_call_semidet(quantum_number_nd, Q).

quantum_number_nd(Q) :- flavour_quantum_number(Q).
quantum_number_nd(Q) :- conserved_quantum_number(Q).
quantum_number_nd(Q) :- orbital_quantum_number(Q).

orbital_quantum_number(principal).
orbital_quantum_number(azimuthal).
orbital_quantum_number(magnetic).
orbital_quantum_number(spin).

flavour_quantum_number(isospin).
flavour_quantum_number(charm).
flavour_quantum_number(strange).
flavour_quantum_number(top).
flavour_quantum_number(bottom).

conserved_quantum_number(baryon).
conserved_quantum_number(lepton).
conserved_quantum_number(weak_isospin).
conserved_quantum_number(electric_charge).
conserved_quantum_number(x_charge).
