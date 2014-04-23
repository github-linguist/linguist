:- module(atoms, [
                  atom/5,
                  nuclide/1,
                  nuclide/2,
                  atomic_number/2,
                  isotope/3,
                  most_abundant_isotope/3,
                  even_even/1,
                  noble/1,
                  noble_shell/2,
                  noble_gas_below/3,
                  atom_block/2,
                  block/3,
                  atom_orbitals/2,
                  electron_configuration/1,
                  electron_configuration/2,
                  electron_configuration_pairs/3
                 ]).

:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(particle_taxonomy).
:- use_module(symbols, [symbol/2, write_symbol/1]).
:- use_module(quantum_numbers, []).
:- use_module(utils, [
                      findnsols/4 as find_electron_configs,
                      between2d/4 as in_block,
                      call_semidet_ground/2 as atoms_call_semidet,
                      call_semidet_ground/3 as block_call_semidet,
                      call_semidet_ground_first/3 as atoms_call_semidet_first
                     ]).
:- use_module(library(apply), [foldl/4 as reduce_noble_shell_foldl]).

:- meta_predicate
    atoms_call_semidet(1, ?),
    block_call_semidet(2, ?, ?),
    find_electron_configs(+, ?, :, -),
    atoms_call_semidet_first(2, ?, ?),
    reduce_noble_shell_foldl(3, +, +, -).

:- dynamic noble_shell_dyn/2.

user:portray(shell(N, L, C)) :-
    N > 0, L >= 0, C > 0,
    write_symbol(shell(N, L, C)).

user:portray(orbital(N, L, ML, S)) :-
    N > 0, L >= 0, integer(ML), rational(S),
    write_symbol(orbital(N, L, ML, S)).

user:portray(atom(Atom)) :-
    atom(Atom),
    atom(Atom, _, _, _, _),
    write_symbol(atom(Atom)).

user:portray(isotope(Atom, Neutrons)) :-
    atom(Atom),
    integer(Neutrons),
    write_symbol(isotope(Atom, Neutrons)).

user:portray(nuclide(Atom, Neutrons, Ionization)) :-
    atom(Atom),
    integer(Neutrons),
    integer(Ionization),
    write_symbol(nuclide(Atom, Neutrons, Ionization)).

symbols:symbol_mf(sharp, s).
symbols:symbol_mf(principal, p).
symbols:symbol_mf(diffuse, d).
symbols:symbol_mf(fundamental, f).

atom(h,   1, hydrogen,    1, 1).
atom(he,  2, helium,     18, 1).
atom(li,  3, lithium,     1, 2).
atom(be,  4, beryllium,   2, 2).
atom(b,   5, boron,      13, 2).
atom(c,   6, carbon,     14, 2).
atom(n,   7, nitrogen,   15, 2).
atom(o,   8, oxygen,     16, 2).
atom(f,   9, flurine,    17, 2).
atom(ne, 10, neon,       18, 2).
atom(na, 11, sodium,      1, 3).
atom(mg, 12, magnesium,   2, 3).
atom(al, 13, aluminium,  13, 3).
atom(si, 14, silicon,    14, 3).
atom(p,  15, phosphorus, 15, 3).
atom(s,  15, sulfur,     16, 3).
atom(cl, 17, chlorine,   17, 3).
atom(ar, 18, argon,      18, 3).
atom(k,  19, potassium,   1, 4).
atom(ca, 20, calcium,     2, 4).
atom(sc, 21, scandium,    3, 4).
atom(ti, 22, titanium,    4, 4).
atom(v,  23, vanadium,    5, 4).
atom(cr, 24, chromium,    6, 4).
atom(mn, 25, manganese,   7, 4).
atom(fe, 26, iron,        8, 4).
atom(co, 27, cobalt,      9, 4).
atom(ni, 28, nickel,     10, 4).
atom(cu, 29, copper,     11, 4).
atom(zn, 30, zinc,       12, 4).
atom(ga, 31, gallium,    13, 4).
atom(ge, 32, germanium,  14, 4).
atom(as, 33, arsenic,    15, 4).
atom(se, 34, selenium,   16, 4).
atom(br, 35, bromine,    17, 4).
atom(kr, 36, krypton,    18, 4).
atom(rb, 37, ribidium,    1, 5).
atom(sr, 38, strontium,   2, 5).
atom(y,  39, yttrium,     3, 5).
atom(zr, 40, zirconium,   4, 5).
atom(nb, 41, niobium,     5, 5).
atom(mo, 42, molybdenum,  6, 5).
atom(tc, 43, technetium,  7, 5).
atom(ru, 44, ruthenium,   8, 5).
atom(rh, 45, rhodium,     9, 5).
atom(pd, 46, palladium,  10, 5).
atom(ag, 47, silver,     11, 5).
atom(cd, 48, cadmium,    12, 5).
atom(in, 49, indium,     13, 5).
atom(sn, 50, tin,        14, 5).
atom(sb, 51, antimony,   15, 5).
atom(te, 52, tellerium,  16, 5).
atom(i,  53, iodine,     17, 5).
atom(xe, 54, xenon,      18, 5).
atom(cs, 55, caesium,     1, 6).
atom(ba, 56, barium,      2, 6).
atom(la, 57, lanthanum,   l, 6).
atom(ce, 58, cerium,      l, 6).
atom(pr, 59, praseodymium,l, 6).
atom(nd, 60, neodymium,   l, 6).
atom(pm, 61, promethium,  l, 6).
atom(sm, 62, samarium,    l, 6).
atom(eu, 63, europium,    l, 6).
atom(gd, 64, gadolinium,  l, 6).
atom(tb, 65, terbium,     l, 6).
atom(dy, 66, dysprosium,  l, 6).
atom(ho, 67, holmium,     l, 6).
atom(er, 68, erbium,      l, 6).
atom(tm, 69, thulium,     l, 6).
atom(yb, 70, ytterbium,   l, 6).
atom(lu, 71, lutetium,    l(lu), 6).
atom(hf, 72, hafnium,     4, 7).
atom(ta, 73, tantalum,    5, 6).
atom(w,  74, tungsten,    6, 6).
atom(re, 75, rhenium,     7, 6).
atom(os, 76, osmium,      8, 6).
atom(ir, 77, iridium,     9, 6).
atom(pt, 78, platinum,   10, 6).
atom(au, 79, gold,       11, 6).
atom(hg, 80, mercury,    12, 6).
atom(tl, 81, thallium,   13, 6).
atom(pb, 82, lead,       14, 6).
atom(bi, 83, bismuth,    15, 6).
atom(po, 84, polonium,   16, 6).
atom(at, 85, astatine,   17, 6).
atom(rn, 86, radon,      18, 6).
atom(fr, 87, francium,    1, 7).
atom(ra, 88, radium,      2, 7).
atom(ac, 89, actinium,    a, 7).
atom(th, 90, thorium,     a, 7).
atom(pa, 91, protactinium,a, 7).
atom(u,  92, uranium,     a, 7).
atom(np, 93, neptunium,   a, 7).
atom(pu, 94, plutonium,   a, 7).
atom(am, 95, americium,   a, 7).
atom(cm, 96, curium,      a, 7).
atom(bk, 97, berkelium,   a, 7).
atom(cf, 98, californium, a, 7).
atom(es, 99, einsteinium, a, 7).
atom(fm,100, fermium,     a, 7).
atom(md,101, mendelevium, a, 7).
atom(no,102, nobelium,    a, 7).
atom(lr,103, lawrencium,  a(lr), 7).
atom(rf,104, rutherfordium,4, 7).
atom(db,105, dubnium,     5, 7).
atom(sg,106, seaborgium,  6, 7).
atom(bh,107, bohrium,     7, 7).
atom(hs,108, hassium,     8, 7).
atom(mt,109, meitnerium,  9, 7).
atom(ds,110, darmstadtium,10, 7).
atom(rg,111, roentgenium, 11, 7).
atom(cn,112, copernicium, 12, 7).
atom(uut,113,ununtrium,   13, 7).
atom(fl,114, flerovium,   14, 7).
atom(uup,115,ununpentium, 15, 7).
atom(lv,116, livermorium, 16, 7).
atom(uus,117,ununseptium, 17, 7).
atom(uuo,118,ununoctium,  18, 7).

proper_isotope_name(h-1, protium).
proper_isotope_name(h-2, deuterium).
proper_isotope_name(h-3, tritium).

isotope(h, 0, 0.999885).
isotope(h, 1, 0.000115).
isotope(h, 2, trace).
isotope(h, 3, decayed(he-3)).
isotope(h, 4, decayed(he-3)).
isotope(h, 5, decayed(he-3)).
isotope(h, 6, decayed(he-3)).
isotope(h, 7, decayed(he-3)).

isotope(he, 2, 0.99999866).
isotope(he, 1, 0.00000134).
isotope(he, 0, syn).
isotope(he, 4, syn).
isotope(he, 5, syn).
isotope(he, 6, syn).
isotope(he, 7, syn).
isotope(he, 8, syn).

isotope(li, 4, 0.92275).
isotope(li, 3, 0.07714).
isotope(li, 1, syn).
isotope(li, 2, syn).
isotope(li, 5, syn).
isotope(li, 6, syn).
isotope(li, 7, syn).
isotope(li, 8, syn).
isotope(li, 9, syn).

isotope(ne, 10, 0.9048).
isotope(ne, 11, 0.0027).
isotope(ne, 12, 0.0925).

isotope(ar, 22, 0.996035).
isotope(ar, 18, 0.003336).
isotope(ar, 20, 0.000629).
isotope(ar, 24, trace).

isotope(kr, 48, 0.56987).
isotope(kr, 50, 0.17279).
isotope(kr, 46, 0.11593).
isotope(kr, 47, 0.11500).
isotope(kr, 44, 0.02286).
isotope(kr, 42, 0.00355).
isotope(kr, 45, trace).
isotope(kr, 49, trace).

isotope(xe, 78, 0.269086).
isotope(xe, 75, 0.264006).
isotope(xe, 80, 0.104357).

isotope(radon, 222, trace).

% http://en.wikipedia.org/w/index.php?title=Isotope_lists,_0-24&action=edit
%

even_even(isotope(Atom, Neutrons)) :-
    isotope(Atom, Neutrons, _),
    atomic_number(Atom, AtomicNumber),
    0 =:= AtomicNumber mod 2,
    0 =:= Neutrons mod 2.

most_abundant_isotope(Atom, nuclide(Atom, N, 0), MoleFrac) :-
    isotope(Atom, N, MoleFrac),
    number(MoleFrac),
    !.

nuclide(nuclide(A, N, I)) :-
    Nuclide = nuclide(A, N, I),
    atoms_call_semidet_first(nuclide_nd, Nuclide, Nuclide).

nuclide(Atom0, Nuclide) :-
    (   atom(Atom0)
    ->  atom_length(Atom0, Length),
        (   Length =< 3 -> atom(Atom0, _, _, _, _),
            Atom = Atom0
        ;   atom(Atom, _, Atom0, _, _)
        ),
        !,
        nuclide_nd(atom(Atom), Nuclide)
    ;   Atom0 = atom(Atom) -> nuclide_nd(Atom0, Nuclide)
    ;   Atom0 = isotope(_, _) -> nuclide_nd(Atom0, Nuclide)
    ;   Atom0 = nuclide(_, _, _) -> nuclide_nd(Atom0, Nuclide)
    ).

nuclide_nd(nuclide(Atom, Neutron, Ionization), nuclide(Atom, Neutron, Ionization)) :-
    isotope(Atom, Neutron, _),
    atomic_number(Atom, AtomicNumber),
    Min is -2 * AtomicNumber,
    between(Min, AtomicNumber, Ionization).

nuclide_nd(isotope(Atom, Neutrons), nuclide(Atom, Neutrons, 0)) :-
    atom(Atom, _, _, _, _),
    isotope(Atom, Neutrons, _).

nuclide_nd(atom(Atom), nuclide(Atom, Neutrons, 0)) :-
    atom(Atom, _, _, _, _),
    once(isotope(Atom, Neutrons, _)).

atom_block(Atom, Block) :-
    atom(Atom, _, _, G, P),
    block(P, G, Block).

%%	block(+Period, +Group, -Block) is semidet.
%%	block(?Period, ?Group, -Block) is nondet.
block(P, G, B) :- block_call_semidet(block_nd(B), P, G).

%%	block_nd(?Block, ?Period, ?Group) is nondet.
block_nd(1-s, 1, 1).
block_nd(1-s, 1, 18).
block_nd(4-f, 4, l).
block_nd(5-f, 5, a).
block_nd(5-d, 6, l(lu)).
block_nd(6-d, 7, a(lr)).
block_nd(P-s, P, G) :- in_block(2-7, 1-2, P, G).
block_nd(P-p, P, G) :- in_block(2-7, 13-18, P, G).
block_nd(P1-d, P, G) :-
    in_block(3-7, 3-13, P, G),
    utils:safe_is(P1, P - 1).

noble(Atom0) :-
    nuclide(Atom0, nuclide(Atom, _, _)),
    atoms_call_semidet(noble_nd, Atom).

noble_shell(Noble, Shell) :-
    noble(Noble),
    (   noble_shell_dyn(Noble, Shell)
    ->  true
    ;   atoms_call_semidet_first(atom_orbitals, Noble, Shell),
        assert(noble_shell_dyn(Noble, Shell))
    ).

noble_nd(he).
noble_nd(ne).
noble_nd(ar).
noble_nd(kr).
noble_nd(xe).
noble_nd(rn).

atomic_number(Atom0, AtomicNumber) :-
    nuclide(Atom0, nuclide(Atom, _, _)),
    atom(Atom, AtomicNumber, _, _, _).

electron_configuration_pairs(Electrons, Configs, Pairs) :-
    electron_configuration(Electrons, Configs),
    setof(P-L, ML^S^member(orbital(P, L, ML, S), Configs), Pairs).

atom_orbitals(Atom0, AggrOrbitals) :-
    nuclide(Atom0, nuclide(Atom, _Neutron, Ionization)),
    atomic_number(Atom, AtomicNumber),
    Electrons is AtomicNumber - Ionization,
    electron_configuration_pairs(Electrons, Configs, Pairs),
    maplist(atom_orbitals_count(Configs), Pairs, Shells),
    (   Electrons > 2
    ->  reduce_noble_shells(Electrons, Shells, AggrOrbitals)
    ;   AggrOrbitals = Shells
    ).

atom_orbitals_count(Orbitals, P-L, shell(P, L, Count)) :-
    aggregate_all(count, member(orbital(P, L, _, _), Orbitals), Count).

principal_number(Principal) :-
    principal_number(inf, Principal).
principal_number(Max, Principal) :-
    between(1, Max, Principal).

azimuthal_number(Max, Azimuthal) :-
    between(0, Max, Azimuthal).

principal_azimuthal(Principal, Azimuthal) :-
    Principal >= 1,
    Max is Principal - 1,
    azimuthal_number(Max, Azimuthal).

azimuthal_magnetic(Azimuthal, Magnetic) :-
    utils:safe_is(LV, Azimuthal),
    Min is -LV,
    Max is +LV,
    between(Min, Max, Magnetic).

electron_configuration(Electrons, Orbitals) :-
    find_electron_configs(Electrons, O,
                          electron_configuration(O), Orbitals).

electron_configuration(orbital(P, L, M, S)) :-
    principal_number(PL),
    principal_number(PL, P),
    azimuthal_number(PL, L),
    PL =:= P + L,
    principal_azimuthal(P, L),
    (   S is +1 rdiv 2
    ;   S is -1 rdiv 2
    ),
    azimuthal_magnetic(L, M).

reduce_noble_shells(Electrons, Orbitals, AggrOrbitals) :-
    findall(Noble, noble_gas_below(Electrons, Noble, _), Nobles),
    reduce_noble_shell_foldl(reduce_noble_shell,
                             Nobles, Orbitals, AggrOrbitals).

reduce_noble_shell(Noble, Orbitals, AggrOrbitals) :-
    (   noble_shell(Noble, NobleShell)
    ->  append(NobleShell, OuterShell, Orbitals),
        append([Noble], OuterShell, AggrOrbitals)
    ;   AggrOrbitals = Orbitals
    ).

noble_gas_below(E, Noble, Rest) :-
    E > 1,
    noble(atom(Noble)),
    atom(Noble, NobleNumber, _, _, _),
    E > NobleNumber,
    utils:safe_is(Rest, E rem NobleNumber).

quantum_numbers:quantum_number_mf(principal, p=P, P) :-
    principal_number(P).

quantum_numbers:quantum_number_mf(azimuthal, n=Principal, Azimuthal) :-
    principal_azimuthal(Principal, Azimuthal).

quantum_numbers:quantum_number_mf(magnetic, l=L, Ml) :-
    azimuthal_magnetic(L, Ml).

symbols:symbol_mf(isotope(Atom, Neutrons), Symbol) :-
    atomic_number(Atom, AtomicNumber),
    utils:capitalize(Atom, AtomCapitalized),
    Mass is AtomicNumber + Neutrons,
    utils:term_sup(Mass, MassSup),
    atom_concat(MassSup, AtomCapitalized, Symbol).

symbols:symbol_mf(ionization(Ionization), Symbol) :-
    utils:number_sign_sup(Ionization, IonSign),
    IonAbs is abs(Ionization),
    (   IonAbs =< 1 -> IonAbsSup = ''
    ;   IonAbs >  1 -> utils:term_sup(IonAbs, IonAbsSup)
    ),
    atom_concat(IonAbsSup, IonSign, Symbol).

symbols:symbol_mf(nuclide(Atom, Neutrons, Ionization), Symbol) :-
    symbol(isotope(Atom, Neutrons), IsotopeSymbol),
    symbol(ionization(Ionization), IonizationSymbol),
    atom_concat(IsotopeSymbol, IonizationSymbol, Symbol).

symbols:symbol_mf(atom(Atom), Symbol) :-
    atomic_number(Atom, AtomicNumber),
    utils:term_sub(AtomicNumber, AtomicNumberSub),
    utils:capitalize(Atom, AtomCapitalized),
    atom_concat(AtomicNumberSub, AtomCapitalized, Symbol).

symbols:symbol_mf(shell(P, L, Count), Symbol) :-
    azimuthal_symbol(L, LS),
    utils:term_sup(Count, CountSup),
    format(atom(Symbol), '~d~a~a', [P, LS, CountSup]).

symbols:symbol_mf(orbital(P, L, ML, S), Symbol) :-
    azimuthal_symbol(L, LS),
    utils:term_sup(ML, MLSup),
    (   utils:safe_is(S, +1 rdiv 2) -> SS = ↑
    ;   utils:safe_is(S, -1 rdiv 2) -> SS = ↓
    ),
    format(atom(Symbol), '~d~w~w~w', [P, LS, MLSup, SS]).

azimuthal_symbol(Number, Char) :-
    (   var(Char), Number >= 7
    ->  Code is 0'k + Number - 7, atom_codes(Char, Code)
    ;   Number = 0 -> Char = s
    ;   Number = 1 -> Char = p
    ;   Number = 2 -> Char = d
    ;   Number = 3 -> Char = f
    ;   Number = 4 -> Char = g
    ;   Number = 5 -> Char = h
    ;   Number = 6 -> Char = i
    ;   (
         var(Number),
         atom(Char),
         atom_length(Char, 1),
         atom_codes(Char, Code)
        )
    ->  Number is Code - 0'k,
        Number >= 7
    ).

:- begin_tests(atoms).

test('nuclide(h)', N == nuclide(h, 0, 0)) :-
    nuclide(h, N).

test('nuclide_stabilty') :-
    forall(once(most_abundant_isotope(_, Nuclide, _)), even_even(Nuclide)).

test('atom_block(he)', B == 1-s) :- atom_block(he, B).

test('electron_spin', ElectronSpin =:= 1 rdiv 2) :-
    once(quantum_numbers:quantum_number(spin, electron, ElectronSpin)).

test('atomic_number(h, 1)') :- atomic_number(atom(h), 1).
test('atomic_number(carbon, 1)', Number == 6) :- atomic_number(carbon, Number).

test('atom_orbitals(h)',  [Shell == [shell(1, 0, 1)]]) :- atom_orbitals(h,  Shell).
test('atom_orbitals(he)', [Shell == [shell(1, 0, 2)]])  :- atom_orbitals(he, Shell).
test('atom_orbitals(li)', [Shell == [he, shell(2, 0, 1)]]) :- atom_orbitals(li, Shell).
test('atom_orbitals(ds)',
     [Shell == [rn, shell(5, 4, 14), shell(6, 2, 8), shell(7, 0, 2)]]) :-
    atom_orbitals(ds, Shell).

test('write_xml') :-
    load_xml_file('data/www.webelements.com/holmium/isotopes.html', Xml),
    open('data/holmium_isotopes.xml', write, Stream, []),
    xml_write(Stream, Xml, []),
    close(Stream).

:- end_tests(atoms).



