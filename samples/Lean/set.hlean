-- Copyright (c) 2015 Jakob von Raumer. All rights reserved.
-- Released under Apache 2.0 license as described in the file LICENSE.
-- Authors: Jakob von Raumer
-- Category of sets

import .basic types.pi trunc

open truncation sigma sigma.ops pi function eq morphism precategory
open equiv

namespace precategory

  universe variable l

  definition set_precategory : precategory.{l+1 l} (Σ (A : Type.{l}), is_hset A) :=
  begin
    fapply precategory.mk.{l+1 l},
                  intros, apply (a.1 → a_1.1),
                intros, apply trunc_pi, intros, apply b.2,
              intros, intro x, exact (a_1 (a_2 x)),
            intros, exact (λ (x : a.1), x),
          intros, apply funext.path_pi, intro x, apply idp,
        intros, apply funext.path_pi, intro x, apply idp,
      intros, apply funext.path_pi, intro x, apply idp,
  end

end precategory

namespace category

  universe variable l
  local attribute precategory.set_precategory.{l+1 l} [instance]

  definition set_category_equiv_iso (a b : (Σ (A : Type.{l}), is_hset A))
    : (a ≅ b) = (a.1 ≃ b.1) :=
  /-begin
    apply ua, fapply equiv.mk,
      intro H,
        apply (isomorphic.rec_on H), intros (H1, H2),
        apply (is_iso.rec_on H2), intros (H3, H4, H5),
        fapply equiv.mk,
        apply (isomorphic.rec_on H), intros (H1, H2),
        exact H1,
      fapply is_equiv.adjointify, exact H3,
          exact sorry,
        exact sorry,
  end-/ sorry

  definition set_category : category.{l+1 l} (Σ (A : Type.{l}), is_hset A) :=
  /-begin
    assert (C : precategory.{l+1 l} (Σ (A : Type.{l}), is_hset A)),
      apply precategory.set_precategory,
    apply category.mk,
    assert (p : (λ A B p, (set_category_equiv_iso A B) ▹ iso_of_path p) = (λ A B p, @equiv_path A.1 B.1 p)),
    apply is_equiv.adjointify,
        intros,
        apply (isomorphic.rec_on a_1), intros (iso', is_iso'),
        apply (is_iso.rec_on is_iso'), intros (f', f'sect, f'retr),
        fapply sigma.path,
          apply ua, fapply equiv.mk, exact iso',
          fapply is_equiv.adjointify,
              exact f',
            intros, apply (f'retr ▹ _),
          intros, apply (f'sect ▹ _),
        apply (@is_hprop.elim),
        apply is_trunc_is_hprop,
      intros,
  end -/ sorry

end category
