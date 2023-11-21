/-
Copyright (c) 2021 Eric Rodriguez. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Eric Rodriguez
-/
import Mathlib.Data.Fintype.CardEmbedding
import Mathlib.Probability.CondCount
import Mathlib.Probability.Notation

#align_import wiedijk_100_theorems.birthday_problem from "leanprover-community/mathlib"@"5563b1b49e86e135e8c7b556da5ad2f5ff881cad"

/-!
# Birthday Problem

This file proves Theorem 93 from the [100 Theorems List](https://www.cs.ru.nl/~freek/100/).

As opposed to the standard probabilistic statement, we instead state the birthday problem
in terms of injective functions. The general result about `Fintype.card (α ↪ β)` which this proof
uses is `Fintype.card_embedding_eq`.
-/


namespace Theorems100

local notation "|" x "|" => Finset.card x

local notation "‖" x "‖" => Fintype.card x

/-- **Birthday Problem**: set cardinality interpretation. -/
theorem birthday :
    2 * ‖Fin 23 ↪ Fin 365‖ < ‖Fin 23 → Fin 365‖ ∧ 2 * ‖Fin 22 ↪ Fin 365‖ > ‖Fin 22 → Fin 365‖ := by
  -- This used to be
  -- `simp only [Nat.descFactorial, Fintype.card_fin, Fintype.card_embedding_eq, Fintype.card_fun]`
  -- but after leanprover/lean4#2790 that triggers a max recursion depth exception.
  -- As a workaround, we make some of the reduction steps more explicit.
  rw [Fintype.card_embedding_eq, Fintype.card_fun, Fintype.card_fin, Fintype.card_fin]
  rw [Fintype.card_embedding_eq, Fintype.card_fun, Fintype.card_fin, Fintype.card_fin]
  decide
#align theorems_100.birthday Theorems100.birthday

section MeasureTheory

open MeasureTheory ProbabilityTheory

open scoped ProbabilityTheory ENNReal

variable {n m : ℕ}

/- In order for Lean to understand that we can take probabilities in `Fin 23 → Fin 365`, we must
tell Lean that there is a `MeasurableSpace` structure on the space. Note that this instance
is only for `Fin m` - Lean automatically figures out that the function space `Fin n → Fin m`
is _also_ measurable, by using `MeasurableSpace.pi`, and furthermore that all sets are measurable,
from `MeasurableSingletonClass.pi`. -/
instance : MeasurableSpace (Fin m) :=
  ⊤

instance : MeasurableSingletonClass (Fin m) :=
  ⟨fun _ => trivial⟩

/- We then endow the space with a canonical measure, which is called ℙ.
We define this to be the conditional counting measure. -/
noncomputable instance : MeasureSpace (Fin n → Fin m) :=
  ⟨condCount Set.univ⟩

-- The canonical measure on `Fin n → Fin m` is a probability measure (except on an empty space).
instance : IsProbabilityMeasure (ℙ : Measure (Fin n → Fin (m + 1))) :=
  condCount_isProbabilityMeasure Set.finite_univ Set.univ_nonempty

theorem FinFin.measure_apply {s : Set <| Fin n → Fin m} :
    ℙ s = |s.toFinite.toFinset| / ‖Fin n → Fin m‖ := by
  erw [condCount_univ, Measure.count_apply_finite]
#align theorems_100.fin_fin.measure_apply Theorems100.FinFin.measure_apply

/-- **Birthday Problem**: first probabilistic interpretation. -/
theorem birthday_measure :
    ℙ ({f | (Function.Injective f)} : Set ((Fin 23) → (Fin 365))) < 1 / 2 := by
  rw [FinFin.measure_apply]
  generalize_proofs hfin
  have : |hfin.toFinset| = 42200819302092359872395663074908957253749760700776448000000 := by
    trans ‖Fin 23 ↪ Fin 365‖
    · rw [← Fintype.card_coe]
      apply Fintype.card_congr
      rw [Set.Finite.coeSort_toFinset, Set.coe_setOf]
      exact Equiv.subtypeInjectiveEquivEmbedding _ _
    · rw [Fintype.card_embedding_eq, Fintype.card_fin, Fintype.card_fin]
      rfl
  rw [this, ENNReal.lt_div_iff_mul_lt, mul_comm, mul_div, ENNReal.div_lt_iff]
  rotate_left; (iterate 2 right; norm_num); decide; (iterate 2 left; norm_num)
  simp only [Fintype.card_pi]
  norm_num
#align theorems_100.birthday_measure Theorems100.birthday_measure

end MeasureTheory

end Theorems100
