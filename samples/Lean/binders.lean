/-
Copyright (c) 2017 Johannes Hölzl. All rights reserved.
Released under Apache 2.0 license as described in the leanprover-community/mathlib repo.
Authors: Johannes Hölzl

Binder elimination
-/
import order.complete_lattice

namespace old_conv
open tactic monad

meta instance : monad_fail old_conv :=
{ fail := λ α s, (λr e, tactic.fail (to_fmt s) : old_conv α), ..old_conv.monad }

meta instance : has_monad_lift tactic old_conv :=
⟨λα, lift_tactic⟩

meta instance (α : Type) : has_coe (tactic α) (old_conv α) :=
⟨monad_lift⟩

meta def current_relation : old_conv name := λr lhs, return ⟨r, lhs, none⟩

meta def head_beta : old_conv unit :=
λ r e, do n ← tactic.head_beta e, return ⟨(), n, none⟩

/- congr should forward data! -/
meta def congr_arg : old_conv unit → old_conv unit := congr_core (return ())
meta def congr_fun : old_conv unit → old_conv unit := λc, congr_core c (return ())

meta def congr_rule (congr : expr) (cs : list (list expr → old_conv unit)) :
  old_conv unit :=
λr lhs, do
  meta_rhs ← infer_type lhs >>= mk_meta_var, -- is maybe overly restricted for `heq`
  t ← mk_app r [lhs, meta_rhs],
  ((), meta_pr) ← solve_aux t (do
    apply congr,
    focus $ cs.map $ λc, (do
      xs ← intros,
      conversion (head_beta >> c xs)),
    done),
  rhs ← instantiate_mvars meta_rhs,
  pr ← instantiate_mvars meta_pr,
  return ⟨(), rhs, some pr⟩

meta def congr_binder (congr : name) (cs : expr → old_conv unit) : old_conv unit := do
  e ← mk_const congr,
  congr_rule e [λbs, do [b] ← return bs, cs b]

meta def funext' : (expr → old_conv unit) → old_conv unit := congr_binder ``_root_.funext

meta def propext' {α : Type} (c : old_conv α) : old_conv α := λr lhs, (do
  guard (r = `iff),
  c r lhs)
<|> (do
  guard (r = `eq),
  ⟨res, rhs, pr⟩ ← c `iff lhs,
  match pr with
  | some pr := return ⟨res, rhs, (expr.const `propext [] : expr) lhs rhs pr⟩
  | none := return ⟨res, rhs, none⟩
  end)

meta def apply (pr : expr) : old_conv unit :=
λ r e, do
  sl ← simp_lemmas.mk.add pr,
  apply_lemmas sl r e

meta def applyc (n : name) : old_conv unit :=
λ r e, do
  sl ← simp_lemmas.mk.add_simp n,
  apply_lemmas sl r e

meta def apply' (n : name) : old_conv unit := do
  e ← mk_const n,
  congr_rule e []

end old_conv

open expr tactic old_conv

/- Binder elimination:

We assume a binder `B : p → Π (α : Sort u), (α → t) → t`, where `t` is a type depending on `p`.
Examples:
  ∃: there is no `p` and `t` is `Prop`.
  ⨅, ⨆: here p is `β` and `[complete_lattice β]`, `p` is `β`

Problem: ∀x, _ should be a binder, but is not a constant!

Provide a mechanism to rewrite:

  B (x : α) ..x.. (h : x = t), p x  =  B ..x/t.., p t

Here ..x.. are binders, maybe also some constants which provide commutativity rules with `B`.

-/

meta structure binder_eq_elim :=
(match_binder  : expr → tactic (expr × expr))    -- returns the bound type and body
(adapt_rel     : old_conv unit → old_conv unit)          -- optionally adapt `eq` to `iff`
(apply_comm    : old_conv unit)                      -- apply commutativity rule
(apply_congr   : (expr → old_conv unit) → old_conv unit) -- apply congruence rule
(apply_elim_eq : old_conv unit)                      -- (B (x : β) (h : x = t), s x) = s t

meta def binder_eq_elim.check_eq (b : binder_eq_elim) (x : expr) : expr → tactic unit
| `(@eq %%β %%l %%r) := guard ((l = x ∧ ¬ x.occurs r) ∨ (r = x ∧ ¬ x.occurs l))
| _ := fail "no match"

meta def binder_eq_elim.pull (b : binder_eq_elim) (x : expr) : old_conv unit := do
  (β, f) ← lhs >>= (lift_tactic ∘ b.match_binder),
  guard (¬ x.occurs β)
  <|> b.check_eq x β
  <|> (do
    b.apply_congr $ λx, binder_eq_elim.pull,
    b.apply_comm)

meta def binder_eq_elim.push (b : binder_eq_elim) : old_conv unit :=
  b.apply_elim_eq
<|> (do
  b.apply_comm,
  b.apply_congr $ λx, binder_eq_elim.push)
<|> (do
  b.apply_congr $ b.pull,
  binder_eq_elim.push)

meta def binder_eq_elim.check (b : binder_eq_elim) (x : expr) : expr → tactic unit
| e := do
  (β, f) ← b.match_binder e,
  b.check_eq x β
  <|> (do
    (lam n bi d bd) ← return f,
    x ← mk_local' n bi d,
    binder_eq_elim.check $ bd.instantiate_var x)

meta def binder_eq_elim.old_conv (b : binder_eq_elim) : old_conv unit := do
  (β, f) ← lhs >>= (lift_tactic ∘ b.match_binder),
  (lam n bi d bd) ← return f,
  x ← mk_local' n bi d,
  b.check x (bd.instantiate_var x),
  b.adapt_rel b.push

theorem {u v} exists_elim_eq_left {α : Sort u} (a : α) (p : Π(a':α), a' = a → Prop) :
  (∃(a':α)(h : a' = a), p a' h) ↔ p a rfl :=
⟨λ⟨a', ⟨h, p_h⟩⟩, match a', h, p_h with ._, rfl, h := h end, λh, ⟨a, rfl, h⟩⟩

theorem {u v} exists_elim_eq_right {α : Sort u} (a : α) (p : Π(a':α), a = a' → Prop) :
  (∃(a':α)(h : a = a'), p a' h) ↔ p a rfl :=
⟨λ⟨a', ⟨h, p_h⟩⟩, match a', h, p_h with ._, rfl, h := h end, λh, ⟨a, rfl, h⟩⟩

meta def exists_eq_elim : binder_eq_elim :=
{ match_binder  := λe, (do `(@Exists %%β %%f) ← return e, return (β, f)),
  adapt_rel     := propext',
  apply_comm    := applyc ``exists_comm,
  apply_congr   := congr_binder ``exists_congr,
  apply_elim_eq := apply' ``exists_elim_eq_left <|> apply' ``exists_elim_eq_right }

theorem {u v} forall_comm {α : Sort u} {β : Sort v} (p : α → β → Prop) :
  (∀a b, p a b) ↔ (∀b a, p a b) :=
⟨assume h b a, h a b, assume h b a, h a b⟩

theorem {u v} forall_elim_eq_left {α : Sort u} (a : α) (p : Π(a':α), a' = a → Prop) :
  (∀(a':α)(h : a' = a), p a' h) ↔ p a rfl :=
⟨λh, h a rfl, λh a' h_eq, match a', h_eq with ._, rfl := h end⟩

theorem {u v} forall_elim_eq_right {α : Sort u} (a : α) (p : Π(a':α), a = a' → Prop) :
  (∀(a':α)(h : a = a'), p a' h) ↔ p a rfl :=
⟨λh, h a rfl, λh a' h_eq, match a', h_eq with ._, rfl := h end⟩

meta def forall_eq_elim : binder_eq_elim :=
{ match_binder  := λe, (do (expr.pi n bi d bd) ← return e, return (d, expr.lam n bi d bd)),
  adapt_rel     := propext',
  apply_comm    := applyc ``forall_comm,
  apply_congr   := congr_binder ``forall_congr,
  apply_elim_eq := apply' ``forall_elim_eq_left <|> apply' ``forall_elim_eq_right }

meta def supr_eq_elim : binder_eq_elim :=
{ match_binder  := λe, (do `(@supr %%α %%cl %%β %%f) ← return e, return (β, f)),
  adapt_rel     := λc, (do r ← current_relation, guard (r = `eq), c),
  apply_comm    := applyc ``supr_comm,
  apply_congr   := congr_arg ∘ funext',
  apply_elim_eq := applyc ``supr_supr_eq_left <|> applyc ``supr_supr_eq_right }

meta def infi_eq_elim : binder_eq_elim :=
{ match_binder  := λe, (do `(@infi %%α %%cl %%β %%f) ← return e, return (β, f)),
  adapt_rel     := λc, (do r ← current_relation, guard (r = `eq), c),
  apply_comm    := applyc ``infi_comm,
  apply_congr   := congr_arg ∘ funext',
  apply_elim_eq := applyc ``infi_infi_eq_left <|> applyc ``infi_infi_eq_right }


universes u v w w₂
variables {α : Type u} {β : Type v} {ι : Sort w} {ι₂ : Sort w₂} {s t : set α} {a : α}

section
variables [complete_lattice α]

example {s : set β} {f : β → α} : Inf (set.image f s) = (⨅ a ∈ s, f a) :=
begin
  simp [Inf_eq_infi, infi_and],
  conversion infi_eq_elim.old_conv,
end

example {s : set β} {f : β → α} : Sup (set.image f s) = (⨆ a ∈ s, f a) :=
begin
  simp [Sup_eq_supr, supr_and],
  conversion supr_eq_elim.old_conv,
end

end