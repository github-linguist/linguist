/-
Copyright (c) 2014 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.

Module: algebra.binary
Authors: Leonardo de Moura, Jeremy Avigad

General properties of binary operations.
-/

import logic.eq
open eq.ops

namespace binary
  section
    variable {A : Type}
    variables (op₁ : A → A → A) (inv : A → A) (one : A)

    local notation a * b := op₁ a b
    local notation a ⁻¹  := inv a
    local notation 1     := one

    definition commutative := ∀a b, a * b = b * a
    definition associative := ∀a b c, (a * b) * c = a * (b * c)
    definition left_identity := ∀a, 1 * a = a
    definition right_identity := ∀a, a * 1 = a
    definition left_inverse := ∀a, a⁻¹ * a = 1
    definition right_inverse := ∀a, a * a⁻¹ = 1
    definition left_cancelative := ∀a b c, a * b = a * c → b = c
    definition right_cancelative := ∀a b c, a * b = c * b → a = c

    definition inv_op_cancel_left := ∀a b, a⁻¹ * (a * b) = b
    definition op_inv_cancel_left := ∀a b, a * (a⁻¹ * b) = b
    definition inv_op_cancel_right := ∀a b, a * b⁻¹ * b =  a
    definition op_inv_cancel_right := ∀a b, a * b * b⁻¹ = a

    variable (op₂ : A → A → A)

    local notation a + b := op₂ a b

    definition left_distributive := ∀a b c, a * (b + c) = a * b + a * c
    definition right_distributive := ∀a b c, (a + b) * c = a * c + b * c
  end

  context
    variable {A : Type}
    variable {f : A → A → A}
    variable H_comm : commutative f
    variable H_assoc : associative f
    infixl `*` := f
    theorem left_comm : ∀a b c, a*(b*c) = b*(a*c) :=
    take a b c, calc
      a*(b*c) = (a*b)*c  : H_assoc
        ...   = (b*a)*c  : H_comm
        ...   = b*(a*c)  : H_assoc

    theorem right_comm : ∀a b c, (a*b)*c = (a*c)*b :=
    take a b c, calc
      (a*b)*c = a*(b*c) : H_assoc
        ...   = a*(c*b) : H_comm
        ...   = (a*c)*b : H_assoc
  end

  context
    variable {A : Type}
    variable {f : A → A → A}
    variable H_assoc : associative f
    infixl `*` := f
    theorem assoc4helper (a b c d) : (a*b)*(c*d) = a*((b*c)*d) :=
    calc
      (a*b)*(c*d) = a*(b*(c*d)) : H_assoc
              ... = a*((b*c)*d) : H_assoc
  end

end binary
