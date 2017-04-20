\documentclass{article}

 % The following packages are needed because unicode
 % is translated (using the next set of packages) to
 % latex commands. You may need more packages if you
 % use more unicode characters:

 \usepackage{amssymb}
 \usepackage{bbm}
 \usepackage[greek,english]{babel}

 % This handles the translation of unicode to latex:

 \usepackage{ucs}
 \usepackage[utf8x]{inputenc}
 \usepackage{autofe}

 % Some characters that are not automatically defined
 % (you figure out by the latex compilation errors you get),
 % and you need to define:

 \DeclareUnicodeCharacter{8988}{\ensuremath{\ulcorner}}
 \DeclareUnicodeCharacter{8989}{\ensuremath{\urcorner}}
 \DeclareUnicodeCharacter{8803}{\ensuremath{\overline{\equiv}}}

 % Add more as you need them (shouldn’t happen often).

 % Using “\newenvironment” to redefine verbatim to
 % be called “code” doesn’t always work properly. 
 % You can more reliably use:

 \usepackage{fancyvrb}

 \DefineVerbatimEnvironment
   {code}{Verbatim}
   {} % Add fancy options here if you like.

 \begin{document}

 \begin{code}
module NatCat where

open import Relation.Binary.PropositionalEquality

-- If you can show that a relation only ever has one inhabitant
-- you get the category laws for free
module
  EasyCategory
  (obj : Set)
  (_⟶_ : obj → obj → Set)
  (_∘_ : ∀ {x y z} → x ⟶ y → y ⟶ z → x ⟶ z)
  (id : ∀ x → x ⟶ x)
  (single-inhabitant : (x y : obj) (r s : x ⟶ y) → r ≡ s)
  where

  idʳ : ∀ x y (r : x ⟶ y) → r ∘ id y ≡ r
  idʳ x y r = single-inhabitant x y (r ∘ id y) r 

  idˡ : ∀ x y (r : x ⟶ y) → id x ∘ r ≡ r
  idˡ x y r = single-inhabitant x y (id x ∘ r) r

  ∘-assoc : ∀ w x y z (r : w ⟶ x) (s : x ⟶ y) (t : y ⟶ z) → (r ∘ s) ∘ t ≡ r ∘ (s ∘ t)
  ∘-assoc w x y z r s t = single-inhabitant w z ((r ∘ s) ∘ t) (r ∘ (s ∘ t))

open import Data.Nat

same : (x y : ℕ) (r s : x ≤ y) → r ≡ s
same .0 y z≤n z≤n = refl
same .(suc m) .(suc n) (s≤s {m} {n} r) (s≤s s) = cong s≤s (same m n r s)

≤-trans : ∀ x y z → x ≤ y → y ≤ z → x ≤ z
≤-trans .0 y z z≤n s = z≤n
≤-trans .(suc m) .(suc n) .(suc n₁) (s≤s {m} {n} r) (s≤s {.n} {n₁} s) = s≤s (≤-trans m n n₁ r s)

≤-refl : ∀ x → x ≤ x
≤-refl zero = z≤n
≤-refl (suc x) = s≤s (≤-refl x)

module Nat-EasyCategory = EasyCategory ℕ _≤_ (λ {x}{y}{z} → ≤-trans x y z) ≤-refl same
 \end{code}

 \end{document} 