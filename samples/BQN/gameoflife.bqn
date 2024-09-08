# Larger initial matrix
r ← 15‿35 ↑ ¯10‿¯20 ↑ (3‿3 ⥊ ↕9) ∊ 1‿2‿3‿4‿7

# Iteration function
Life ← {∨´ 1‿𝕩 ∧ 3‿4 = +˘˝ +˝ 1‿0‿¯1 ⌽⌜ 1‿0‿¯1 ⌽˘¨ <𝕩}

# 'animate' 100 iterations of the game of life
{ i ← Life 𝕩 ⋄ •Delay 1 ⋄ •Show i ⊏ "·#" ⋄ i}⍟100 r
