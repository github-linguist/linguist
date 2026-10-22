# A Markov chain is a model that assigns a
# probability to each token that comes after
# the previous token.
# It can be used to generate patterns that
# look a bit like the source text.

# Try running it a few times!

# Build the Markov chain
# Chain ? TokenLen Input
Build ← (
  ⬚@\0⊃(↘-1⟜⧈∘|↻) ⊙(⊂⊙@\n⊂@\n)
  ∧(⍜(°□⬚""get)⊂ ⊙˜⊙∘)⊙⊙[]
)
Initial ↚ ⊏⌊×⚂⊸⧻ ▽⊸(≡⊢=@\n)⊙◌°map
Next ↚ (
  ⊏⊸(⌊×⚂⧻) °□get           # Next char
  ⍥↘₁⊸(↥⊃(×⁅÷2⚂=@\n|=@\0)) # End condition
)
# Generate some text from a Markov chain
# ? Init TokenSize Chain
GenFrom ← (
  ⊙˜⊙∘ (⨬⋅⊸Initial∘±⊸⧻) ⊙˜⊙∘
  ↘¯1↘1⊙⋅◌⍥⊃(⊂⟜(Next˜↙⊙¯))⋅⊙∘∞
)

$ Hello there!
$ How are you doing today?
$ I am fine, thank you.
$ That is good to hear.
$ Nice weather we're having today isn't it?
$ Yes, it's been very mild.
&p GenFrom "" ⟜Build 2
