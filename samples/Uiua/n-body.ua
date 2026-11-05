# ? G VelPos
Step ← (
  # Calc velocity deltas
  ⤙(×⊙(
      ˙⊞- ≡⊣         # Differences
      ≡▽˙⊞≠°⊏        # Remove self
      ≡/+÷⊃(≡₁/+˙×)± # Accs
    ))
  ⍜≡⊢+ # Update velocities
  ≡\+  # Update positions
)
# ? Steps G Starts
Simulate ← ⍥⊃⟜Step⋅≡⊣ ⊙⊙(≡⊟0)
Render   ← ⬚0≡°⊚⁅ -⊸(¤¤/↧♭₂)

Render Simulate 120 20 [0_60 150_0 30_15 80_110]
≡⌞⧈(/↥♭)3_3 # Upscale

&fwa "n-body.gif" gif 16
&p "Wrote gif to n-body.gif"
