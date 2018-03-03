module Reals where

  -- (a set with properties of) the reals
  data ℝ : Set where
    r0 : ℝ
    r1 : ℝ
    _+_ : ℝ → ℝ → ℝ

  -- equality
  data _==_ : ℝ → ℝ → Set where
    AXrefl== : ∀ {r} → r == r
    AXsymm== : ∀ {r s} → r == s → s == r
    AXtrans== : ∀ {r s t} → r == s → s == t → r == t
    AX+0 : ∀ {r} → (r + r0) == r
    AXsymm+ : ∀ {r s} → (r + s) == (s + r)
    AX+== : ∀ {r s t} → r == s → (r + t) == (s + t)

  THM0+ : {r : ℝ} → r == (r0 + r)
  THM0+ = AXsymm== (AXtrans== AXsymm+ AX+0)
  -- AXsymm+ AX+0   r0 + r == r + r0 and r + r0 == r
  -- AXtrans==      so r0 + r == r
  -- AXsymm==       so r == r0 + r

  THM0+alt : {r : ℝ} → r == (r0 + r)
  THM0+alt {r} = AXsymm== {r0 + r} {r} ((AXtrans== {r0 + r} {r + r0} {r}) (AXsymm+ {r0} {r}) (AX+0 {r}))

  -- strict partial ordering
  data _<_ : ℝ → ℝ → Set where
    AXtrans<<< : ∀ {r s t} → r < s → s < t → r < t
    AX<=< : ∀ {r s t} → r < s → s == t → r < t
    AX=<< : ∀ {r s t} → r == s → s < t → r < t
    AX0<1 : r0 < r1
    AX+<< : ∀ {r s t} → r < s → (r + t) < (s + t)

  THM<+1 : {r : ℝ} → r < (r + r1)
  THM<+1 = AX<=< (AX=<< THM0+ (AX+<< AX0<1)) AXsymm+
  -- AX0<1              0 < 1
  -- AX<+ %             so 0 + r < 1 + r
  -- AX=<< lem0+ %      so r < 1 + r
  -- AX<=< % AXsymm+    so r < r + 1
