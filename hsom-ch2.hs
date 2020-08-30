import Euterpea

-- Concrete ii V I example rewritten in g Major
g251 :: Music Pitch
g251 = let aMinor = a 3 wn :=: c 4 wn :=: e 4 wn
           dMajor = d 4 wn :=: fs 4 wn :=: a 4 wn
           gMajor = g 3 bn :=: b 4 bn :=: d 4 bn
       in  aMinor :+: dMajor :+: gMajor
 

-- Exercise for twoFiveOne generally

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let secondMinor = minorChord (trans 2 p) d
                     fifthMajor = majorChord (trans 7 p) d
                     tonicMajor = majorChord p (d * 2)
                 in
                     secondMinor :+: fifthMajor :+: tonicMajor

minorChord :: Pitch -> Dur -> Music Pitch
minorChord p d = note d p :=: note d (trans 3 p) :=: fifth p d

majorChord :: Pitch -> Dur -> Music Pitch
majorChord p d = note d p :=: note d (trans 4 p) :=: fifth p d

fifth :: Pitch -> Dur -> Music Pitch
fifth p d = note d (trans 7 p)

{- Proof By calculation of the twoFiveOne (G, 4) wn  = g251
twoFiveOne (G, 3) wn = let secondMinor = minorChord (trans 2 p) wn
                           fifthMajor = majorChord (trans 7 p) wn
                           tonicMajor = mahjorChord (G, 3) (wn * 2)
                       in secondMinor :+: fifthMajor :+: tonicMajor

After deestructuring the functions we have the following variables applied to the context =>

secondMinor (A, 3) wn = minorChord (A, 3) wn = note wn a :=: note wn (trans 3 a) :=: note wn (trans 7 p)
secondMinor (A, 3) wn = a 3 wn :=: c 3 wn :=: e 4 wn
secondMinor (A, 3) = aMinor in g241

fifthMajor (d, 4) wn = majorChord (D, 4) wn = note wn (D, 4) :=: note wn (trans 4 (D, 4)) :=: note wn (trans 7 (D, 4)
fifthMajor (D, 4) wn = (D, 4) wn :=: (Fs, 4) wn :=: (A, 4) wn
fifthMajor (D, 4) wn = dMajor in g251

tonicMajor (G, 3) (wn * 2)= majorChord (G, 3) bn = note bn (G, 3) :=: note bn (trans 4 (G, 3)) :=: note bn (trans 7 (G, 3))
tonicMajor (G, 3) (wn * 2) = (G, 3) bn :=: (B, 4) bn :=: (D, 4) bn
tonicMajor (G, 3) (wn * 2) = gMajor in g251

Switching the variables we conclude that
twofiveOne (G, 3) wn = aMinor :=: dMajor :=: gMajor


-}
