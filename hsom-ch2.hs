import Euterpea

-- Concrete ii V I example rewritten in g Major
g251 :: Music Pitch
g251 = let aMinor = a 3 wn :=: c 4 wn :=: e 4 wn
           dMajor = d 4 wn :=: fs 4 wn :=: a 4 wn
           gMajor = g 4 bn :=: b 4 bn :=: d 5 bn
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
