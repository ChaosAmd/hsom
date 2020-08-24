import Euterpea

p1 = (Ef, 4)
p2 = (F, 4)
p3 = (G, 4)


mel = (note qn p1 :=: note qn (trans (-3) p1)) :+:
      (note qn p2 :=: note qn (trans (-3) p2)) :+:
      (note qn p3 :=: note qn (trans (-3) p3))

hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d(trans (-3) p)

mel1 :: Music Pitch
mel1 = hNote qn p1 :+: hNote qn p2 :+: hNote qn p3

mel2 :: Music Pitch
mel2 = let hNote' d p = note d p :=: note d(trans (-3) p)
       in hNote' qn p1 :+: hNote' qn p2 :+: hNote' qn p3


hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0
hList d (p:ps) = hNote d p :+: hList d ps

mel3 = hList qn [p1, p2, p3]

thNote :: Dur -> Pitch -> Int -> Music Pitch
thNote d p t = note d p :=: note d (trans t p)

thList :: Dur -> [Pitch] -> [Int] -> Music Pitch
thList d [] [] = rest 0
thList d (p:ps) (t:ts) = thNote d p t :+: thList d ps ts


pBach = [(G, 3), (Fs, 3), (B, 3)]
tBach = [9, 12, 8]
mel4 = thList qn pBach tBach