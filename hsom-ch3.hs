-- Exercise 3.3
simple x y z = x * (y + z)

applyEach :: [(a -> b)] -> a -> [b]
applyEach x fns = map ($ x) fns

applyAll :: [(a -> a)] -> a -> a
applyAll = foldl (.) id
