
primos :: [Integer]

primos = colar[2..]

colar(p:ps) = p : colar[x | x <- ps, x `mod` p > 0]