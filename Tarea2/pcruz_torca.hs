pcruz a b
  | length a == 3 && length b == 3 =
    [ a !! 1 * b !! 2 - a !! 2 * b !! 1
    , a !! 2 * b !! 0 - a !! 0 * b !! 2
    , a !! 0 * b !! 1 - a !! 1 * b !! 0
    ]
  | otherwise = error "A o B no es un vector tridimensional"