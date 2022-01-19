addOddsOnEven :: [Int] -> Int
addOddsOnEven ls = sum [ x | (x,i) <- zip ls [0..], even i && odd x ]

addOddsOnEven' :: [Int] -> Int
addOddsOnEven' ls = addOddsOnEven'' (length ls) 0 ls

addOddsOnEven'' :: Int -> Int -> [Int] -> Int
addOddsOnEven'' sum i [] = sum
addOddsOnEven'' sum i (x:xs) = if even i && odd x then addOddsOnEven'' (i-1) (sum+x) xs else addOddsOnEven'' (i-1) sum xs