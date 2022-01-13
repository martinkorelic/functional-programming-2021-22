maxlist [] = 0
maxlist xs = foldl max xs

{- 
Non type-variable argument in the constraint.
There is no inferred knowledge that the list that is being fold is numeric.
He might have intended to write a maximum function.
-}

prepend xs [] = xs
prepend xs ys = xs : ys

{-
Cannot construct the infinite type: a ~ [a].
Function at one point returning a, and then in the second one [a].
Prepending an element to the list, and the element if the list is empty.
-}

double f = f f
{-
Cannot construct the infinite type: t ~ t -> t1
The output from one function to the input of another does not match.
Using two functions in a row.
-}