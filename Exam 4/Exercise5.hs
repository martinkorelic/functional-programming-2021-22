insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x ys = x : ys
insertAt _ _ [] = error "list too short"
insertAt i x (y:ys) = y : insertAt (i-1) x ys

{-

a) No error
b) Error
c) Error
d) No error

-}

--deleteAt i (insertAt i x xs) = xs
deleteAt :: (Eq t, Num t) => t -> [a] -> [a]
deleteAt 0 (y:ys) = ys
deleteAt _ [] = error "list too short"
deleteAt i (y:ys) = y : deleteAt (i-1) ys


{-

Induction:

Case 1: i_1 = 0

insertAt i_1 x_1 (insertAt i_2 x_2 xs)
=
x_1 : (insertAt i_2 x_2 xs)
=
insertAt (i_2+1) x_2 (x_1 : xs)
=
insertAt (i_2+1) x_2 (insertAt i_1 x_1 xs)

Case 2: i_1 > 0.
This implies that i_2 > 0

case 2a: xs=[]
insertAti_1x_1 (insertAt i_2 x_2 [])
=
insertAt i_1 x_1 (error "list too short")
=
error "list too short"
=
insertAt (i_2+1) x_2 (error "list too short")
=
insertAt (i_2+1) x_2 (insertAt i_1 x_1 xs)

case 2b: xs=y:ys
insertAt i_1 x_1 (insertAt i_2 x_2 (y:ys))
=
insertAt i_1 x_1 (y :insertAt (i_2-1) x_2 ys)
=
y :insertAt (i_1-1) x_1 (insertAt (i_2-1) x_2 ys)
= {IH}
y :insertAt i_2 x_2 (insertAt (i_1-1) x_1 ys)
=
insertAt (i_2+1) x_2 (y :insertAt (i_1-1) x_1 ys)
=
insertAt (i_2+1) x_2 (insertAt i_1 x_1 (y:ys))

-}