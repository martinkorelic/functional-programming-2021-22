module Exercise2 where

{-

1.

my_fun = double (length [length [0]])

double (length [length [0]])
= { def. length }
double (length [1 + length []])
=
double (length [1 + 0])
=
double (length [1])
=
double (1 + length [])
=
double (1 + 0)
=
double 1
=
1 + 1
=
2

-}

{-

2.

double (length [length [0]])
=
length [length [0]] + length [length [0]]
=
(1 + length []) + length [length [0]]
=
(1 + 0) + length [length [0]]
=
1 + length [length [0]]
=
1 + (1 + length [])
=
1 + (1 + 0)
=
1 + 1
=
2

-}

{-

3.

double (length [length [0]])
= let x = length [length [0]] in x + x
= let x = 1 + length [] in x + x
= let x = 1 + 0 in x + x
= let x = 1 in x + x
= 1 + 1
= 2

-}