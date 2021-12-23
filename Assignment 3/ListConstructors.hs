module ListConstructors where

{-

1.

xs0 = [1,2,3] ++ 4:[5]
xs0 = [1,2,3,4,5]
[Integer]

xs1 = [1:2:[3],[4,5]]
xs1 = [[1,2,3],[4,5]]
[[Integer]]

xs2 = "abc"
xs2 = ['a','b','c']
[Char] or String

xs3 = []
[a]

xs4 = [[],[]]
xs4 = [[],[]]
[[a]]

xs5 = [[]:[]]
xs5 = [[]]
[[a]]

xs6 = [[]++[]]
xs6 = [[]]
[[a]]

xs7 = [[[]]]
xs7 = [[[]]]
[[[a]]]


2.

xs0 = 1:2:3:4:5:[]
xs1 = (1:2:3:[]):(4:5:[]):[]
xs2 = 'a':'b':'c':[]
xs3 = []
xs4 = ([]):[]
xs5 = ([]):[]
xs6 = ([]):[]
xs7 = (([]):[]):[]

3.

The list constructor is made of numeric types.
-}