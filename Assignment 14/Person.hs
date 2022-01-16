{-# LANGUAGE RankNTypes #-}
module Person where

import Lenses
import Data.Monoid

data Person = Person { name::String, age::Integer, favouriteCourse::String }
  deriving (Eq,Show)

students :: [Person]
students = [elena,peter,pol,sjaak,frits,twan,marc]

elena, peter, pol, sjaak, frits, twan, marc :: Person
elena = Person "Elena" 33 "Functional Programming"
peter = Person "Peter" 57 "Imperative Programming"
pol   = Person "Pol"   36 "Object Oriented Programming"
sjaak = Person "Sjaak" 26 "Software Verification"
frits = Person "Frits" 61 "Functional Programming"
twan  = Person "Twan"  21 "Category Theory"
marc  = Person "Marc"  41 "Mathematics"

pretty :: Person -> String
pretty p = name p ++ " (" ++ show (age p) ++ "), likes " ++ favouriteCourse p

name' :: Lens Person String
name' f p = (\x->p{ name=x }) <$> f (name p)

age' :: Lens Person Integer
age' f p = (\x->p{ age=x }) <$> f (age p)

favCourse' :: Lens Person String
favCourse' f p = (\x->p{ favouriteCourse=x }) <$> f (favouriteCourse p)

sumOf :: (Num a) => Traversal s a -> s -> a 
sumOf trav = getSum . foldMapOf trav Sum

meanOf :: (Fractional n, Integral a) => Traversal s a -> s -> n
meanOf trav x = fromIntegral (sumOf trav x) / fromIntegral (lengthOf trav x)

-- use mapOf and a lens to increase the age of Frits by 2
frits2 = mapOf age' (+2) frits

--increment the age of each student by two 
expr1 = mapOf (each.age') (+2) students 

--promote all students 
expr2 = mapOf (each.name') ("dr."++) students

--promote all students named frits 
expr3 = mapOf (each.name'.when (=="Frits")) ("dr."++) students

--average age of students that like FP
expr4 = meanOf (each.when(\p->favouriteCourse p=="Functional Programming").age') students

--promote all students that like FP 
expr5 = mapOf (each.when(\p->favouriteCourse p=="Functional Programming").name') ("dr."++) students 