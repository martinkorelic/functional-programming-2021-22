module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")

-- 1
martin, nick :: Person
martin = ("Martin", 22, "Functional Programming")
nick = ("Nick", 22, "Functional Programming")

students :: [Person]
students = [elena, peter, pol, martin, nick]

-- 2
age :: Person -> Age
age (_, n, _)  =  n

name :: Person -> Name
name (n, _, _) = n

favouriteCourse  :: Person -> FavouriteCourse
favouriteCourse (_,_,n) = n

-- 3
showPerson       :: Person -> String
showPerson (j,k,l) = show j ++ show k ++ show l

-- 4
twins            :: Person -> Person -> Bool
twins p1 p2 = age p1 == age p2

-- 5
increaseAge      :: Person -> Person
increaseAge (m,n,k) = (m,n+1,k)

-- 6

-- increment the age of all students by two
incrementTwo = let increment2 p = (age p+2, name p) in map increment2 students

-- promote all of the students (prefix"dr. "to their name)
promote = let promote1 p = ("dr. " ++ name p) in map promote1 students

-- find all students named Frits
frits = let frits1 p = name p == "Frits" in filter frits1 students

-- find all students who are in their twenties
twenties = let twenties1 p = age p < 30 && age p >= 20 in filter twenties1 students

-- compute the average age of all students
accAge = let ages p = toInteger (age p) in map ages students

averageAge =  total `div` toInteger (length students) where total = sum accAge

-- promote the students whose favourite course is Functional Programming
fpPromote = let pr p = "dr. " ++ name p in map pr (let fpPromote1 p = favouriteCourse p == "Functional Programming" in filter fpPromote1 students)