data Person = Person { name::String, age::Integer, favouriteCourse::String }
elena, peter, pol :: Person
elena = Person {name="Elena", age=33, favouriteCourse="Functional Programming"}
peter = Person {name="Peter", age=57, favouriteCourse="Imperative Programming"}
pol  = Person {name="Pol", age=36, favouriteCourse="Object Oriented Programming"}

persons = [elena, peter, pol]

increaseAge2 :: [Person]
increaseAge2 = map (\(Person n a p) -> Person n (a+2) p) persons

promote :: [Person]
promote = map (\(Person n a p) -> Person ("dr. " ++ n) a p) persons

fritz :: [Person]
fritz = filter (\(Person n _ _) -> n == "Fritz") persons

twenties :: [Person]
twenties = filter (\p -> let a = age p in 30 > a && a >= 20) persons

average :: Integer
average = sum (map age persons) `div` toInteger (length persons)

promoteF :: [Person]
promoteF = map (\(Person n a p) -> Person ("dr. " ++ n) a p) (filter (\p -> favouriteCourse p == "Functional Programming") persons)