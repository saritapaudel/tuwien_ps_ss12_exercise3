type Student = String

data DB = DB { students :: [Student] }

addStudent :: Student -> DB -> DB
addStudent s db = DB { students = s:(students db) }

load :: DB
load = DB ["Hans","Peter","Fritz"]

main = do
    let db = load
    let db2 = addStudent "Maria" db
    mapM_ putStrLn $ students db2