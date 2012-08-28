import System.IO
import Control.Monad.State

type Student = String

type DB = [Student]

addStudent :: Student -> DB -> ((),DB)
addStudent s db = ((),s:db)

printStudents :: DB -> IO ()
printStudents = mapM_ putStrLn

main = do
    let db = []
    loop db
    
loop :: DB -> IO ()
loop db = do
    putStr "> "
    hFlush stdout
    cmd <- getLine
    case cmd of
        "quit" -> return ()
        "show students" -> do
            printStudents db
            loop db
        "add student" -> do
            let (_,db2) = addStudent "fritz" db
            loop db2
        otherwise -> loop db
