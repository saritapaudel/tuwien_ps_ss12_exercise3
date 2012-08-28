import System.IO
import Control.Monad.State
import Data.List

type Student = String

type DB = [Student]

addStudent :: Monad m => Student -> StateT DB m ()
addStudent s = do
    db <- get
    let db' = s:db
    put db'

removeStudent :: Monad m => Student -> StateT DB m ()
removeStudent s = do
    db <- get
    let db' = delete s db
    put db'

getStudents :: Monad m => StateT DB m [Student]
getStudents = do
    db <- get
    return db

main = do
    let db = ["fritz"]
    execStateT loop db
    return ()

loop :: StateT DB IO ()
loop = do
    db <- get
    liftIO $ putStr "> "
    liftIO $ hFlush stdout
    cmd <- liftIO getLine
    case cmd of
        "quit" -> return ()
        "show students" -> do
            ss <- getStudents
            liftIO $ mapM_ putStrLn ss
            loop
        "add student" -> do
            addStudent "fritz"
            loop
        "remove student" -> do
            removeStudent "fritz"
            loop
        otherwise -> loop
