import System.IO
import Control.Monad.State
import Data.List
import Data.Char

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
    case splitWords 2 cmd of
        ("quit":_) -> return ()
        ("show":"students":_) -> do
            ss <- getStudents
            liftIO $ mapM_ putStrLn ss
            loop
        ("add":"student":arg:_) -> do
            addStudent $ read arg
            loop
        ("remove":"student":arg:_) -> do
            removeStudent $ read arg
            loop
        otherwise -> loop

splitWords :: Int -> String -> [String]
splitWords 0 s = [s]
splitWords n s = w : splitWords (n-1) ws
    where (w,ws) = (break isSpace . dropWhile isSpace) s




