import System.IO
import System.Exit
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
    forever $ execStateT loop db

loop :: StateT DB IO ()
loop = do
    db <- get
    liftIO $ putStr "> "
    liftIO $ hFlush stdout
    line <- liftIO getLine
    let (cmd,args) = break isSpace line
    case cmd of
        "quit" -> do
            liftIO exitSuccess
        "showStudents" -> do
            s <- getStudents
            liftIO $ mapM_ putStrLn s
        "addStudent" -> do
            case readMaybe args of
                Just s -> addStudent s
                Nothing -> liftIO $ putStrLn "invalid input"
        "removeStudent" -> do
            removeStudent $ read args
        "test" -> do
            return ()
        otherwise -> return ()

test :: Monad m => String -> String -> (Int,Int) -> StateT DB m ()
test a b (c,d) = return ()

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing