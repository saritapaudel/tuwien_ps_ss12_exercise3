import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import System.IO
import System.Directory
import System.Exit
import System.Environment
import Data.Char
import Control.Monad.Error
import Data.Functor
import Data.Time
import System.Console.Haskeline

type DB = Map CourseName (Map RegName RegData)
data RegData = RegData { timespan :: Timespan
                       , constraints :: [Constraint]
                       , students :: [Student]
                       } deriving (Read,Show)
type CourseName = String
type RegName = String
type Timespan = (Day,Day)
data Constraint = Require RegName
                | RequireOneOf [RegName] 
                | Forbid RegName 
                deriving (Read,Show,Eq)
type Student = String

addCourse :: CourseName -> DB -> DB
addCourse = flip Map.insert Map.empty

removeCourse :: CourseName -> DB -> DB
removeCourse = Map.delete

addReg :: RegName -> Timespan -> CourseName -> DB -> DB
addReg r t = Map.adjust (Map.insert r (RegData t [] []))

removeReg :: RegName -> CourseName -> DB -> DB
removeReg r = Map.adjust (Map.delete r)

getReg :: RegName -> CourseName -> DB -> Maybe RegData
getReg r c db = Map.lookup c db >>= Map.lookup r

getConstraints :: RegName -> CourseName -> DB -> [Constraint]
getConstraints r c db = fromMaybe [] $ constraints <$> getReg r c db

addConstraint :: Constraint -> RegName -> CourseName -> DB -> DB
addConstraint c r = Map.adjust (Map.adjust addCon r)    
    where addCon (RegData t cs ss) = RegData t (c:cs) ss

removeConstraint :: Constraint -> RegName -> CourseName -> DB -> DB
removeConstraint c r = Map.adjust (Map.adjust removeCon r)
    where removeCon (RegData t cs ss) = RegData t (delete c cs) ss

testConstraint :: Constraint -> [RegName] -> Bool
testConstraint (Require r)       = elem r
testConstraint (RequireOneOf rs) = not . null . intersect rs
testConstraint (Forbid r)        = notElem r

-- the resulting list contains all violated constraints
applyConstraints :: [RegName] -> [Constraint] -> [Constraint]
applyConstraints rs = filter (not . flip testConstraint rs)

getStudents :: RegName -> CourseName -> DB -> [Student]
getStudents r c db = fromMaybe [] $ students <$> getReg r c db

allStudents :: DB -> [Student]
allStudents = nub . concat . outerFold
            where outerFold = Map.foldr innerFold []
                  innerFold = flip $ Map.foldr concatStudents
                  concatStudents = (:) . students

studentRegs :: Student -> DB -> [(CourseName,[RegName])]
studentRegs s db = Map.assocs $ Map.filter (not . null) $ 
                   Map.map (Map.keys . Map.filter (elem s . students)) db

isRegistered :: Student -> RegName -> CourseName -> DB -> Bool
isRegistered s r c db = fromMaybe False $ do regs <- Map.lookup c db
                                             reg <- Map.lookup r regs
                                             return (s `elem` students reg)

-- does not do any constraint checks
addStudent :: Student -> RegName -> CourseName -> DB -> DB
addStudent s r = Map.adjust (Map.adjust addStud r)
    where addStud (RegData t cs ss) = RegData t cs (s:ss)

removeStudent :: Student -> RegName -> CourseName -> DB -> DB
removeStudent s r = Map.adjust (Map.adjust removeStud r)
    where removeStud (RegData t cs ss) = RegData t cs (delete s ss)

------------------------------------------------------------------------------

data Cmd = ListCourses 
         | AddCourse CourseName 
         | RemoveCourse CourseName
         | AddReg CourseName RegName Timespan
         | RemoveReg CourseName RegName 
         | ListCons CourseName RegName
         | AddCon CourseName RegName Constraint
         | RemoveCon CourseName RegName Constraint
         | AllStudents
         | ListStudents CourseName RegName
         | ListStudentRegs Student 
         | IsRegistered Student CourseName RegName
         | Register Student CourseName RegName
         | Unregister Student CourseName RegName
         | Help
         | Quit
         | Dump
         deriving (Read,Show)

main = do db <- loadDB
          putStrLn "for a list of commands, type Help"
          runInputT defaultSettings $ runStateT (runErrorT loop) db

loop :: ErrorT String (StateT DB (InputT IO)) ()
loop = (parse >>= eval) `catchError` (liftIO . putStrLn) >> loop

parse :: ErrorT String (StateT DB (InputT IO)) Cmd
parse = do line <- lift $ lift $ getInputLine "> "
           case (listToMaybe . reads . fromMaybe "") line of
              Nothing -> throwError "invalid input (try Help)"
              Just (cmd,_) -> return cmd

eval :: Cmd -> ErrorT String (StateT DB (InputT IO)) ()

eval (ListCourses) = do 
    crs <- gets (Map.assocs . Map.map Map.assocs)
    liftIO $ forM_ crs $ \(c,rs) -> do
        putStrLn ("\n" ++ c ++ ":")
        forM_ rs $ \(r, RegData t cs ss) -> do
            putStr ("  " ++ r)
            putStr (' ':show t)            
            putStr (" [" ++ show (length ss) ++ " registered]")
            case length cs of
                0 -> putChar '\n'
                1 -> putStrLn " [1 constraint]"
                n -> putStrLn (" [" ++ show n ++ " constraints]")

eval (AddCourse c) = modify (addCourse c)

eval (RemoveCourse c) = do assertCourse c
                           modify (removeCourse c)

eval (AddReg c r t) = do assertCourse c
                         modify (addReg r t c)

eval (RemoveReg c r) = do assertCourseAndReg c r
                          modify (removeReg r c)

eval (ListCons c r) = do assertCourseAndReg c r
                         cons <- gets (getConstraints r c)
                         liftIO $ mapM_ print cons

eval (AddCon c r con) = do assertCourseAndReg c r
                           modify (addConstraint con r c)

eval (RemoveCon c r con) = do
    assertCourseAndReg c r
    cons <- gets (getConstraints r c)
    assert (not . null $ cons) "unknown constraint"
    modify (removeConstraint con r c)

eval (AllStudents) = do ss <- gets allStudents
                        liftIO $ mapM_ putStrLn ss

eval (ListStudents c r) = do assertCourseAndReg c r
                             ss <- gets (getStudents r c)
                             liftIO $ mapM_ putStrLn ss

eval (ListStudentRegs s) = do crs <- gets (studentRegs s)
                              liftIO $ forM_ crs $ \(c,rs) -> do
                                putStrLn (c ++ ": ")
                                forM_ rs $ \r -> putStrLn ("  " ++ r)

eval (Register s c r) = do 
    assertCourseAndReg c r
    regs <- gets (fromMaybe [] . lookup c . studentRegs s)
    cons <- gets (getConstraints r c)
    let vcons = applyConstraints regs cons
    if null vcons then modify (addStudent s r c)
                  else liftIO $ do 
                    putStrLn "could not register because of the following constraints:"
                    forM_ vcons $ \vc -> putStrLn ("  " ++ show vc)

eval (Unregister s c r) = do assertCourseAndReg c r
                             modify (removeStudent s r c)

eval (IsRegistered s c r) = do assertCourseAndReg c r
                               isReg <- gets (isRegistered s r c)
                               liftIO $ print isReg

eval (Quit) = do db <- get
                 liftIO $ saveDB db
                 liftIO exitSuccess

eval (Dump) = do db <- get
                 liftIO $ print db

eval (Help) = liftIO $ putStrLn "Available commands:\n\
    \  \n\
    \  ListCourses\n\
    \  AddCourse \"<course>\"\n\
    \  RemoveCourse \"<course>\"\n\
    \  \n\
    \  AddReg \"<course>\" \"<reg>\" (<begin>,<end>)\n\
    \        where <begin> and <end> are of the form yyyy-mm-dd\n\
    \  RemoveReg \"<course>\" \"<reg>\"\n\
    \  \n\
    \  ListCons \"<course>\" \"<reg>\"\n\
    \  AddCon \"<course>\" \"<reg>\" (<constraint>)\n\
    \  RemoveCon \"<course>\" \"<reg>\" (<constraint>)\n\
    \      where <constraint> is one of the following:\n\
    \          Require \"<reg>\"\n\
    \          RequireOneOf [\"<reg1>\",\"<reg2>\",...]\n\
    \          Forbid \"<reg>\"\n\
    \  \n\
    \  AllStudents\n\
    \  ListStudents \"<course>\" \"<reg>\"\n\
    \  ListStudentRegs \"<student>\"\n\
    \  IsRegistered \"<student>\" \"<course>\" \"<reg>\"\n\
    \  Register \"<student>\" \"<course>\" \"<reg>\"\n\
    \  Unregister \"<student>\" \"<course>\" \"<reg>\"\n\
    \  \n\
    \  Help\n\
    \  Quit"

assert :: (MonadError e m) => Bool -> e -> m ()
assert b e = unless b (throwError e)

assertCourse :: Monad m => CourseName -> ErrorT String (StateT DB m) ()
assertCourse c = do isCourse <- gets (Map.member c)
                    assert isCourse ("unknown course: " ++ c)

assertCourseAndReg :: Monad m => CourseName -> RegName -> ErrorT String (StateT DB m) ()
assertCourseAndReg c r = do regs <- gets (Map.lookup c)
                            case regs of
                                Nothing -> throwError ("unknown course: " ++ c)
                                Just rs -> assert (Map.member r rs) 
                                                  ("unknown registration: " ++ r)

dbFilePath :: IO FilePath
dbFilePath = do args <- getArgs
                if null args then return "db.data"
                else return (head args)

loadDB :: IO DB
loadDB = do path <- dbFilePath
            e <- doesFileExist path
            if e then read <$> readFile path
                 else return Map.empty

saveDB :: DB -> IO ()
saveDB db = do path <- dbFilePath
               let s = show db
               writeFile path s
