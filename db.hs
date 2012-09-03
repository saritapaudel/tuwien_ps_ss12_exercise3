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

type DB = Map CourseName (Map RegName RegData)
data RegData = RegData { timespan :: Timespan
                       , constraints :: [Constraint]
                       , students :: [Student]
                       } deriving (Read,Show)
type CourseName = String
type RegName = String
type Timespan = (Int,Int) -- (UTCTime,UTCTime) TODO
data Constraint = RequireOneOf [RegName] 
                | Forbid RegName deriving (Read,Show,Eq)
type Student = String

testdb :: DB
testdb = Map.fromList [("FFP", Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans", "peter"]),("Abgabegespräch", RegData (0,0) [RequireOneOf ["Kursanmeldung"]] ["fritz", "peter"])]),("FP",Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans", "franz"])])]

-- courses

courseNames :: DB -> [CourseName]
courseNames = Map.keys

addCourse :: CourseName -> DB -> DB
addCourse = flip Map.insert Map.empty

removeCourse :: CourseName -> DB -> DB
removeCourse = Map.delete

-- registrations

regNames :: CourseName -> DB -> [RegName]
regNames c = Map.keys . fromMaybe Map.empty . Map.lookup c

addReg :: RegName -> Timespan -> CourseName -> DB -> DB
addReg r t = Map.adjust (Map.insert r (RegData t [] []))

removeReg :: RegName -> CourseName -> DB -> DB
removeReg r = Map.adjust (Map.delete r)

getReg :: RegName -> CourseName -> DB -> Maybe RegData
getReg r c db = Map.lookup c db >>= Map.lookup r

-- constraints

getConstraints :: RegName -> CourseName -> DB -> [Constraint]
getConstraints r c db = fromMaybe [] $ constraints <$> getReg r c db

addConstraint :: Constraint -> RegName -> CourseName -> DB -> DB
addConstraint c r = Map.adjust (Map.adjust addCon r)    
    where addCon (RegData t cs ss) = RegData t (c:cs) ss

removeConstraint :: Constraint -> RegName -> CourseName -> DB -> DB
removeConstraint c r = Map.adjust (Map.adjust removeCon r)
    where removeCon (RegData t cs ss) = RegData t (delete c cs) ss

testConstraint :: Constraint -> [RegName] -> Bool
testConstraint (RequireOneOf rs) = not . null . intersect rs
testConstraint (Forbid r)        = notElem r

-- the resulting list contains all violated constraints
applyConstraints :: [RegName] -> [Constraint] -> [Constraint]
applyConstraints rs = filter (not . flip testConstraint rs)

-- students

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

-- I/O

-- TODO: comment as much as possible (make it literate?)

data Cmd = ListCourses 
         | AddCourse CourseName 
         | RemoveCourse CourseName
         | ListRegs CourseName 
         | ShowReg CourseName RegName
         | AddReg CourseName RegName Timespan
         | RemoveReg CourseName RegName 
         | AddCon CourseName RegName Constraint
         | RemoveCon CourseName RegName Constraint
         | ListStudents 
         | ListStudentRegs Student 
         | IsRegistered Student CourseName RegName
         | RegisterStudent Student CourseName RegName
         | UnregisterStudent Student CourseName RegName
         | Help
         | Quit
         | Debug
         deriving (Read,Show)

helpString = 
    "Available commands:\n\
    \  \n\
    \  ListCourses\n\
    \  AddCourse \"<course>\"\n\
    \  RemoveCourse \"<course>\"\n\
    \  \n\
    \  ListRegs \"<course>\"\n\
    \  ShowReg \"<course>\" \"<reg>\"\n\
    \  AddReg \"<course>\" \"<reg>\" (<begin>,<end>)\n\
    \        where <begin> and <end> are of the form \"yyyy-mm-dd hh:mm:ss\"\n\
    \  RemoveReg \"<course>\" \"<reg>\"\n\
    \  \n\
    \  AddCon \"<course>\" \"<reg>\" <constraint>\n\
    \  RemoveCon \"<course>\" \"<reg>\" <constraint>\n\
    \      where <constraint> is one of the following:\n\
    \          RequireOneOf [\"<reg1>\",\"<reg2>\",...]\n\
    \          Forbid \"<reg>\"\n\
    \  \n\
    \  ListStudents\n\
    \  ListStudentRegs \"<student>\"\n\
    \  IsRegistered \"<student>\" \"<course>\" \"<reg>\"\n\
    \  Register \"<student>\" \"<course>\" \"<reg>\"\n\
    \  Unregister \"<student>\" \"<course>\" \"<reg>\"\n\
    \  Help\n\
    \  Quit"

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

main = do db <- loadDB
          runStateT (runErrorT loop) db


newtype App a = App { runApp :: ErrorT String (StateT DB IO) a }


loop :: ErrorT String (StateT DB IO) ()
loop = (parse >>= eval >> loop) `catchError` handler
    where handler e = do liftIO $ putStrLn e
                         loop

parse :: ErrorT String (StateT DB IO) Cmd
parse = do liftIO $ putStr "> "
           liftIO $ hFlush stdout
           line <- liftIO getLine            
           case maybeRead line of
              Nothing -> throwError "invalid input (try Help)"
              Just (cmd,_) -> return cmd

maybeRead :: Read a => String -> Maybe (a,String)
maybeRead = listToMaybe . reads

assert :: (MonadError e m) => Bool -> e -> m ()
assert b e = unless b (throwError e)

assertCourse :: CourseName -> ErrorT String (StateT DB IO) ()
assertCourse c = do isCourse <- gets (Map.member c)
                    assert isCourse ("unknown course: " ++ c)

assertCourseAndReg :: CourseName -> RegName -> ErrorT String (StateT DB IO) ()
assertCourseAndReg c r = do regs <- gets (Map.lookup c)
                            case regs of
                                Nothing -> throwError ("unknown course: " ++ c)
                                Just rs -> assert (Map.member r rs) 
                                                  ("unknown registration: " ++ r)

eval :: Cmd -> ErrorT String (StateT DB IO) ()

eval (ListCourses) = do cs <- gets courseNames
                        liftIO $ mapM_ putStrLn cs

eval (AddCourse c) = modify (addCourse c)

eval (RemoveCourse c) = do assertCourse c
                           modify (removeCourse c)

eval (ListRegs c) = do assertCourse c
                       rs <- gets (regNames c)
                       liftIO $ mapM_ putStrLn rs

eval (ShowReg c r) = do assertCourseAndReg c r
                        Just (RegData t cs ss) <- gets (getReg r c)
                        liftIO $ do putStrLn $ "Timespan: " ++ show t
                                    putStrLn $ "Constraints: " ++ show cs
                                    putStrLn $ "Registered Students: " ++ show ss

eval (AddReg c r t) = do assertCourse c
                         modify (addReg r t c)

eval (RemoveReg c r) = do assertCourse c
                          modify (removeReg r c)

eval (AddCon c r con) = do assertCourseAndReg c r
                           modify (addConstraint con r c)

eval (RemoveCon c r con) = do
    assertCourseAndReg c r
    cons <- gets (constraints . fromJust . Map.lookup r . fromJust . Map.lookup c)
    assert (not . null $ cons) "unknown constraint"
    modify (removeConstraint con r c)

eval (ListStudents) = do ss <- gets allStudents
                         liftIO $ mapM_ putStrLn ss

eval (ListStudentRegs s) = do rs <- gets (studentRegs s)
                              liftIO $ print rs

eval (RegisterStudent s c r) = do 
    assertCourseAndReg c r
    regs <- gets (fromMaybe [] . lookup c . studentRegs s)
    cons <- gets (getConstraints r c)
    let vcons = applyConstraints regs cons
    if null vcons then modify (addStudent s r c)
                  else do liftIO $ putStrLn "violated constraints:"
                          liftIO $ print vcons

eval (UnregisterStudent s c r) = do 
    assertCourseAndReg c r
    isReg <- gets (isRegistered s r c)
    assert isReg (s ++ " is not registered for " ++ c ++ " " ++ r)
    modify (removeStudent s r c)

eval (IsRegistered s c r) = do assertCourseAndReg c r
                               isReg <- gets (isRegistered s r c)
                               liftIO $ print isReg

eval (Quit) = do db <- get
                 liftIO $ saveDB db
                 liftIO exitSuccess

eval (Help) = liftIO $ putStrLn helpString

eval (Debug) = do db <- get
                  liftIO $ print db
