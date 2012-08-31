import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import System.IO
import System.Exit
import Data.Char
import Control.Monad.Error

type DB = Map CourseName (Map RegName RegData)
data RegData = RegData { timespan :: Timespan
                       , constraints :: [Constraint]
                       , students :: [Student]
                       } deriving (Show)
type CourseName = String
type RegName = String
type Timespan = (Int,Int) -- (UTCTime,UTCTime)
data Constraint = RequireOneOf [RegName] 
                | Forbid RegName deriving (Read,Show)
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

-- constraints

getConstraints :: RegName -> CourseName -> DB -> [Constraint]
getConstraints r c db =  fromMaybe [] $ do regs <- Map.lookup c db
                                           reg <- Map.lookup r regs
                                           return (constraints reg)

addConstraint :: Constraint -> RegName -> CourseName -> DB -> DB
addConstraint c r = Map.adjust (Map.adjust addCon r)    
    where addCon (RegData t cs ss) = RegData t (c:cs) ss

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

-- I/O



-- TODO: check if we haven't missed anything
data Cmd = ShowCourses 
         | AddCourse CourseName 
         | RemoveCourse CourseName
         | ShowRegs CourseName 
         | AddReg CourseName RegName Timespan
         | RemoveReg CourseName RegName 
         | AddCon CourseName RegName Constraint
         | ShowStudents 
         | ShowStudentRegs Student 
         | RegisterStudent CourseName RegName Student
         | Quit
         | Debug
         | Help
         deriving (Read,Show)

-- TODO: complete
helpString = 
    "Available commands:\n\
    \  ShowCourses\n\
    \  AddCourse \"<course name>\"\n\
    \  Help"

main = do let db = testdb  -- TODO: load db
          runStateT (runErrorT loop) db

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
eval (ShowCourses)         = do cs <- gets courseNames
                                liftIO $ mapM_ putStrLn cs

eval (AddCourse c)         = do modify (addCourse c)

eval (RemoveCourse c)      = do assertCourse c 
                                modify (removeCourse c)

eval (ShowRegs c)          = do assertCourse c
                                rs <- gets (regNames c)
                                liftIO $ mapM_ putStrLn rs

eval (AddReg c r t)        = do assertCourseAndReg c r
                                modify (addReg r t c)

eval (RemoveReg c r)       = do assertCourse c
                                modify (removeReg r c)

eval (AddCon c r con)      = do assertCourseAndReg c r
                                modify (addConstraint con r c)

eval (ShowStudents)        = do ss <- gets allStudents
                                liftIO $ mapM_ putStrLn ss

eval (ShowStudentRegs s)   = do rs <- gets (studentRegs s)
                                liftIO $ print rs

eval (RegisterStudent c r s) = do 
    assertCourseAndReg c r
    regs <- gets (fromMaybe [] . lookup c . studentRegs s)
    cons <- gets (getConstraints r c)
    let vcons = applyConstraints regs cons
    if null vcons then modify (addStudent s r c)
                  else do liftIO $ putStrLn "violated constraints:"
                          liftIO $ print vcons

eval (Quit)                = do liftIO exitSuccess -- TODO: save db
eval (Debug)               = do state <- get
                                liftIO $ print state
eval (Help)                = do liftIO $ putStrLn helpString

