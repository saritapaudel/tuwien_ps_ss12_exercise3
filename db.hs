import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import System.IO
import System.Exit
import Data.Char
import Control.Monad.Error

type Courses = Map CourseName Registrations
type Registrations = Map RegName RegData
data RegData = RegData { timespan :: Timespan
                       , constraints :: [Constraint]
                       , students :: [Student]
                       } deriving (Show)
type CourseName = String
type RegName = String
type Timespan = (Int,Int) -- (UTCTime,UTCTime)
data Constraint = RequireOneOf [RegName] | Forbid RegName deriving (Read,Show)
type Student = String

testdb :: Courses
testdb = Map.fromList [("FFP", Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans", "peter"]),("Abgabegespräch", RegData (0,0) [] ["fritz", "peter"])]),("FP",Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans", "franz"])])]

-- courses

courseNames :: Courses -> [CourseName]
courseNames = Map.keys

addCourse :: CourseName -> Courses -> Courses
addCourse = flip Map.insert Map.empty

removeCourse :: CourseName -> Courses -> Courses
removeCourse = Map.delete

-- registrations

regNames :: CourseName -> Courses -> [RegName]
regNames cname = Map.keys . fromMaybe Map.empty . Map.lookup cname

addReg :: RegName -> Timespan -> CourseName -> Courses -> Courses
addReg rname tspan = Map.adjust (Map.insert rname (RegData tspan [] []))

removeReg :: RegName -> CourseName -> Courses -> Courses
removeReg rname = Map.adjust (Map.delete rname)

-- constraints

addConstraint :: Constraint -> RegName -> CourseName -> Courses -> Courses
addConstraint con rname = Map.adjust (Map.adjust addCon rname)    
    where addCon (RegData t cs ss) = RegData t (con:cs) ss

violatedConstraints :: [Constraint] -> [RegName] -> [Constraint]
violatedConstraints cs rs = filter (\c -> not $ testConstraint c rs) cs

testConstraint :: Constraint -> [RegName] -> Bool
testConstraint (RequireOneOf rs) = not . null . intersect rs
testConstraint (Forbid r) = notElem r

-- students

allStudents :: Courses -> [Student]
allStudents = nub . concat . outerFold
            where outerFold = Map.foldr innerFold []
                  innerFold = flip $ Map.foldr concatStudents
                  concatStudents = (:) . students

studentRegs :: Student -> Courses -> [(CourseName,[RegName])]
studentRegs s cs = Map.assocs $ Map.filter (not . null) $ 
                   Map.map (Map.keys . Map.filter (elem s . students)) cs

isRegistered :: Student -> RegName -> CourseName -> Courses -> Bool
isRegistered s rname cname cs = fromMaybe False $ do
    regs <- Map.lookup cname cs
    reg <- Map.lookup rname regs
    return (s `elem` students reg)

registerStudent :: Student -> RegName -> CourseName -> Courses -> Either [Constraint] Courses
registerStudent s rname cname cs = 
    if null vcons then Right (registerStudent' s) else Left vcons
    where vcons = violatedConstraints (constraints reg) otherRegsOfStudent
          reg = fromMaybe (RegData undefined [] []) $ Map.lookup rname regs
          otherRegsOfStudent = Map.keys $ Map.filter (\x -> s `elem` students x) regs
          regs = fromMaybe Map.empty $ Map.lookup cname cs
          registerStudent' s = Map.adjust (Map.adjust addStud rname) cname cs
          addStud (RegData t cs ss) = RegData t cs (s:ss)

-- I/O

--loop :: StateT Courses IO ()
--loop = do
--    liftIO $ hSetBuffering stdin LineBuffering -- for GHCi
--    liftIO $ putStr "> "
--    liftIO $ hFlush stdout
--    line <- liftIO getLine
--    let (cmd,args) = break isSpace line
--    case cmd of
--        "courses" -> do
--            cnames <- liftM courseNames get
--            liftIO $ mapM_ putStrLn cnames
--        "addCourse" -> do
--            case parse (pString "course name") args of
--                Left msg -> liftIO $ putStrLn msg
--                Right (cname,_) -> modify (addCourse cname)
--        "removeCourse" -> do
--            courses <- get
--            case parse (pCourse courses) args of
--                Left msg -> liftIO $ putStrLn msg
--                Right (cname,_) -> modify (removeCourse cname)

--        "regs" -> case maybeRead args of
--            Nothing -> liftIO $ putStrLn "invalid input"
--            Just (cname,_) -> do
--                rnames <- liftM (regNames cname) get
--                liftIO $ mapM_ putStrLn rnames
--        "addReg" -> case parse_rname_tspan_cname args of
--            Nothing -> liftIO $ putStrLn "invalid input"
--            Just (rname,tspan,cname) -> modify (addReg rname tspan cname)
--        "removeReg" -> case parse_rname_cname args of
--            Nothing -> liftIO $ putStrLn "invalid input"
--            Just (rname,cname) -> modify (removeReg rname cname)

--        "addCon" -> case parse_con_rname_cname args of
--            Nothing -> liftIO $ putStrLn "invalid input"
--            Just (con,rname,cname) -> modify (addConstraint con rname cname)

--        "students" -> do
--            students <- liftM allStudents get
--            liftIO $ mapM_ putStrLn students
--        "studentRegs" -> case maybeRead args of
--            Nothing -> liftIO $ putStrLn "invalid input"
--            Just (student,_) -> do
--                regs <- liftM (studentRegs student) get
--                liftIO $ print regs

--        "registerStudent" -> case parse_stud_rname_cname args of
--            Nothing -> liftIO $ putStrLn "invalid input"
--            Just (stud,rname,cname) -> do
--                db <- get
--                case registerStudent stud rname cname db of
--                    Left vcons -> do
--                        liftIO $ putStrLn "error: violates constraints:"
--                        liftIO $ mapM_ print vcons
--                    Right db' -> put db'

--        "debug" -> do
--            state <- get
--            liftIO $ print state
--        "quit" -> do
--            liftIO $ hSetBuffering stdin NoBuffering -- for GHCi
--            liftIO exitSuccess -- TODO: save db
--        otherwise -> return ()

maybeRead :: Read a => String -> Maybe (a,String)
maybeRead = listToMaybe . reads



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

loop :: ErrorT String (StateT Courses IO) ()
loop = (parse >>= eval >> loop) `catchError` handler
    where handler e = do liftIO $ putStrLn e
                         loop

parse :: ErrorT String (StateT Courses IO) Cmd
parse = do liftIO $ putStr "> "
           liftIO $ hFlush stdout
           line <- liftIO getLine            
           case maybeRead line of
              Nothing -> throwError "invalid input (try Help)"
              Just (cmd,_) -> return cmd

assert :: (MonadError e m) => Bool -> e -> m ()
assert b e = unless b (throwError e)

eval :: Cmd -> ErrorT String (StateT Courses IO) ()

eval (ShowCourses)         = do cnames <- gets courseNames
                                liftIO $ mapM_ putStrLn cnames

eval (AddCourse cname)     = do modify (addCourse cname)

eval (RemoveCourse cname)  = do isCourse <- gets (Map.member cname)
                                assert isCourse ("unknown course: " ++ cname)
                                modify (removeCourse cname)

eval (ShowRegs cname)      = do isCourse <- gets (Map.member cname)
                                assert isCourse ("unknown course: " ++ cname)
                                rnames <- gets (regNames cname)
                                liftIO $ mapM_ putStrLn rnames
--eval (AddReg CourseName RegName Timespan)
--eval (RemoveReg CourseName RegName)
--eval (AddCon CourseName RegName Constraint)
--eval (ShowStudents)
--eval (ShowStudentRegs Student)
--eval (RegisterStudent CourseName RegName Student)
eval (Quit) = do liftIO exitSuccess -- TODO: save db
eval (Debug) = do state <- get
                  liftIO $ print state
eval (Help) = do liftIO $ putStrLn helpString

