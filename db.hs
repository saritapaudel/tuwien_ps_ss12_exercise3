import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import System.IO
import System.Exit
import Data.Char

type Courses = Map CourseName Registrations
type Registrations = Map RegName RegData
data RegData = RegData { timespan :: Timespan
                       , constraints :: [Constraint]
                       , students :: [Student]
                       } deriving (Show)
type CourseName = String
type RegName = String
type Timespan = (Int,Int) -- (UTCTime,UTCTime)
data Constraint = RequireOneOf [RegName] | Forbid RegName deriving (Show)
type Student = String

testdb :: Courses
testdb = Map.fromList [("FFP", Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans", "peter"]),("Abgabegespräch", RegData (0,0) [] ["fritz", "peter"])]),("FP",Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans", "franz"])])]

addCourse :: Monad m => CourseName -> StateT Courses m ()
addCourse = modify . flip Map.insert Map.empty

removeCourse :: Monad m => CourseName -> StateT Courses m ()
removeCourse = modify . Map.delete

addRegistration :: Monad m => RegName -> Timespan -> CourseName -> StateT Courses m ()
addRegistration rname tspan cname = 
    modify $ Map.adjust (Map.insert rname (RegData tspan [] [])) cname

removeRegistration :: Monad m => RegName -> CourseName -> StateT Courses m ()
removeRegistration rname cname = 
    modify $ Map.adjust (Map.delete rname) cname

addConstraint :: Constraint -> RegName -> CourseName -> Courses -> Courses
addConstraint con rname cname =
    Map.adjust (Map.adjust addCon rname) cname
    where addCon (RegData t cs ss) = RegData t (con:cs) ss

allStudents :: Courses -> [Student]
allStudents = nub . concat . outerFold
            where outerFold = Map.foldr innerFold []
                  innerFold = flip $ Map.foldr concatStudents
                  concatStudents = (:) . students

isRegistered :: Student -> RegName -> CourseName -> Courses -> Bool
isRegistered s rname cname cs = fromMaybe False $ do
    regs <- Map.lookup cname cs
    reg <- Map.lookup rname regs
    return (s `elem` students reg)

violatedConstraints :: [Constraint] -> [RegName] -> [Constraint]
violatedConstraints cs rs = filter (\c -> not $ testConstraint c rs) cs

testConstraint :: Constraint -> [RegName] -> Bool
testConstraint (RequireOneOf rs) = not . null . intersect rs
testConstraint (Forbid r) = notElem r

main :: IO ()
main = do
    let db = testdb -- TODO: load db
    runStateT (forever loop) db
    return ()

loop :: StateT Courses IO ()
loop = do
    liftIO $ putStr "> "
    liftIO $ hFlush stdout
    line <- liftIO getLine
    let (cmd,args) = break isSpace line
    case cmd of
        "quit" -> liftIO exitSuccess -- TODO: save db
        "students" -> do
            students <- liftM allStudents get
            liftIO $ mapM_ putStrLn students
        "addCourse" -> case maybeRead args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (cname,_) -> addCourse cname
        "removeCourse" -> case maybeRead args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (cname,_) -> removeCourse cname
        "courses" -> do
            courses <- liftM Map.keys get
            liftIO $ mapM_ putStrLn courses
        "addRegistration" -> case maybeRead args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (rname,arg2) -> case maybeRead arg2 of
                Nothing -> liftIO $ putStrLn "invalid input"                
                Just (tspan,arg3) -> case maybeRead arg3 of
                    Nothing -> liftIO $ putStrLn "invalid input"
                    Just (cname,_) -> addRegistration rname tspan cname
        "registrations" -> case maybeRead args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (cname,_) -> do
                courses <- get
                let regs = Map.lookup cname courses
                case regs of
                    Nothing -> liftIO $ putStrLn "course not found"
                    Just reg -> liftIO $ mapM_ putStrLn $ Map.keys reg
        "debug" -> do
            state <- get
            liftIO $ putStrLn $ show state
        otherwise -> return ()

maybeRead :: Read a => String -> Maybe (a,String)
maybeRead = listToMaybe . reads
