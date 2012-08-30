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

violatedConstraints :: [Constraint] -> [RegName] -> [Constraint]
violatedConstraints cs rs = filter (\c -> not $ testConstraint c rs) cs

testConstraint :: Constraint -> [RegName] -> Bool
testConstraint (RequireOneOf rs) = not . null . intersect rs
testConstraint (Forbid r) = notElem r

-- I/O

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
        "courses" -> do
            cnames <- liftM courseNames get
            liftIO $ mapM_ putStrLn cnames
        "addCourse" -> case maybeRead args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (cname,_) -> modify (addCourse cname)
        "removeCourse" -> case maybeRead args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (cname,_) -> modify (removeCourse cname)        

        "regs" -> case maybeRead args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (cname,_) -> do
                rnames <- liftM (regNames cname) get
                liftIO $ mapM_ putStrLn rnames
        "addReg" -> case parse_rname_tspan_cname args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (rname,tspan,cname) -> modify (addReg rname tspan cname)
        "removeReg" -> case parse_rname_cname args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (rname,cname) -> modify (removeReg rname cname)

        "addCon" -> case parse_con_rname_cname args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (con,rname,cname) -> modify (addConstraint con rname cname)

        "students" -> do
            students <- liftM allStudents get
            liftIO $ mapM_ putStrLn students
        "studentRegs" -> case maybeRead args of
            Nothing -> liftIO $ putStrLn "invalid input"
            Just (student,_) -> do
                regs <- liftM (studentRegs student) get
                liftIO $ print regs                

        "debug" -> do
            state <- get
            liftIO $ print state
        "quit" -> liftIO exitSuccess -- TODO: save db
        otherwise -> return ()

maybeRead :: Read a => String -> Maybe (a,String)
maybeRead = listToMaybe . reads

parse_rname_tspan_cname :: String -> Maybe (RegName, Timespan, CourseName)
parse_rname_tspan_cname args = do
    (rname,arg2) <- maybeRead args
    (tspan,arg3) <- maybeRead arg2
    (cname,_)    <- maybeRead arg3
    return (rname,tspan,cname)

parse_rname_cname :: String -> Maybe (RegName, CourseName)
parse_rname_cname args = do
    (rname,arg2) <- maybeRead args
    (cname,_)    <- maybeRead arg2
    return (rname,cname)

parse_con_rname_cname :: String -> Maybe (Constraint, RegName, CourseName)
parse_con_rname_cname args = do
    (con,arg2)   <- maybeRead args
    (rname,arg3) <- maybeRead arg2
    (cname,_)    <- maybeRead arg3
    return (con,rname,cname)


