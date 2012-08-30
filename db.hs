import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Either

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

db :: Courses
db = Map.fromList [("FFP", Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans", "peter"]),("Abgabegespräch", RegData (0,0) [] ["fritz", "peter"])]),("FP",Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans", "franz"])])]

addCourse :: CourseName -> Courses -> Courses
addCourse cname = Map.insert cname Map.empty

addRegistration :: RegName -> Timespan -> CourseName -> Courses -> Courses
addRegistration rname tspan cname cs = 
    Map.adjust (Map.insert rname (RegData tspan [] [])) cname cs

addConstraint :: Constraint -> RegName -> CourseName -> Courses -> Courses
addConstraint con rname cname =
    Map.adjust (Map.adjust addCon rname) cname
    where addCon (RegData t cs ss) = RegData t (con:cs) ss

allStudents :: Courses -> [Student]
allStudents = nub . concat . outerFold
            where outerFold = Map.foldr innerFold []
                  innerFold = flip $ (Map.foldr concatStudents)
                  concatStudents = (:) . students

isRegistered :: Student -> RegName -> CourseName -> Courses -> Bool
isRegistered s rname cname cs = fromMaybe False $ do
    regs <- Map.lookup cname cs
    reg <- Map.lookup rname regs
    return (s `elem` (students reg))

--registerStudent :: Student -> RegName -> CourseName -> Courses -> ()

violatedConstraints :: [Constraint] -> [RegName] -> [Constraint]
violatedConstraints cs rs = filter (\c -> not $ testConstraint c rs) cs

testConstraint :: Constraint -> [RegName] -> Bool
testConstraint (RequireOneOf rs) = not . null . (intersect rs)
testConstraint (Forbid r) = notElem r


