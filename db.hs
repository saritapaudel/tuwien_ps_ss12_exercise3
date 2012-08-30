import Data.Map (Map)
import qualified Data.Map as Map

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
db = Map.fromList [("FFP", Map.fromList [("Kursanmeldung", RegData (0,0) [] ["hans"])])]

addCourse :: CourseName -> Courses -> Courses
addCourse cname = Map.insert cname Map.empty

addRegistration :: RegName -> Timespan -> CourseName -> Courses -> Courses
addRegistration rname tspan cname cs = 
    Map.adjust (Map.insert rname (RegData tspan [] [])) cname cs

