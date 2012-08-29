import Data.List
import Data.Time



type DB = [Course]
data Course = C CourseName [Registration] deriving (Show)
type CourseName = String
data Registration = R RegName Timespan [Constraint] [Student] deriving (Show)
type RegName = String
type Timespan = (Int,Int) --(UTCTime,UTCTime)
data Constraint = RequiredAny [RegName] | Disallowed RegName deriving (Show)
type Student = String

testDB :: DB
testDB = [C "FFP" [R "Kursanmeldung" (0,0) [] []]]


addRegistration :: RegName -> Timespan -> CourseName -> DB -> DB
addRegistration rname tspan cname cs = 
    case find (\(C name _) -> name == cname) cs of
        Nothing -> cs
        Just (C name regs) -> cs'
            where cs' = updateCourse c' cs
                  c'  = C name (reg:regs)
                  reg = R rname tspan [] []

removeRegistration :: RegName -> CourseName -> DB -> DB
removeRegistration rname cname cs =
    case find (\(C name _) -> name == cname) cs of
        Nothing -> cs
        Just (C name regs) -> cs'
            where cs'   = updateCourse c' cs
                  c'    = C name regs'
                  regs' = removeReg rname regs
                  removeReg n1 rs

updateCourse :: Course -> [Course] -> [Course]
updateCourse _ [] = []
updateCourse (C name' regs') ((C name regs):cs)
    | name' == name = (C name regs') : cs
    | otherwise     = (C name regs) : updateCourse (C name' regs') cs