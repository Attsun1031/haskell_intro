-- 型クラス
import YesNo
data Traffic = Red | Yellow | Green

instance Eq Traffic where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show Traffic where
    show Red = "Red Traffic"
    show Green = "Green Traffic"
    show Yellow = "Yellow Traffic"

instance YesNo Traffic where
    yesno Red = False
    yesno _ = True
