import Data.List (break)

-- zipper for list
type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, x:bs) = (x:xs, bs)


{- file system -}
-- data structures
type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:crumbs) = (Folder name (ls ++ item:rs), crumbs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo toName (Folder name items, crumbs) =
  let (ls, item:rs) = break (nameIs toName) items
  in (item, FSCrumb name ls rs:crumbs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder fname _) = name == fname
nameIs name (File fname _) = name == fname

-- sample data
myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa"
    , File "pope_time.avi" "god bless"
    , Folder "pics"
      [ File "ape_throwing_up.jpg" "bleargh"
      , File "watermelon_smash.gif" "smash!!"
      , File "skull_man(scary).bmp" "Yikes!" ]
    , File "dijon_poupon.doc" "best mustard"
    , Folder "programs"
        [ File "fartwizard.exe" "10gotofart"
        , File "owl_bandit.dmg" "mov eax, h00t"
        , File "not_a_virus.exe" "really not a virus"
          , Folder "source code"
          [ File "best_hs_prog.hs" "main = print (fix error)" , File "random.hs" "main = print 4"
          ]
        ]
    ]
