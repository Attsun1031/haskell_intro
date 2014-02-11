-- 最短距離計算
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

-- サンプルデータ
heathrow2Rondon :: RoadSystem
heathrow2Rondon = [ Section 50 10 30
                  , Section 5 90 20
                  , Section 40 2 25
                  , Section 10 8 0
                  ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
    then reverse bestAPath
    else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let timeA = sum (map snd pathA)
      timeB = sum (map snd pathB)
      forwardTime2A = timeA + a
      crossTime2A = timeB + b + c
      forwardTime2B = timeB + b
      crossTime2B = timeA + a + c
      newPath2A = if forwardTime2A <= crossTime2A
                    then (A, a):pathA
                    else (C, c):(B, b):pathB
      newPath2B = if forwardTime2B <= crossTime2B
                    then (B, b):pathB
                    else (C, c):(A, a):pathA
  in (newPath2A, newPath2B)
