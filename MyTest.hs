import Test.HUnit

f :: State -> State
f ss = ss'
  where c@Counts{ tried = n } = counts ss
        ss' = ss{ counts = c{ tried = n + 1 } }
