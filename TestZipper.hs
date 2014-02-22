import Test.HUnit

test1 = "hoge" ~: TestCase (assertEqual "" 1 1)
test2 = "fuga" ~: TestCase (assertEqual "" 1 2)

main = do
  runTestTT $ TestList [test1, test2]
  return ()
