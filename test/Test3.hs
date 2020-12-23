module Test3 (tests3) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.Hedgehog as HH
import Test.Tasty.HUnit

import Part3

tests3 :: [TestTree]
tests3 =
  [ test18
  , test19
  , test20
  , test21
  , test22
  , test23
  , test24
  , test25
  , test26
  , test27
  , test28
  , test29
  , test30
  , test31
  , test32
  ]

test18 :: TestTree
test18 = testGroup "P18"
  [ testCase "prob18 17 == T" $ prob18 17 @?= True
  , testCase "prob18 1 == F" $ prob18 1 @?= False
  , testCase "prob18 479001599 == T" $ prob18 479001599 @?= True
  , testCase "prob18 2971215073 == T" $ prob18 2971215073 @?= True
  , testCase "prob18 2971215073 == F" $ prob18 8971215073 @?= False
  ]

test19 :: TestTree
test19 = testGroup "P19"
  [ testCase "prob19 19 == [(19,1)]" $ prob19 19 @?= [(19,1)]
  , testCase "prob19 1 == []" $ prob19 1 @?= []
  , testCase "prob19 2020 == [(2,2),(5,1),(101,1)]" $
    prob19 2020 @?= [(2,2),(5,1),(101,1)]
  ]

test20 :: TestTree
test20 = testGroup "P20"
  [ testCase "prob20 6 == T" $ prob20 6 @?= True
  , testCase "prob20 8 == F" $ prob20 8 @?= False
  , testCase "prob20 1 == F" $ prob20 1 @?= False
  , testCase "prob20 33550336 == T" $ prob20 33550336 @?= True
  , testCase "prob20 8589869056 == T" $ prob20 8589869056 @?= True
  , testCase "prob20 10 ^ 10 == F" $ prob20 (10 ^ 10) @?= False
  ]

test21 :: TestTree
test21 = testGroup "P21"
  [ testCase "prob21 6 == [1,2,3,6]" $ prob21 6 @?= [1,2,3,6]
  , testCase "prob21 7 == [1,7]" $ prob21 7 @?= [1,7]
  , testCase "prob21 1 == [1]" $ prob21 1 @?= [1]
  , testCase "prob21 1048576 == [1, 2, 4, .. , 524288, 1048576]" $ 
    prob21 1048576 @?= [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384,
                        32768, 65536, 131072, 262144, 524288, 1048576]
  , testCase "prob21 9246432350 == [1, 2, 5, .. , 4623216175, 9246432350]" $ 
    prob21 9246432350 @?= [1, 2, 5, 10, 25, 50, 2393, 4786, 11965, 23930, 59825, 77279, 119650,
                           154558, 386395, 772790, 1931975, 3863950, 184928647, 369857294, 924643235, 
                           1849286470, 4623216175, 9246432350]
  ]

test22 :: TestTree
test22 = testGroup "P22"
  [ testCase "prob22 \"parti iii izi\" == 6" $
    prob22 "parti iii izi" @?= 6
  , testCase "prob22 \"I have no letters\" == 0" $
    prob22 "I have no letters" @?= 0
  , testCase "prob22 \"no letters\" == 0 " $
    prob22 "no letters" @?= 0
  , testCase "prob22 \"\" == 0 " $
    prob22 "" @?= 0
  , testCase "prob22 \"i\" == 1 " $
    prob22 "i" @?= 1
  , testCase "prob22 \"iii\" == 3 " $
    prob22 "iii" @?= 3
  , testCase "prob22 \"iii iii\" == 9 " $
    prob22 "iii iii" @?= 9
  , testCase "prob22 \"iii\n\tiii\n\ti0i\" == 18 " $
    prob22 "iii\n\tiii\n\ti0i" @?= 18
  , testCase "prob22 \"\0105\0105 \0105\0105i\" == 6 " $
    prob22 "\0105\0105 \0105\0105i" @?= 6
  ]

test23 :: TestTree
test23 = testGroup "P23"
  [ testCase "prob23 \"1-4: hello world!\" == \"hell\"" $
    prob23 "1-4: hello world!" @?= Just "hell"
  , testCase "prob23 \"10-11:  2345678901\" == \"01\"" $
    prob23 "10-11:  2345678901" @?= Just "01"
  , testCase "prob23 \"100-120: 1\" == Nothing" $
    prob23 "100-120: 1" @?= Nothing
  , testCase "prob23 \"5-3: abcde\" == \"edc\"" $
    prob23 "5-3: abcde" @?= Just "edc"
  ]

test24 :: TestTree
test24 = testGroup "P24"
  [ testCase "prob24 1 == T" $ prob24 1 @?= True
  , testCase "prob24 2 == F" $ prob24 2 @?= False
  , testCase "prob24 6 == T" $ prob24 6 @?= True
  , testCase "prob24 15 == T" $ prob24 15 @?= True
  , testCase "prob24 666 == T" $ prob24 666 @?= True
  , testCase "prob24 1830 == T" $ prob24 1830 @?= True
  , testCase "prob24 500500 == T" $ prob24 500500 @?= True
  , testCase "prob24 200010000 == T" $ prob24 200010000 @?= True
  , testCase "prob24 50000 * 50001 `div` 2 == T" $ prob24 (50000 * 50001 `div` 2) @?= True
  ]

test25 :: TestTree
test25 = testGroup "P25"
  [ testCase "prob25 9 == T" $ prob25 9 @?= True
  , testCase "prob25 0 T" $ prob25 0 @?= True
  , testCase "prob25 10 == F" $ prob25 10 @?= False
  , testCase "prob25 101 == T" $ prob25 101 @?= True
  , testCase "prob25 101..101 == T" $ prob25 1010101010101 @?= True
  , testCase "prob25 906609 == T" $ prob25 906609 @?= True
  ]

test26 :: TestTree
test26 = testGroup "P26"
  [ testCase "prob26 1 2 == F"         $ prob26 1     2     @?= False
  , testCase "prob26 6 6 == T"         $ prob26 6     6     @?= True
  , testCase "prob26 220 284 == T"     $ prob26 220   284   @?= True
  , testCase "prob26 1184 1210 == T"   $ prob26 1184  1210  @?= True
  , testCase "prob26 2620 2924 == T"   $ prob26 2620  2924  @?= True
  , testCase "prob26 10744 10856 == T" $ prob26 10744 10856 @?= True
  , testCase "prob26 10744 10856 == T" $ prob26 69615 87633 @?= True
  , testCase "prob26 79750 88730 == T" $ prob26 79750 88730 @?= True
  ]

test27 :: TestTree
test27 = testGroup "P27"
  [ testCase "prob27 5 [1,2,3] == (2,3)" $
    prob27 5 [1,2,3] @?= Just (2,3)
  , testCase "prob27 5 [2,1,2] == N" $
    prob27 5 [2,1,2] @?= Nothing
  , testCase "prob27 4 [2,1,2] == (2,2)" $
    prob27 4 [2,1,2] @?= Just (2,2)
  , testCase "prob27 4 [1,2,1] == N" $
    prob27 4 [1,2,1] @?= Nothing
  , testCase "prob27 501 [1..500] == (1, 500)" $
    prob27 501 [1..500] @?= Just (1, 500)
  , testCase "prob27 1999 [1..1000] == (999, 1000)" $
    prob27 1999 [1..1000] @?= Just (999, 1000)
  , testCase "prob27 3 [1..1000] == (1, 2)" $
    prob27 3 [1..1000] @?= Just (1, 2)
  ]

test28 :: TestTree
test28 = testGroup "P28"
  [ testCase "prob28 5 [3,1,1,1,2] == (2,1,1,1)" $
    prob28 5 [3,1,1,1,2] @?= Just (2,1,1,1)
  , testCase "prob28 5 [3,2,1,1,5] == N" $
    prob28 5 [3,2,1,1,5] @?= Nothing
  , testCase "prob28 1994 [1..500] == (500, 499, 498, 497)" $
    prob28 1994 [1..500] @?= Just (500, 499, 498, 497)
  , testCase "prob28 3994 [1..1000] == (1000, 999, 998, 997)" $
    prob28 3994 [1..1000] @?= Just (1000, 999, 998, 997)
  , testCase "prob28 10 [1..1000] == (4, 3, 2, 1)" $
    prob28 10 [1..1000] @?= Just (4, 3, 2, 1)
  , testCase "prob28 10^10 [1..1000] == Nothing" $
    prob28 (10^10) [1..1000] @?= Nothing
  ]

test29 :: TestTree
test29 = testGroup "P29"
  [ testCase "prob29 1 == 9"        $ prob29 1 @?= 9
  , testCase "prob29 2 == 9009"     $ prob29 2 @?= 9009
  , testCase "prob29 3 == 906609"   $ prob29 3 @?= 906609
  ]

test30 :: TestTree
test30 = testGroup "P30"
  [ testCase "prob30 4 == 6"  $ prob30 4 @?= 6
  , testCase "prob30 5 == 28" $ prob30 5 @?= 28
  ]

test31 :: TestTree
test31 = testGroup "P31"
  [ testCase "prob31 250 == 0"        $ prob31 250   @?= 0
  , testCase "prob31 300 == 504"      $ prob31 300   @?= 504
  , testCase "prob31 10000 == 31626"  $ prob31 10000 @?= 31626
  , testCase "prob31 20000 == 115818" $ prob31 20000 @?= 115818
  ]

test32 :: TestTree
test32 = testGroup "P32"
  [ testCase "prob32 [2,3,5] 10 == [5+5, 5+3+2, 3+3+2+2, 2+2+2+2+2]" $
    prob32 [2,3,5] 10 @?= [[5,5],[5,3,2],[3,3,2,2],[2,2,2,2,2]]
  , testCase "prob32 [2,3,5] 50 == [5+..+5, .. ,2+..+2]" $
    prob32 [2,3,5] 50 @?= [
        [5,5,5,5,5,5,5,5,5,5],
        [5,5,5,5,5,5,5,5,5,3,2],
        [5,5,5,5,5,5,5,5,3,3,2,2],
        [5,5,5,5,5,5,5,5,2,2,2,2,2],
        [5,5,5,5,5,5,5,3,3,3,3,3],
        [5,5,5,5,5,5,5,3,3,3,2,2,2],
        [5,5,5,5,5,5,5,3,2,2,2,2,2,2],
        [5,5,5,5,5,5,3,3,3,3,3,3,2],
        [5,5,5,5,5,5,3,3,3,3,2,2,2,2],
        [5,5,5,5,5,5,3,3,2,2,2,2,2,2,2],
        [5,5,5,5,5,5,2,2,2,2,2,2,2,2,2,2],
        [5,5,5,5,5,3,3,3,3,3,3,3,2,2],
        [5,5,5,5,5,3,3,3,3,3,2,2,2,2,2],
        [5,5,5,5,5,3,3,3,2,2,2,2,2,2,2,2],
        [5,5,5,5,5,3,2,2,2,2,2,2,2,2,2,2,2],
        [5,5,5,5,3,3,3,3,3,3,3,3,3,3],
        [5,5,5,5,3,3,3,3,3,3,3,3,2,2,2],
        [5,5,5,5,3,3,3,3,3,3,2,2,2,2,2,2],
        [5,5,5,5,3,3,3,3,2,2,2,2,2,2,2,2,2],
        [5,5,5,5,3,3,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,5,5,5,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,5,5,3,3,3,3,3,3,3,3,3,3,3,2],
        [5,5,5,3,3,3,3,3,3,3,3,3,2,2,2,2],
        [5,5,5,3,3,3,3,3,3,3,2,2,2,2,2,2,2],
        [5,5,5,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2],
        [5,5,5,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,5,5,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,5,3,3,3,3,3,3,3,3,3,3,3,3,2,2],
        [5,5,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2],
        [5,5,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2],
        [5,5,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2],
        [5,5,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,5,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,5,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3],
        [5,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2],
        [5,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2],
        [5,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2],
        [5,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [5,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2],
        [3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2],
        [3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2],
        [3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2],
        [3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2],
        [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]]
   ]