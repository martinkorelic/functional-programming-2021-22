module Triangle where

triangle :: Int -> String
triangle i = triangleR i i 0

triangleR :: Int -> Int -> Int -> String
triangleR _ 0 e = ""
triangleR n i e = triangleR n (i-1) e ++ replicate (e+n-i) ' ' ++ replicate (2*i-1) '*' ++ ['\n']

christmasTree :: Int -> String
christmasTree i = christmasTreeR i i

christmasTreeR :: Int -> Int -> String
christmasTreeR _ 0 = ""
christmasTreeR n i = christmasTreeR n (i-1) ++ triangleR i i (n-i)