main = do
    in0 <- getLine
    st <- getLine
    print (svara st 2)

svara :: String -> Int -> Int
svara [] n = min 2 n
svara [x] n =
    if x == 'l' || x == 'v' 
        then 1 
        else 2
svara [x, y] n =
    if x == 'l' && y == 'v' 
        then 0 
        else (min (svara [x] n) (svara [y] n))
-- svara (x:xs) n = min (svara [x] n) (svara xs n)
svara li n = min (svara ([head li, head (tail li)]) 2) (svara (tail li) 2)

-- ChatGPT hjälpte till lite här men jag har redan gjort läxan klart, så jag får använda AI jättemycket nu

-- https://open.kattis.com/submissions/19058688
