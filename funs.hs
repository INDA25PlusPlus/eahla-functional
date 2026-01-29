-- fibonacci n
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2) 


-- reverse list
loop [] = []
loop li =
    let nli = loop (tail li)
    nli ++ [last li]


-- median ord
import Data.List

main = do
    let ord = ["hej", "hejdå", "wut", "wutwut"] -- 5
    let lens = get_lens ord
    let lens = sort lens
    print (med lens)

-- längd
len [] = 0
len x = len (tail x) + 1

--
get_lens [] = []
get_lens [x] = [len x]
get_lens li = get_lens ([head li]) ++ get_lens (tail li)

--helper
helper li = init li

-- sorted nums to median
med [] = -1
med [x] = x 
med [x, y] = (x + y) / 2
med li = med (helper (tail li))
