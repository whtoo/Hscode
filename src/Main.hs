module Main where
fibs ::  Int -> Int
fibs 0  = 0
fibs 1  = 1
fibs n  = fibs(n-1) + fibs(n-2)

add :: Int -> Int -> Int
add x y = x + y

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y:insert x ys
                
insertsort2 :: Ord a => [a] -> [a]
insertsort2 [] = []
insertsort2 (x:xs) = insert x (insertsort2 xs)


main::IO()
main = putStrLn "Hi,my lamd"