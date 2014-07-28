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

swapall2 :: Ord a => [a] -> [a]
swapall2 [] = []
swapall2 [x] = [x]
swapall2 (x1:x2:xs) | x1 > x2 = x2 : swapall2(x1:xs)
                    | otherwise = x1 : swapall2(x2:xs)
                    
swapSort :: Ord a => [a] -> [a]
swapSort [] = []
swapSort xs = swapSort restlst ++ [lastItem]
                where tmplst = swapall2 xs
                      restlst = init tmplst
                      lastItem = last tmplst
                      
delForSort :: Ord a =>a -> [a] -> [a]
delForSort _ [] = []                      
delForSort key (x:xs) | key == x = xs
                         | otherwise = x : delForSort key xs

selectsort :: Ord a => [a]-> [a]
selectsort [] = []
selectsort xs = selected : selectsort restlst
                where selected = minimum xs
                      restlst = delForSort selected xs
                      
quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []                     
quicksort' (x:xs) =  quicksort' minVal ++ [x]++ quicksort' maxVal
                where minVal = filter (<x) xs
                      maxVal = filter (>=x) xs

merge1 :: Ord a => [a] -> [a] -> [a]
merge1 [] xs = xs
merge1 ys [] = ys 
merge1 (y:ys) (x:xs) | y < x = y : merge1 ys (x:xs)
                     | otherwise = x : merge1 (y:ys) xs   

                          
main::IO()
main = putStrLn "Hi,my lamd"