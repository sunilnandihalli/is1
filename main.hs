-- hello
module Main where
import Data.List as L

stringToInt::String->Int
stringToInt str = read str


getkdistpairs::[Int]->[Int]->Int->[(Int,Int)]
getkdistpairs _ [] _ = []
getkdistpairs (x:xs) (y:ys) k 
    | (x+k)==y = (x,y):getkdistpairs xs ys k
    | (x+k)<y  = getkdistpairs xs (y:ys) k
    | (x+k)>y  = getkdistpairs (x:xs) ys k


countkapart::[Int]->Int->Int
countkapart (v:vs) k = let aux1 = dropWhile (\x -> k > (x-v)) vs
                           aux = getkdistpairs (v:vs) aux1 k      
                       in length aux          
                             


solve::Int->Int->[Int]->Int
solve n k vals = countkapart (L.sort vals) k

main = 
    do nkstr<-getLine
       valsstr<-getLine
       let [n,k] = map stringToInt $ words nkstr
           vals = map stringToInt $ words valsstr
       print $ solve n k vals

