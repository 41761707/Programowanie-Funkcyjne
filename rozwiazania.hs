FAJNIEJSZE FUNKCJE WYKŁADY
fact n | n <= 0 = 1 | n == 1 = 1 | otherwise = n*fact (n-1)

Prelude> :{ 
Prelude| mylen [] = 0
Prelude| mylen (x:xs) = 1 + mylen xs
Prelude| :}

Prelude> :{
Prelude| qs [] = []
Prelude| qs (x:xs) = qs[t|t<-xs, t<=x] ++ [x] ++ qs[t|t<-xs, t>x]
Prelude| :}

mymap f [] = []
mymap f (x:xs) = f x:(mymap f xs)

Prelude> :{
Prelude| filter p [] = []
Prelude| filter p (x:xs) | p x = x:filter p xs | otherwise = filter p xs
Prelude| :}
Prelude> filter (>3) [1,2,3,4,5,6]
[4,5,6]

[x| x<-xs, p x] = filter p xs

Prelude> :{
Prelude| elem x [] = False
Prelude| elem x (y:ys) = if x==y then True else elem x ys
Prelude| :}

Prelude> :{
Prelude| zip _ [] = []
Prelude| zip [] _ = []
Prelude| zip (x:xs) (y:ys) = (x,y) : zip xs ys
Prelude| :}

Prelude> :{
Prelude| zipWith _ [] _ = []
Prelude| zipWith _ _ [] = []
Prelude| zipWith f (x:xs) (y:ys) = (f x y):zipWith f xs ys
Prelude| :}

Prelude> :{ 
Prelude| foldr _ b [] = b
Prelude| foldr f b (x:xs) = f x (foldr f b xs)
Prelude| :}

reverse xs == foldr (\x xs->xs++[x])) [] xs

Prelude> :{
Prelude| foldl _ e [] = e
Prelude| foldl f e (x:xs) = (foldl f (f e x) xs)
Prelude| :}

Prelude> flip f x y = f y x

ĆWIKI
16.
    a) euler n = length [x | x <-[1..n], gcd n x ==1]
    b) eulersum n = sum [euler x | x <-[1..n], mod n x == 0 ]

17.
    pitagoras a = [(a,b,c) | a <- [1..200], b <- [1..a], c<- [1..b], a^2 == b^2 + c^2, gcd b c == 1 ]

18. 
    :{
    fibb 0 = 0
    fibb 1 = 1
    fibb n = fibb (n-1) + fibb (n-2)
    :}

19.
    :{
    binomial n 0 = 1
    binomial n k = if n==k then 1 else binomial (n-1) (k-1) + binomial (n-1) k
    :}
20.
    [n | n <- [1..10000], n == sum[d|d<-[1..(n-1)],gcd d n == d]

27.
    :{
    mymap f [] = []
    mymap f (x:xs) = f x:(mymap f xs)
    :}

30.
Prelude> :{
Prelude| myinits [] = [[]]
Prelude| myinits x = [x] ++ myinits (init x)
Prelude| :}

31.
Prelude> :{
Prelude| mytails [] = [[]]
Prelude| mytails (x:xs) = [(x:xs)] ++ mytails xs
Prelude| :}

32.
mysplit = \n -> \xs -> (take n xs, drop n xs)
mysplit n x = (take n x, drop n x)
permutations xs = [(mysplit a xs) | a<-[0..length xs]]

33.
Prelude> :{
Prelude| nondec [] = False
Prelude| nondec [_] = True
Prelude| nondec (x:y:ys) = if x <= y then nondec (y:ys) else False
Prelude| :}
Prelude> :t nondec
nondec :: Ord a => [a] -> Bool

34.
:{
    myzip [] _ = []
    myzip _ [] = []
    myzip (x:xs) (y:ys) = (a,b) : myzip xs ys
:}

38.
Prelude> mmap f = map (map f)
Prelude> :t mmap
mmap :: (a -> b) -> [[a]] -> [[b]]
Prelude> mmap (^2) [[1,2,3],[4,5,6]]
[[1,4,9],[16,25,36]]
Prelude> mmmap f = map (map (map f))
Prelude> :t mmmap
mmmap :: (a -> b) -> [[[a]]] -> [[[b]]]
Prelude> mmmap (^2) [[[1,2,3],[4,5,6]]]
[[[1,4,9],[16,25,36]]]
podpunkt 3 to tak z definicji trochę, bo
f (g x) == (f . g) x, czyli
map ( map f) == (map . map) f
 

TRENING REKURENCJA
rwidzewlodz acc [] = acc
rwidzewlodz acc (x:xs) = rwidzewlodz (gcd acc x) xs

sigma n = sum [ x | x <-[1..n], mod n x == 0] (?)

to działa: (\f g n-> [f d * g (n/d) | d<-[1..n]] ) (^2) (^2) 3
to też już: (\f g n-> [f d * g (div n d) | d<-[1..n], mod n d == 0] ) (^2) (^2) 3

naizdup f n = [f x | x<-[1..n]]

Prelude> :{
Prelude| mymax [] = 0
Prelude| mymax [x] = x
Prelude| mymax (x:xs) | x > maxTail = x | otherwise = maxTail where maxTail = mymax xs
lub 
Prelude| :}
