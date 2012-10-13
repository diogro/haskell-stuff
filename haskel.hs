import Data.List
import System.Random

fibs :: [Integer]
fibs  = 0:1:[ a + b | (a,b) <- zip fibs (tail fibs) ]

sgn :: Float -> Int
sgn x =
 if x < 0
  then -1
  else if x > 0
   then 1
   else 0

baskara :: Float -> Float -> Float -> (Float, Float)
baskara a b c =
        let delta = sqrt (b^2 - 4*a*c)
        in ( (-b-delta)/(2*a),
             (-b+delta)/(2*a) )
raiz = do
        putStrLn "Digite os coeficientes"
        putStrLn "a:"
        a <- getLine
        putStrLn "b:"
        b <- getLine
        putStrLn "c:"
        c <- getLine
        let raiz = baskara (read a) (read b) (read c)
        putStrLn ("Raizes:" ++ show (fst(raiz)) ++ " - " ++ show (snd(raiz)))

guess r = do
        putStrLn "\nChuta ai Pilantra"
        num <- getLine
        if (read num) < r
                then do putStrLn "\nBaixo demais!"
                        guess r
                else if (read num) > r
                        then do putStrLn "\nAlto demais!"
                                guess r
                        else putStrLn "Valeu cumpade"


nomes = do
        putStrLn "Fala um nome"
        nome <- getLine
        case nome of
                "mane" -> do putStrLn "Doido"
                "jose" -> do putStrLn "mais doido ainda"
                "saulo" -> do putStrLn "massa"
                _ -> do putStrLn "xiiii..."

fact :: Integer -> Integer
fact 0 = 1
fact n = n*fact(n-1)

factdouble :: Integer -> Integer
factdouble 0 = 1
factdouble 1 = 1
factdouble n = n * factdouble (n-2)

power :: Integer -> Integer -> Integer
power x 0 = 1
power x y = x* power x (y-1)

plusOne :: Integer -> Integer
plusOne x = x + 1

add :: Integer -> Integer -> Integer
add x 0 = x
add x y = add (plusOne x) (y-1)

repli :: Integer -> a -> [a]
repli 0 a = []
repli n a = a : repli (n-1) a

element :: [a] -> Integer -> a
element (x:xs) 0 = x
element (x:xs) n = element xs (n-1)

zipper :: [a] -> [b] -> [(a,b)]
zipper (x:xs) [] = []
zipper (x:xs) (y:ys) = (x,y) : zipper (xs) (ys)

diffs :: [Int] -> [Int]
diffs [] = []
diffs x = [ abs  (b - a)  | (a,b) <- zip x (tail x)]

factors :: Integer -> [Integer]
factors p = [ f | f <- [1..p], p `mod` f == 0 ]

notfactors p = [ f | f <- [1..p], not(p `mod` f == 0) ]

factores :: [[Integer]]
factores = map factors [0..]

notfactores = map notfactors [0..]

lengh :: [a] -> Integer
lengh [] = 0
lengh (x:xs) = 1 + (lengh xs)

primos :: [Integer]
primos = 1 : [ (x !! 1) | x <- factores , (lengh x) == 2 ]

rleE :: Eq a => [a] -> [(Integer, a)]
rleE list = [ ((lengh x), (head x)) | x <- (group list)]

rleD :: [(Integer, a)] -> [a]
rleD list = concat [ repli a b | (a,b) <- list ]

n2 = [ 1/x^2 | x <- [1..]]

ands :: [Bool] -> Bool
ands = foldr (&&) True

ors :: [Bool] -> Bool
ors =foldr (||) False

maxs :: Ord a => [a] -> a
maxs = foldr1 max

mins :: Ord a => [a] -> a
mins = foldr1 min

factList :: Integer -> [Integer]
factList p = scanl1 (*) [1..p]

for :: a -> (a->Bool) -> (a->a) -> (a-> IO ()) -> IO ()
for i p f job =
        if p i
        then do
                job i
                for (f i) p f job
        else return ()

printInt :: Show a => [a] -> IO ()
printInt [] = return ()
printInt (x:xs) =
        do
        print x
        printInt xs

data Tree a = Leaf a | Branch (Tree a) (Tree a)

tree1 :: Tree Integer
tree1 =
        Branch
         (Branch
          (Branch
           (Leaf 1)
           (Branch (Leaf 2) (Leaf 3)))
          (Branch
           (Leaf 4)
           (Branch (Leaf 5) (Leaf 6))))
         (Branch
          (Branch (Leaf 7) (Leaf 8))
          (Leaf 9))

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f = g where
  g (Leaf x) = Leaf (f x)
  g (Branch left right) = Branch (g left) (g right)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fbranch fleaf = g where
  g (Leaf x) = fleaf x
  g (Branch left right) = fbranch (g left) (g right)


doubleTree = treeMap (*2)  -- doubles each value in tree

sumTree = treeFold (+) id -- sum of the leaf values in tree

fringeTree = treeFold (++) (: [])  -- list of the leaves of tree

ehprimo :: Integer -> Bool
ehprimo n = and [ x /= 0 | x <- map (mod n) [2..n-1] ]

primosnk :: [Integer]
primosnk = sort [ n*10^k+1 | n <- [1..10], k <- [1..7], ehprimo (n*10^k+1) ]
