{-

HIGHER ORDER FUNCTIONS

===========================

a function that takes a function as a parameter or returns a function as a value
is called a higher order function

=================

CURRIED FUNCTIONS

strictly speaking every function in haskell only takes one parameter
max 4 5 is actually:
    creating a function that takes one parameter, and returns 4 or the parameter,
    whichever is largest
THEN
    passing 5 as the parameter to that function

max 4 5 is the same as (max 4) 5

putting the space between two things is function application
let's look at the type of max

max :: (Ord a) => a -> a -> a

that can also be written as 

max :: (Ord a) => a -> (a -> a)
max takes an a and returns (->) a function, which takes an a and returns an a

this is why the return type and parameters of a function are just
separated by ->

if we call a function with too few parameters applied, we get a partially applied function
think of this as another function which takes as many parameters as we have left out

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

let multTwoWithNine = multThree 9
multTwoWithNine 2 3 returns 54

we used multThree 9 as a partially applied function here
and then gave it arguments 2 and 3 to give 2*3*9=54

let multWithEighteen = multTwoWithNine 2
multWithEighteen 10 returns 180

again we used multTwoWithNine 2 (which is multThree 9 2) as a partially applied function

let's write a function which takes a number and compares it to 100

compareWithHundred : (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

this works
but think about what compare 100 returns:
    it returns a function which takes a number as an argument, and compares it to 100
isn't that what we wanted in the first place?

so we can rewrite as

compareWithHundred:: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

so compareWithHundred 50 is the same as compare 100 50
compareWithHundred is just compare 100 waiting for the next argument

infix functions can also be partially applied by adding parentheses (sections)

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

now divideByTen 200 is equivalent to doing (/10) 200, or 200 / 10

this function checks if a character supplied to it is uppercase:

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

the only thing to consider here is that something like (-4) doesn't evaluate as
_ - 4, it evaluates as negative 4
if you wanted _ - 4 you would do (subtract 4)

=======================

MORE HIGHER ORDER STUFF

let's write a function that takes a function as a parameter and then applies it twice

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

the first parameter in the type declaration (a -> a) is a function

using this function:
applyTwice (+3) 10 returns 16
applyTwice (++ "HAHA") "HEY" returns "HEY HAHA HAHA"
applyTwice (3:) [1] returns [3,3,1]

let's use this idea to implement zipWith
it takes a function and two lists as parameters
it joins the two lists by applying the function between corresponding elements

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

in the type declaration,
the first parameter (a -> b -> c) is a function that takes a and b and returns c
the second and third parameters are lists of a and b
they have to be of a and b because the function in the first argument says so
the last parameter is also a list

using this:
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
[6,8,7,9]
ghci> zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]
ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
["foo fighters","bar hoppers","baz aldrin"]
ghci> zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]
ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
[[3,4,6],[9,20,30],[10,12,12]]

let's implement another function, flip
it takes a function as an argument and returns another function
which is the same, except the first two arguments are flipped

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

the type declaration says that flip takes a function, which takes an a and b
and returns a function that takes a b and an a
the second set of brackets is unnecessary because -> is right associative

(b -> a -> c) == (b -> (a -> c)) == b -> a -> c

we can rewrite the function

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

=======================================

MAPS AND FILTERS

map takes a function and a list, and applies that function to every element in the list
it returns a new list

it's defined like this:
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

example usages:
ghci> map (+3) [1,5,3,1,6]
[4,8,6,4,9]
ghci> map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]
ghci> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
[[1,4],[9,16,25,36],[49,64]]
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
[1,3,6,2,2]

filter takes a predicate (a function that tells us whether something is true or not - i.e. returns a bool)
and a list, and returns the list of elements that satisfy the predicate

it's defined like this:
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

if p x evaluates to true, it gets included in the new list
if it doesn't then don't include it

examples usages:
ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
[5,6,4]
ghci> filter (==3) [1,2,3,4,5]
[3]
ghci> filter even [1..10]
[2,4,6,8,10]
ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
[[1,2,3],[3,4,5],[2,2]]
ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
"uagameasadifeent"

everything that we can do with a map or a filter could be done with list comprehensions
just use whichever is more readable / easiest
map (+3) [1,5,3,1,6] is the same as writing [x+3 | x <- [1,5,3,1,6]]. 
The filter equivalent of applying several predicates in a list comprehension is either filtering something several times or joining the predicates with the logical && function.

let's rewrite quicksort in a more readable way using a filter:

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted  = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

let's find the largest number under 100,000 that's divisible by 3829
to do that we filter a set of possibilities in which we know the solution lies

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

we make a list of all numbers below 100,000, descending
we filter it by our predicate: x % 3829 == 0
because we are going in descending order the first number to satisfy
this predicate is our answer so we take the head
the evaluation stops when the first valid number is found

let's find the sum of all odd squares that are less than 10000

we will make us of the takeWhile function
it takes a predicate and a list, goes from the start of the list and returns
elements while the predicate holds. once an element is found for which the 
predicate doesn't hold, it stops.

to solve the problem then, we find all square numbers by mapping (^2) to [1..]
then we filter the results so we only get the odd squares
then we take elements from that list while they are smaller than 10000
then we sum everything up

we can do it in one line:

sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

we could have also written this using list comprehensions:

sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

now let's look at collatz sequences
looking at natural numbers
if the number is even, divide it by 2
if it's odd, multiply by 3 and add 1
continue until we reach 1

for all starting numbers between 1 and 100, how many chains have a length >15?

first lets write a function to produce a chain

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n*3 + 1)

so now to answer the question

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

here numLongChains is of type Int because length returns Int rather than Num
we could cast with fromIntegral if we wanted to

we can even use map to get a list of partially applied functions

listOfFuns = map (*) [0..]

and then do something like

(listOfFuns !! 4) 5
which returns 20
as listOfFuns !! 4 is (4*) which we can then give 5 as an argument

===============================

LAMBDAS

lambdas are used when we only need a function once
we can make a lambda using a \ , then the parameters, then -> and the function body, all surrounded by ()

we used a where binding in numLongChains to define isLong, just so we could pass it to filter
we could instead use a lambda

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

lambdas are expressions so we can pass them as arguments like that
the expression (\xs -> length xs > 15) returns a function

lambdas can take any number of parameters:

zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
returns [153.0,61.5,31.0,15.75,6.6]

you can also pattern match in lambdas
the only difference is you can't define several patterns for one parameter

map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
returns [3,8,9,8,7]

======================================

FOLDS

when doing recursion we often have an edge case for [], and want to do something
like x:xs, do something on x, then do something on xs recursively
this is a very common pattern, so folds were introduced to encapsulate it
they are similar to map but reduce a list to a single value

a fold takes a binary function, a starting value (call it the accumulator), and a list to 'fold up'
the binary function takes two parameters
the binary function is called with the accumulator and the first (or last) element of the list
this produces a new accumulator
the binary function is called again with the new accumulator and the new first (or last) list element
continue over the whole list until we're left with the final accumulator value

foldl is the left fold function, which folds the list up from the left side
the binary function is applied between the starting value and the head of the list

let's implement sum using foldl instead of explicit recursion

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

\acc x -> acc + x is the binary function
0 is the starting value
xs is the list to be folded

in the case of
ghci> sum' [3,5,2,1]
11

first, 0 is used as the acc parameter to the binary function and 3 is used as the x
0+3 = 3 so acc becomes 3
next, 3 is used as acc and 5 is x => acc = 8
continue like this and the result is 11

if we take into account that functions are curried we can write the implementation even more succintly

sum' : (Num a) => [a] -> a
sum' = foldl (+) 0

the lambda function (\acc x -> acc + x) is the same as (+)
we can omit the xs as the parameter because calling foldl (+) 0 will return a function that takes a list

generally, if you have a function like foo a = bar b a, you can rewrite it as foo = bar b, because of currying

let's implement elem with a left fold

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

the right fold, foldr, is similar except the accumulator takes values from the right of the list
the left fold's binary function takes the accumulator as the first parameter and the current value as the second one (\acc x -> ...)
the right fold's binary function takes the current value as the first paramter and the accumulator as the second (\x acc -> ...)

the accumulator value (and hence the result) of a fold can be of any type
we will implement map using a right fold. the accumulator will be a list.
we will accumulate the mapped list element by element
therefore the starting value should be an empty list

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

if we map (+3) to [1,2,3]:
    we start from the right with 3
    we apply f, to get 6
    we prepend to the accumulator, so 6:[] which is [6]
    now apply f to 2 to get 5
    prepand to [6] to get [5:6]
    continue to get [4,5,6]

we could have also implemented map with a left fold
however it would involve adding on to the end of the list every time
++ is more expensive than : so we use a right fold instead
generally we use right folds when building up new lists from a list for this reason

if you reverse a list you can do a right fold on it just as you would have done a left fold, and vice versa
however, bear in mind that right folds work on infinite lists but left folds do not
the intuition is that if you start at the end of an infinite list and work left, you will eventually reach the start of the list
but if you start at the left and work right, you will not reach the end

folds can be used to implement any function where you traverse a list once, 
element by element, and return some value based on that
whenever you want to traverse a list to return something, you probably want to use a fold

foldl1 and foldr1 are similar to foldl and foldr respectively,
but you don't need to specify a starting value 
they assume the first (or last) element of the list is the starting value

with that in mind we could implement sum as foldl1 (+)
however they depend on the list having at least one element

let's implement some standard library function using folds

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' : [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

we could also write reverse as foldl (flip (:)) [] because the lambda is just doing : with the parameters the other way around

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : ac else acc) []

these two are maybe better off defining in a different way but you can do it with folds

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

scanl and scanr are like foldl and foldr, but they give you a list
of all the intermediate accumulator steps
there is also scanl1 and scanr1

scanl (+) 0 [3,5,2,1]
returns [0,3,8,10,11]
result is the last element of the list

scanr (+) 0 [3,5,2,1]
returns [11,8,3,1,0]
result is the head of the list

how many elements does it take for the sum of the roots of all natural numbers
to exceed 1000?

to get the roots we do map sqrt [1..]
to get the sum we could do a fold, but we want to see how it's progressing
so we will use a scan
once we've done the scan, we can look at how many elements are below 1000
this number + 1 is how many it takes to exceed 1000

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

we use takeWhile because filter doesn't work on infinite lists

=====================================

FUNCTION APPLICATION WITH $

the $ function is defined:

($) :: (a -> b) -> a -> b
f $ x = f x

what is the point in this?
normal function application by putting spaces between things has very high precedence
applying the $ function has the lowest precedence
also, function application with spaces is left associative ( f a b c is the same as ((f a) b ) c .)
whereas function application with $ is right associative

if we wanted to do sum (map sqrt [1..130])
we could instead do sum $ map sqrt [1..130]

when a $ is encountered, the expression on it's right is used as a parameter for the function on its left

sqrt 3 4 9 is ((sqrt 3) + 4) + 9
if we wanted the square root of (3+4+9) we could write sqrt $ 3 + 4 + 9 rather than sqrt ( 3 + 4 + 9)

sum ( filter (>10) (map (*2) [2..10])) can be written as sum $ filter (>10) $ map (*2) [2..10]
because $ is right associative

it also means that function application can be treated like any other function
so for instance we can map function application over a list of functions

map ($3) [(4+), (10*), (^2), sqrt]
returns [7.0,30.0,9.0,1.7320508075688772]

================================================

FUNCTION COMPOSITION

in maths we can do (f o g)(x) - f of g of x = f(g(x)
this produces a new function that when called with parameter x is the
equivalent of calling g(x) then calling f on that result

in haskell function composition is pretty much the same thing
function composition is done with the . function
it is defined:

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

one usage for function composition is to make functions on the fly
to pass to other functions
we can use lambdas for this but function composition is often nicer

say we have a list of numbers and want to turn them all into negative numbers
we could do

map (\x -> negate (abs x)) [5,-3,-6,7]

using function composition we can rewrite it as

map (negate . abs) [5,-3,-6,7]

function composition is right associative so we can compose many functions at a time
f (g (z x)) is equivalent to (f . g . z) x

with that in mind we can turn

map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]

into

map (negate . sum . tail) [[1..5],[3..6],[1..7]]

when you want to use functions that take several parameters in this way you
either have to partially apply them or use $

sum (replicate 5 (max 6.7 8.9)) can be rewritten as
(sum . replicate 5 . max 6.7) 8.9 or
sum. replicate 5 . max 6.7 $ 8.9

another common use of function composition is point free style / pointless style

consider this function from earlier
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

we know we can omit the xs on both sides because of currying
writing it as 
sum' = foldl (+) 0
is called writing it in point free style

if we wanted to write

fn x = ceiling (negate (tan (cos (max 50 x))))

in point free style, we express it as a composition of functions

fn = ceiling . negate . tan . cos . max 50

earlier we wrote

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))   

we could write it with function composition as

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

but if we wanted it to be more readable it could be

oddSquareSum :: Integer
oddSquareSum = 
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

-}