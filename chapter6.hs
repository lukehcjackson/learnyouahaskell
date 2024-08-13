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

-}