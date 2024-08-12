
{-
SYNTAX IN FUNCTIONS
=============================

PATTERN MATCHING

pattern matching is defining a pattern to which some data should conform,
and then treating it differently if it does / doesn't

for instance this function checks if a number is a 7 or not

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Not a seven"

When you call this function, the patterns are checked top-to-bottom and when the
argument conforms to a pattern, the corresponding function body will be used.

In this case we could do it with an if-else, but for more complex examples
pattern matching is easier:

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

We can use pattern matching to define the factorial function recursively:

factorial :: (Integral a) => a -> String
factorial 0 = 1
factorial n = n * factorial (n - 1)

If we do not cover all cases in the pattern matching it can throw an error
on an unexpected input

If we wanted to add two vectors without using pattern matching we would have done

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

Instead using pattern matching:

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

We have fst and snd for the first and second element of a pair
What about triples? We have to write our own functions

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

You can pattern match in list comprehensions:

let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
[a+b | (a,b) <- xs] returns [4,7,6,8,11,4]

We can use lists in pattern matching:

x:xs will bind the head of the list to x and the rest of it to xs
    if the list is only one element then xs will be an empty list
x:y:z:zs will bind the first three elements to x,y,z and the rest to zs
    (and only match against lists with >=3 elements)

We can use this to implement the head function:

head' :: [a] -> a
head' [] = error "Called head on an empty list"
head' (x:_) = x

Notice that if we want to bind multiple variables (even if we throw one
away with _) we need to surround them in parentheses

Let's write a function that tells us some of the first elements of a list:

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

We could equally have written (x:[]) as [x] and (x:y:[]) as [x.y]
but there is no other way to write (x:y:_)

We can write ANOTHER version of the length function using pattern matching and recursion

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

Let's implement sum

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

We can use 'as patterns' to break something up according to a pattern while still
keeping a reference to the entire thing
e.g. xs@(x:y:ys) will do the same thing as (x:y:ys) but also allow us to get the
entire list with xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

==================================================

GUARDS

guards are a way of testing if some property of a value is true or false
similar to if/else but work very well with pattern matching

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"

This works similarly to pattern matching where each statement is evaluated top-to-bottom
and the first one that matches is executed

Let's modify this to calculate the bmi rather than expecting it as input:

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "Underweight"
    | weight / height ^ 2 <= 25.0 .........

Let's implement max

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

Guards can be written inline but it's a bit less readable

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

Let's implement compare

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

================================

WHERE

when we defined bmiTell we repeated 'weight / height ^ 2' multiple times
it would be better to calculate it once and bind it to a variable

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"
    where bmi = weight / height ^ 2

we could even do

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= underweight = "Underweight"
    | bmi <= normal      = "Normal"
    | bmi <= overweight  = "Overweight"
    | otherwise          = "Obese"
    where bmi = weight / height ^ 2
          underweight = 18.5
          normal = 25.0
          fat = 30.0

you can even use 'where' bindings to pattern match:

    ...
    where bmi = weight / height ^ 2
          (underweight, normal, overweight) = (18.5, 25.0, 30.0)

Let's write a function which returns the initials of a name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

You can also define functions in where blocks:

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

============================================

LET

let bindings are similar to where bindings, but you can define a variable anywhere
(not just at the end)

this is a function to get a cylinder's surface area from it's height and radius

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

the format is let <bindings> in <expression>

the difference between let bindings and where bindings is that let bindings are
expressions, whereas where bindings are just syntactic constructs

so let bindings can be used in expressions like
4 * (let a = 9 in a + 1) + 2
    returns 42

they can also be used to introduce functions in a local scope:

[let square x = x * x in (square 5, square 3, square 2)]
returns [(25,9,4)]

if we wanted to bind several variables inline we have to separate them with semicolons

(let a = 100; b = 200; c = 300 in a*b*c)
returns (6000000)

you can use a let binding to dismantle a tuple into components
(let (a,b,c) = (1,2,3) in a+b+c)
returns 6

you can put let bindings inside list comprehensions
rewriting a function from earlier:

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]

this is similar to how we would use a predicate, except it doesn't filter the list,
it only binds to names

we could also combine it with a predicate:

calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

why not just use let bindings all the time instead of where?
let bindings cannot be used across guards

==========================================================

CASE EXPRESSIONS

case expressions are haskell's answer to switch case statements

this function from earlier

head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x

is just syntactic sugar, and is interchangeable with

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x
                    
the syntax is simple:

case expression of pattern -> result
                   pattern -> result
                   pattern -> result
                   ...

as before the patterns and checked top to bottom and if no pattern matches we
get a runtime error

pattern matching can only be used on function parameters
case expressions can be used almost anywhere

describeList :: [a]
describeList xs = "The list is " ++ case xs of []  -> "empty"
                                               [x] -> "a singleton list"
                                               xs  -> "a longer list"

we could have equally defined it with where using

describeList xs = "The list is " ++ what xs
    where what [] = "empty"
          what [x] = "a singleton list"
          what xs = "a longer list"

but maybe it's a bit nicer using the case expression

-}