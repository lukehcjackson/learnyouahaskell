{-

TYPES AND TYPECLASSES
================================
TYPES

in the ghci :t x will give you the type of x
ghci> :t 'a'
'a' :: Char
ghci> :t True
True :: Bool
ghci> :t "HELLO!"
"HELLO!" :: [Char]
ghci> :t (True, 'a')
(True, 'a') :: (Bool, Char)
ghci> :t 4 == 5
4 == 5 :: Bool

functions also have types. we can choose to explicitly declare their types
this is generally considered good practice

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

type [Char] -> [Char] means this function maps from a string to a string
makes sense as it takes one string as a parameter and returns one string
[Char] is synonymous with String so we could instead write String -> String

this function adds three integers
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

the paramters are separated with ->
there is no special distinction between paramters and return type
the return type is the last item in the declaration and the parameters are the first three

types:
    Int = 32 bit (probably) integer
    Integer = unbounded integer, so can store really big numbers
    Float = single precision float
    Double = double precision
    Bool
    Char
    Tuples have one type for each combination of length and types of their components

======================
TYPE VARIABLES

ghci> :t head
head :: [a] -> a

a here is a type variable, which means it can be of any type
think generics
function that have type variables are called polymorphic functions

ghci> :t fst
fst :: (a, b) -> a

=============================
TYPECLASSES

a typeclass is an interface that defines some behaviour
if a type is part of a typeclass, it supports and implements the behaviour
    the typeclass describes

what's the type of the == function?
    :t (==) returns (==) :: (Eq a) => a -> a -> Bool

everything before the => is called a class constraint
the type declaration reads like this:
    the == function takes any two values of the same type and returns a Bool
    the type of those two values must be a member of the Eq typeclass

any type where it makes sense to use == on two values of that type should be a
member of the Eq class

list of typeclasses:
    Eq is used for types that support equality testing i.e. == or /=.

    Ord is for types that have an ordering.
    e.g. ghci> :t (>)
    (>) :: (Ord a) => a -> a -> Bool
    All the standard comparing functions >, >=, ...

    Show is for things which can be presented as strings
    ghci> show 3
    "3"
    ghci> show 5.334
    "5.334"
    ghci> show True
    "True"

    Read takes a string and returns a type which is a member of Read
    ghci> read "True" || False
    True
    ghci> read "8.2" + 3.8
    12.0
    ghci> read "5" - 2
    3
    ghci> read "[1,2,3,4]" ++ [3]
    [1,2,3,4,3]
    note: if we just try to do read "4" it will throw an error
    this is because the type of read is read :: (Read a) => String -> a
    the compiler has no way to know the type of a if we don't use it for anything, just that it's a type which is part of Read

    in this case we need to use explicit type annotations to say what the type of the
    expression should be
    e.g. read "5" :: Int
    or   read "(3, 'a')" :: (Int, Char)

    Enum members are sequentially ordered types so can be enumerated
    They have defined successors and predecessors - succ and pred
    e.g. ['a'..'e'] or succ 'B'

    Bounded members have an upper and lower bound
    minBound :: Int returns -2147483648
    maxBound :: Bool returns True

    Num is a numeric typeclass. 
    Whole numbers are polymorphic constants - they can act like any type that's a member of the Num typeclass.
    ghci> 20 :: Int
    20
    ghci> 20 :: Integer
    20
    ghci> 20 :: Float
    20.0
    ghci> 20 :: Double
    20.0

    Integral is a numeric typeclass which only inclues whole numbers
    fromIntegral :: (Num b, Integral a) => a -> b
    takes an integral and turns it into a num
    useful for wanting to use integrals and floats together

-}