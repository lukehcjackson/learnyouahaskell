
{-

MODULES

A Haskell module is a collection of related functions, types, and typeclasses
A Haskell program is a collection of modules:
    the main module loads up the other modules,
    and then uses the functions defined in them to do something

Having code split into modules has the normal advantages:
    -if a module is written to be generic enough it can be reused in other programs
    -if your own modules are loosely coupled enough they can be reused in the same way

The Haskell standard library is split into modules
Each of them contains functions and types that are somehow related, and serve some common purpose
There's a module for lists, for concurrent programming, for complex numbers ...

Everything we have dealt with so far is part of the Prelude module, which is imported by default

#### IMPORTING MODULES #####

modules must be imported before defining any functions (top of the file)

we import a module like

import Data.List

Data.List has useful functions for working with lists
let's use a function that it exports to create a function that tells us how many unique elements a list has

(after importing Data.List)
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

//reminding myself what this does
numUniques takes a list of a (which must be a member of the Eq typeclass - i.e. == works on them)
and returns an Int
the function body is length . nub
which is a function composition => the same as length(nub ...)
the ... depends on what nub does which we havent introduced
//

When we do 'import Data.List', all of the functions that Data.List exports
become available in the global namespace - we can call them from wherever in the script

nub is a function defined in Data.List which takes a list and removes duplicate elements
Therefore length . nub is equivalent to \xs -> length (nub xs)

//reminding myself again
\xs -> ... defines a lambda function (function we only want to use once) on xs
//

You can also put the functions of modules into the global namespace using GHCI
If you're in GHCI and want to be able to call the functions exported by Data.List,
do this:

ghci> :m + Data.List Data.Map Data.Set

However, if you've already loaded a script that imports a module, you
don't have to use :m + to access it.

If you only need a few functions from a module you can selectively import them
If we only wanted nub and sort from Data.List:

import Data.List (nub, sort)

Or you can export everything in a module EXCEPT a few functions
You might want to do this if several modules export functions with the same name
and you want to get rid of the ones you aren't using

import Data.List hiding (nub)

You can also deal with name clashes like this using qualified imports
For instance, Data.Map (the module for maps) exports a lot of functions
with the same names as functions in Prelude, like filter and null
When we import Data.Map and call 'filter', Haskell doesn't know which to use
So we use a qualified import

import qualified Data.Map

This means that if we want to use Data.Map's filter function, we have to do

Data.Map.filter

whereas 'filter' just refers to the normal filter.

Typing Data.Map.filter every time is a bit long winded so we can give it an alias

import qualified Data.Map as M

so now we can just type M.filter instead

Link to standard Haskell library and their source code: https://downloads.haskell.org/ghc/latest/docs/libraries/
Hoogle can be used to search for functions: https://hoogle.haskell.org/

#### DATA.LIST ####

The Data.List module provides useful functions for dealing with lists (!!!)
Some of its functions, like map and filter, are actually exported into Prelude
for convenience, so we've already used some of them. 

You don't have to do a qualified import for Data.List because none of its
function names clash with Prelude, except for those that Prelude already
steals from Data.List 

intersperse takes an element and a list, and then puts that element
in between each pair of elements in the list

intersperse '.' "MONKEY"
returns "M.O.N.K.E.Y"

intersperse 0 [1,2,3,4,5]
returns [1,0,2,0,3,0,4,0,5]

intercalate takes a list of lists, and a list
it then inserts that list in between all of the given lists,
and flattens the result
(like intersperse but for lists!)

intercalate " " ["hey","there","guys"]
returns "hey there guys"

intercalate [0,0,0] [[1,2,3], [4,5,6], [7,8,9]]
returns [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

transpose transposes a list of lists (wow)
i.e. if we consider a 2D list to be a matrix, it finds the matrix transpose
    the columns become the rows and vice versa

transpose [[1,2,3],[4,5,6],[7,8,9]]
returns [[1,4,7],[2,5,8],[3,6,9]]

transpose ["hey","there","guys"]
returns ["htg","ehu","yey","rs","e"]
interestingly this doesn't enforce a squareness constraint

Say we have the polynomials 3x^2 + 5x + 9
                            10x^3 + 9
                            8x^3 + 5x^2 + x - 1
and we want to add them together
We can represent them as the lists [0,3,5,9]   
                                   [10,0,0,9]
                                   [8,5,1,-1]
and add them together:

map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
which returns [18,8,6,17]

transposing the lists gives us the third powers in row 1, the second powers in row 2, ...
mapping sum over the transposed lists gives us our result

We have seen foldl and foldl1
foldl' and foldl1' are stricter versions of their respective lazy incarnations
When using lazy folds on really big lists, this can give a stack overflow error
This happens because the accumulator value isn't actually computed for each iteration of the fold
It makes a promise that it will compute the value once it is actually asked to (called a thunk)
This happens for every single intermediate accumulator
The thunks keep building up and this overflows the stack
The stricter folds actually do the intermediate computations as they go
which helps to avoid this issue

concat flattens a list of lists into just a list of elements

concat ["foo","bar","car"]
returns "foobarcar"

concat [[3,4,5],[2,3,4],[2,1,1]]
returns [3,4,5,2,3,4,2,1,1]

It removes just one level of nesting
If you want to flatten a double nested list (like a list of lists of lists)
then you can call concat twice

concatMap is the same as firstly mapping a function to a list,
and then concatenating the list with concat

concatMap (replicate 4) [1..3]
returns [1,1,1,1,2,2,2,2,3,3,3,3]

and takes a list of boolean values and returns True if all the values in the list are True

and $ map (>4) [5,6,7,8]
returns True
(maps (>4) to [5,6,7,8] which gives [True, True, True, True], which returns True when passed into 'and')

and $ map (==4) [4,4,4,3,4]
returns False

or returns True is any of the boolean values in a list are True

or $ map (==4) [2,3,4,5,6,1]
returns True

or $ map (>4) [1,2,3]
returns False

any and all take a predicate and then check if any or all of the elements
in a list satisfy the predicate
usually, we use these functions instead of mapping over a list and then doing 'and' or 'or'

any (==4) [2,3,4,6,1,4]
returns True

all (>4) [6,9,10]
returns True

all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
returns False

any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
returns True

iterate takes a function and a starting value. It applies the function
to the starting value, then applies it to the result of that, then again and again...
It returns the results in the form of an infinite list

take 10 $ iterate (*2) 1
returns [1,2,4,8,16,32,64,128,256,512]

take 3 $ iterate (++ "haha") "haha"
["haha","hahahaha","hahahahahaha"]

splitAt takes a number and a list. It then splits the list at that many elements,
returning the resulting two lists in a tuple

splitAt 3 "heyman"
returns ("hey","man")

splitAt 100 "heyman"
returns ("heyman","")

splitAt -3 "heyman"
returns ("", "heyman")

let (a,b) = splitAt 3 "foobar" in b ++ a
returns "barfoo"
(does splitAt 3 "foobar" giving ("foo","bar"), so a = "foo" and b = "bar". Then b ++ a = "bar" ++ "foo" => "barfoo")

takeWhile takes elements from a list while a predicate holds, and then,
when an element is encountered that doesn't satisfy the predicate, it's cut off

takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
returns [6,5,4]

takeWhile (/=' ') "This is a sentence"
returns "This"
(takes while the element is not equal to a space character)

takeWhile is a really useful function
Say we want to know the sum of all third powers that are under 10,000
We can't map (^3) to [1..], apply a filter, and then try to sum that up
because filtering an infinite list never finishes
WE know that the elements are ascending but Haskell doesn't!
Instead, we can use takeWhile:

sum $ takeWhile (<10000) $ map (^3) [1..]
returns 53361

We apply (^3) to an infinite list. Once an element that's over 10,000
is encountered, the list is cut off
Now we can easily sum it up. 

dropWhile is similar, except it drops all the elements while a predicate is true
Once the predicate equates to False, it returns the rest of the list

dropWhile (/=' ') "This is a sentence"
returns " is a sentence"

dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
returns [3,4,5,4,3,2,1]

Say we are given a list, which represents the value of a stock by date
The list is made of tuples, whose first component is the stock value,
the second is the year, the third is the month, and the fourth the date.
We want to know when the stock value first exceeded $1000

let stock = [(994.4, 2008, 9, 1), (995.2, 2008, 9, 2), (999.2, 2008, 9, 3), (1001.4, 2008, 9, 4), (998,=.3, 2008, 9, 5)]
head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
returns (1001.4,2008,9,4)

(this defines a lambda function as our predicate. it returns True while the first item in each tuple is <1000.
therefore the predicate becomes False as soon as we get the first value where price >= 1000
so stop dropping items from the list! get the remaining list and return the first item with head)

span is kind of like takeWhile, only it returns a pair of lists.
The first list contains everything the resulting list from takeWhile WOULD contain,
if it were called with the same predicate and the same list. 
The second list contains the part of the list that would have been dropped.

let (fw, rest) = spawn (/=' ') "This is a sentence" in "First word: " ++ fw ++ ", the rest: " ++ rest
returns "First word: This, the rest: is a sentence"

Where span spans the list while the predicate is True, 
break instead breaks it when the predicate is first True
Doing break p is the same as doing span (not . p)

break (==4) [1,2,3,4,5,6,7]
returns ([1,2,3], [4,5,6,7])

span (/=4) [1,2,3,4,5,6,7]
returns ([1,2,3], [4,5,6,7])

When using break, the second list in the result will start with the first
element that satisfies the predicate. 

sort sorts a list
The type of the elements in the list has to be part of the Ord typeclass

sort [8,5,3,2,1,6,4,2]
returns [1,2,2,3,4,5,6,8]

sort "This will be sorted now"
returns "   Tbedeehii......"

group takes a list and groups adjacent elements into sublists if they are equal

group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
returns [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

If we sort a list before grouping it, we can find out how many times
each element appears in the list

map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
returns [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]

(we sort the list, group it, then map a lambda function over it. 
For each item in the list (which is a list itself) 
we take the first element and the length of the list, and make a new tuple with those)

inits and tails are like init and tail, only they recursively apply that
to a list until there's nothing left

inits "woot"
returns ["","w","wo","woo","woot"]

tails "woot"
returns ["woot","oot","ot","t",""]

let w = "woot" in zip (inits w) (tails w)
returns [("", "woot"), ("w", "oot"), ("wo", "ot"), ("woo", "t"), ("woot", "")]

We can use a fold to implement searching a list for a sublist. 

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

First we call tails with the list in which we're searching
Then we go over each tail, and see if it starts with what we're searching for

In doing that, we've made a function that behaves like isInfixOf. 
isInfixOf searches for a sublist within a list and returns True if it's somewhere in the target list

"cat" `isInfixOf` "im a cat burglar"
returns True

"Cat" `isInfixOf` "im a cat burglar"
returns False

isPrefixOf and isSuffixOf search for a sublist at the beginning and at the end of a list, respectively

"hey" `isPrefixOf` "hey there!"
returns True

"there!" `isSuffixOf` "oh hey there!"
returns True



-}