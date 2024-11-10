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

We have seen elem and notElem - they check if an element is or isn't inside a list

partition takes a list and a predicate,
and returns a pair of lists. 
The first list in the result contains all the elements that satisfy the predicate
and the second one contains all the elements that don't

partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
returns ("BOBMORGAN","sidneyeddy")

partition (>3) [1,3,5,6,3,2,1,0,3,7]
returns ([5,6,7],[1,3,3,2,1,0,3])

It is important to note how this differs from span and break
span and break stop once they reach the first element that doesn't satisfy the predicate
partition goes through the whole list and splits it up according to the predicate

find takes a list and a predicate, and returns the first element that satisfies the predicate
However, it returns that element wrapped in a Maybe value
This is covered more in the next chapter, but for now:
A Maybe value can either be Just something or Nothing
Like how a list can be either an empty list, or a list with some elements, 
a Maybe value can either be no elements or a single element
If the type of a list of integers is [Int], then the type of maybe having an Int
is Maybe Int

find (>4) [1,2,3,4,5,6]
returns Just 5

find (>9) [1,2,3,4,5,6]
returns Nothing

Let's have a look at the type of find
:t find
returns find :: (a -> Bool) -> [a] -> Maybe a

( (a -> Bool) is how the type of a predicate is defined)
The result of find is of type Maybe a
That's similar to having a type of [a], only a value of type Maybe can contain
either no elements or one element, whereas a list can contain no elements, one element, or several elements

Recall when we were looking at stock prices for when they first went over $1000
We used head. But head isn't really safe - if the price never went over $1000,
dropWhile would return the empty list and then taking head would throw an error
However, if we rewrote it as

find (\(val, y, m, d) -> val > 1000) stock

we would be much safer. If our stock never went over $1000, we would get back
a Nothing. But if there was a valid answer, we would get a Just.

elemIndex is kind of like elem
Except it doesn't return a bool, it returns the index of the element
we're looking for. 
If the element isn't in the list, it returns a Nothing

4 `elemIndex` [1,2,3,4,5,6]
returns Just 3

10 `elemIndex` [1,2,3,4,5,6]
returns Nothing

elemIndices is like elemIndex, only it returns a list of indices,
in the case that the element we are searching for appears multiple times. 
Because we are now using a list to represent the indices, we don't need
the Maybe type. If the element never appears we can just return []

' ' `elemIndices` "Where are the spaces?"
returns [5,9,13]

findIndex is like find, but it maybe returns the index of the first element
that satisfies the predicate
findIndices returns a list of the indices of all elements that satisfy
the predicate. 

findIndex (==4) [5,4,2,1,6,4]
returns Just 5

findIndex (==7) [5,3,2,1,6,4]
returns Nothing

findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
returns [0,6,10,14]

We have already covered zip and zipWith
We noted that they zip together two lists, either in a tuple or 
with a binary function (a function which takes two parameters)
What if we want to zip three lists? Or three lists with a function
that takes three parameters?
For that, we have zip3, zip4, ... , zipWith3, zipWith4, ...
These variants go up to 7

zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
returns [7,8,9]

zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
returns [(2,2,5,2),(3,2,5,2),(3,2,3,2)]

Just like with normal zipping, lists that are longer than the shortest list
get cut down to size

lines is a useful function when dealing with files, or input from somewhere
It takes a string and returns every line of that string in a separate list

lines "first line\nsecond line\nthird line"
returns ["first line","second line","third line"]

unlines is the inverse function of lines
It takes a list of strings, and joins them together with \n's

unlines ["first line", "second line", "third line"]
returns "first line\nsecond line\nthird line\n"

words and unwords are for splitting a line of text into words,
or joining a list of words into a text

words "hey these are the words in this sentence"
returns ["hey","these","are","the","words","in","this","sentence"]

words "hey these       are the words\nin this one"
returns ["hey","these","are","the","words","in","this","one"]

unwords ["hey","there","mate"]
returns "hey there mate"

We've already mentioned nub, which takes a list and weeds out duplicate elements,
returning a list of unique elements

nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
returns [1,2,3,4]

nub "Lots of words and stuff"
returns "Lots fwrdanu"

delete takes an element and a list, and deletes the first occurance of that
element in the list

delete 'h' "hey there gang"
returns "ey there gang"

delete 'h' . delete 'h' $ "hey there gang"
returns "ey tere gang"

\\ is the list difference function.
It acts like a set difference
For every element in the right hand list, it removes a matching element in the left one

[1..10] \\ [2,5,9]
returns [1,3,4,6,7,8,10]

"Im a big baby" \\ "big"
returns "Im a  baby"

Doing [1..10] \\ [2,5,9] is like doing
delete 2 . delete 5 . delete 9 $ [1..10]

union also acts like a function on sets
It returns the union of two lists!
It goes over every element in the second list, and appends it
to the first one if it isn't already in yet.

"hey man" `union` "man what's up"
returns "hey manwt'sup"

[1..7] `union` [5..10]
returns [1,2,3,4,5,6,7,8,9,10]

intersect works like set intersection
It returns only the elements found in both lists

[1..7] `intersect` [5..10]
returns [5,6,7]

insert takes an element and a list of elements that can be sorted,
and inserts it into the last position where it's still <= the next element
In other words, insert starts at the beginning of the list and
keeps going until it finds an element that equal or greater than the element
we want to insert, and inserts it just before. 

insert 4 [3,5,1,2,8,2]
returns [3,4,5,1,2,8,2]

insert 4 [1,3,4,4,1]
returns [1,3,4,4,1]
in this case the 4 is inserted between the original 3 and 4

If we use insert to insert into a sorted list,
the result will remain sorted

insert 4 [1,2,3,5,6,7]
returns [1,2,3,4,5,6,7]

The length, take, drop, splitAt, !! and replicate functions all take an Int
as one of their parameters, or return an Int. 
They could be more generic and usable if they instead took any type
that's part of the Integral or Num typeclasses (depending on the function)
This is for historical reasons, and fixing it could break a lot of existing code. 

Data.List has their more generic equivalents, named
genericLength, genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate

For instance, length has a type signature of
    length :: [a] -> Int
If we try to get the average of a list of numbers by doing
    let xs = [1..6] in sum xs / length xs
we get a type error, because you can't use / with an Int. 
genericLength, however, has a type signature of
    genericLength :: (Num a) => [b] -> a
Because a Num can act like a floating point number, getting the
average like we tried to before works fine. 

The nub, delete, union, intersect and group functions have generic counterparts
called nubBy, deleteBy, unionBy, intersectBy, and groupBy. 
The difference is that the first set of functions use == to test for equality. 
The 'by' versions also take an equality function and use that to compare. 
This means that, for instance, 'group' is the same as 'groupBy (==)'

For instance, say we have a list that describes the value of a function for
every second. We want to segment it into sublists, based on when the value was
below zero and when it went above. If we just did a normal group, it would just
group the equal adjacent values together. But what we want is to group them by
whether they are negative or not. This is where groupBy comes in. 
The equality function supplied to the 'by' functions should take two elements
of the same type and return True if it considers them equal by it's standards

let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
groupBy (\x y -> (x > 0) == (y > 0)) values
returns [[-4.3, -2.4, -1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]

This lets us clearly see which regions are positive and which are negative. 
The equality function supplied takes two elements, and returns True only if
they are both negative or both positive. 
This equality function could also have been written as
    \x y  -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)
An even clearer way to write equality functions for the 'by' functions is
if you import the 'on' function from Data.Function 

on is defined like this:
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

So by doing (==) `on` (>0) returns an equality function that looks like
    \x y -> (x > 0) == (y > 0)
'on' is used a lot with the 'by' functions, because with it, we can do:

groupBy ((==) `on` (>0)) values
returns returns [[-4.3, -2.4, -1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
(with the same values)

This is more readable
It reads 'group this by equality on whether the elements are greater than zero'

#### Data.Char #####

The Data.Char module export functions that deal with characters.
It's also helpful when filtering and mapping over strings, because they are just lists of characters.

Data.Char exports a bunch of predicates over characters. That is, functions that take a character and 
tell us whether some assumption about it is true or false. For instance:

isControl checks whether a character is a control character

isSpace checks whether a character is a white-space character (spaces, tabs, newlines, ...)

isLower checks whether a character is lowercase

isUpper checks whether a character is uppercase

isAlpha checks whether a character is a letter

isAlphaNum checks whether a character is a letter or a number

isPrint checks whether a character is printable. Control characters, for instance, are not printable. 

isDigit checks whether a character is a digit

isOctDigit checks whether a character is an octal digit

isHexDigit checks whether a character is a hex digit

isLetter checks whether a character is a letter

isMark checks for unicode mark characters.
these are characters that combine with preceding letters, to form letters with accents - e◌́ => é

isNumber checks whether a character is numeric

isPunctuation checks whether a character is punctuation

isSymbol checks whether a character is a mathematical or currency symbol

isSeparator checks for unicode spaces and separators

isAscii checks whether a character falls into the first 128 characters of unicode (i.e. is ascii)

isLatin1 checks whether a character falls into the first 256 characters of unicode

isAsciiUpper checks whether a character is ASCII and is upper-case

isAsciiLower checks whether a character is ASCII and is lower-case

All of these predicates have a type signature of Char -> Bool. Most of the time we use this to filter out strings or something similar.
For instance, say we are making a program that takes a username, which can only be comprised of alphanumeric characters. We can use the
Data.List function 'all' in combination with the Data.Char predicates to determine if the username is alright:

    all isAlphaNum "bobby123"
    returns True

    all isAlphaNum "hello hello hello!"
    returns False

We could also use isSpace to simulate the Data.List function 'words'

words "hey guys its me"
returns ["hey","guys","its","me"]

groupBy ((==) `on` isSpace) "hey guys its me"
returns ["hey", " ", "guys", " ", "its", " ", "me"]

Well, it almost does what words does. It leaves us with the empty spaces. 
We can filter these out:

filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"
returns ["hey", "guys", "its", "me"]

Data.Char also exports a datatype that's kind of like Ordering. 
The Ordering type can have a value of LT, EQ or GT. It's sort of an enumeration. It describes a few possible results that can arise from comparing two elements. 
The GeneralCategory type is also an enumeration. It presents us with a few possible categories that a character can fall into. 
The main function for getting the general category of a character is generalCategory. It has a type of generalCategory :: Char -> GeneralCategory
There are abouut 31 categories, here are a few:

generalCategory ' '
returns Space

generalCategory 'A'
returns UppercaseLetter

generalCategory 'a'
returns LowercaseLetter

generalCategory '.'
returns OtherPunctuation

generalCategory '9'
returns DecimalNumber

map generalCategory " \t\nA9?|"
returns [Space, Control, Control, UppercaseLetter, DecimalNumber, OtherPunctuation, MathSymbol]

Since the GeneralCategory type is part of the Eq typeclass, we can also test for things like
generalCategory c == Space

toUpper converts a character to upper-case. Spaces, numbers, etc remain unchanged

toLower converts a character to lower-case

toTitle convers a character to title-case (which is normally the same as uppercase)

digitToInt converts a character to an Int. This only works for characters in the ranges 0..9, a..f and A..F 

map digitToInt "34538"
returns [3,4,5,3,8]

map digitToInt "FF85AB"
returns [15,15,8,5,10,11]

intToDigit is the inverse function of digitToInt. It takes an Int in the range of 0..15 and converts it to a lower-case character

intToDigit 15
returns 'f'

intToDigit 5
returns '5'

The ord and chr functions convert characters to their corresponding numbers and vice versa

ord 'a'
returns 97

chr 97
returns 'a'

map ord "abcdefgh"
returns [97,98,99,100,101,102,103,104]

The difference between the ord values of two characters is their distance between each other in the Unicode table. 

The Caesar cipher is a method of encoding messages by shifting each character by a fixed number of positions in the alphabet. 
We can create a sort of Caesar cipher of our own, only we won't contrict ourselves to the alphabet:

encode :: Int -> String -> String
encode shift msg = 
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

We first convert the string to a list of numbers (map ord msg). Then, we add the shift amount to each number (map + shift ords)
before converting the list of numbers back to characters (map chr shifted).
You could also write the body of this function as map(chr . (+ shift) . ord) msg

encode 3 "Heeey"
returns "Khhh|"

encode 4 "Heeey"
returns "Liii}"

Decoding a message is just shifting it back by the number of places it was shifted by in the first place

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

ghci> encode 3 "Im a little teapot"  
returns "Lp#d#olwwoh#whdsrw"  
ghci> decode 3 "Lp#d#olwwoh#whdsrw"  
returns "Im a little teapot"  

##### Data.Map #####

Association lists (also called dictionaries) are lists that are used to store key-value pairs, where ordering does not matter. 
For instance, we might use an association list to store phone numbers, where phone numbers might be the values and people's names would be the keys. 
We don't care what order they're stored, we just want to get the right phone number for the right person. 

The most obvious way to represent an association list in Haskell would be by having a list of pairs. 
The first component in the pair would be the key, and the second component the value. 
Here's an example:

phoneBook =     
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ]

This is just a list of pairs of strings. 
The most common task when dealing with association lists is looking up some value by key. Let's make a function that looks up a value given a key. 

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

The function takes a key and a list, filters the list so only matching keys remain (filter (\(k,v) -> key == k) % xs),
gets the first key-value that matches (head) and returns the value (snd - returns the second element i.e the value)
What happens if the key we're looking for isn't in the association list?
We would try to get the head of an empty list, which throws an error. 
Let's use the Maybe datatype - if we don't find the key, we'll return a Nothing. If we do find it, we'll return Just ..., where ... is the value corresponding to the key

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs

This is a textbook recursive function that operates on a list. The edge case, splitting a list into a head and a tail, recursive calls, they're all there.
This is the classic fold pattern, so let's see how this could be implemented as a fold:

findKey (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

findKey "penny" phoneBook
returns Just "853-2492"

findKey "wilma" phoneBook
returns Nothing

We have just implemented the 'lookup' function from Data.List
If we want to find the corresponding value to a key, we have to traverse all the elements of the list until we find it. 
The Data.Map module offers association lists that are must faster - because they are internally implemented with trees.
It also provides a lot of utility functions. From now on, we'll say we are working with maps instead of association lists. 

Because Data.Map exports functions that clash with Prelude and Data.List, we will do a qualified import:

import qualified Data.Map as Map

The fromList function takes an association list (in the form of a list) and returns a map with the same associations

ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
returns fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] 

If there are duplicate keys in the original association list, the duplicates are discarded. This is the type signature of fromList:]

Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v

It says that it takes a list of pairs of type k and v, and returns a map that maps from keys of type k to values of type v
Notice that when we were doing association lists with normal lists, the keys only had to be equatable (their type belonging to the Eq typeclass)
but now they have to be orderable. This is an essential constraint in the Data.Map module. The keys must be orderable so it can arrange them into a tree. 

You should always use a Data.Map for key-value associations unless you have keys that aren't part of the Ord typeclass. 

empty represents an empty map. It takes no arguments, it just returns an empty map. 

Map.empty
returns fromList []

insert takes a key, a value and a map, and returns a new map that's just like the old one, only with the key and value inserted. 

Map.insert 3 100 Map.empty
returns fromList [(3,100)]

Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))  
returns fromList [(3,100),(4,200),(5,600)]  

Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty  
returns fromList [(3,100),(4,200),(5,600)]  

We can implement our own fromList by using the empty map, insert, and a fold:

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty 

We start off with an empty map, and we fold it up from the right, inserting the key value pairs into the accumulator as we go. 

null checks if a map is empty

Map.null Map.empty
returns True

Map.Null $ Map.fromList [(2,3),(5,5)]
returns False

size reports the size of a map

Map.size Map.empty
returns 0

Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)] 
returns 5

singleton takes a key and a value, and creates a map that has exactly one mapping

Map.singleton 3 9
returns fromList [(3,9)]

Map.insert 5 9 $ Map.singleton 3 9
returns fromList[(3,9), (5,9)]

lookup works like the Data.List lookup, only it operates on maps. It returns Just something if it finds something for the key, and Nothing if it doesn't

member takes a key and a map and reports whether the key is in the map or not

Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
returns True

map and filter work much like their list equivalents

Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
returns fromList [(1,100), (2,400), (3,900)]

Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
returns fromList [(2,'A'),(4,'B')]

toList is the inverse of fromList

Map.toList . Map.insert 9 2 $ Map.singleton 4 3
returns [(4,3),(9,2)]

keys and elems returns lists of keys and values respectively.
keys is the equivalent of  map fst . Map.toList 
elems is the equivalent of map snd . Map.toList

fromListWith acts like fromList, only it doesn't discard duplicate keys, but instead uses a function supplied with it to decide what to do with them
let's say someone can have multiple phone numbers, and we have an association list set up like this:

    phoneBook =   
        [("betty","555-2938")  
        ,("betty","342-2492")  
        ,("bonnie","452-2928")  
        ,("patsy","493-2928")  
        ,("patsy","943-2929")  
        ,("patsy","827-9162")  
        ,("lucille","205-2928")  
        ,("wendy","939-8282")  
        ,("penny","853-2492")  
        ,("penny","555-2111")  
        ]  

If we just used fromList to put that into a map, we would lose a few numbers. So instead, we can do:

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

Now,
Map.lookup "patsy" $ phoneBookToMap phoneBook  
returns "827-9162, 943-2929, 493-2928"  

If a duplicate key is found, the function we pass is used to combine the values of those keys into some other value. 
We could also first make all the values in the association list singleton lists, and then use ++ to combine the numbers:

phoneBookToMap :: (Ord k) => [(k,a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

Map.lookup "patsy" $ phoneBookToMap phoneBook  
returns ["827-9162","943-2929","493-2928"]  

Another use case is for when we are making a map from an association list with numbers:
when a duplicate key is found, we want to biggest value for the key to be kept

Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11)]
returns fromList [(2,100),(3,29)]

Or we could choose to add together values with the same keys

Map.fromListWith (+) ....

insertWith is to 'insert' what fromListWith is to 'fromList'. It inserts a key-value pair into a map, but if that map already contains the key, it uses the function passed with it to determine what to do. 

Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
returns fromList [(3,104),(5,103),(6,339)]

Full list of Data.Map functions: https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html

##### Data.Set #####

The Data.Set module offers sets. All the elements in a set are unique. They're internally implemented with trees => they're ordered. 
Checking for membership, insertion, deletion etc is much faster than doing the same thing with lists. 

We do a qualified import because the names in Data.Set clash with a lot of Prelude and Data.List names. 

import qualified Data.Set as Set

Say we have two pieces of text. We want to find out which characters are used in both of them.

    text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
    text2 = "The old man left his garbage can out and now his trash is all over my lawn!"  

The fromList functions works how you would expect. It takes a list and converts it to a set. 

ghci> let set1 = Set.fromList text1  
ghci> let set2 = Set.fromList text2  
ghci> set1  
fromList " .?AIRadefhijlmnorstuy"  
ghci> set2  
fromList " !Tabcdefghilmnorstuvwy"

The items are ordered and each element in unique. 

We can use the intersection function to see which elements they both share:

Set.intersection set1 set2
returns fromList " adefhilmnorstuy"

We can use the difference function to see which letters are in the first but not the second set, and vice versa:

Set.difference set1 set2
returns fromList ".?AIRj"

Set.difference set2 set1
returns fromList "!Tbcgvw"

We can see all the unique letters used in both sentences using union:

Set.union set1 set2
returns fromList " !.?AIRTabcdefghijlmnorstuvwy"

The null, size, member, empty, singleton, insert and delete functions all work how you would expect:

    ghci> Set.null Set.empty  
    True  
    ghci> Set.null $ Set.fromList [3,4,5,5,4,3]  
    False  
    ghci> Set.size $ Set.fromList [3,4,5,3,4,5]  
    3  
    ghci> Set.singleton 9  
    fromList [9]  
    ghci> Set.insert 4 $ Set.fromList [9,3,8,1]  
    fromList [1,3,4,8,9]  
    ghci> Set.insert 8 $ Set.fromList [5..10]  
    fromList [5,6,7,8,9,10]  
    ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]  
    fromList [3,5]  

We can also check for subsets and proper subsets.
Set A is a subset of set B if B contains all the elements of A. 
Set A is a proper subset of set B if B contains all the elements of A, and has more elements. 

    ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
    True  
    ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
    True  
    ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]  
    False  
    ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
    False 

We can also map over sets and filter them:

Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
returns fromList [3,5,7]

Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
returns fromList [3,4,5,6,7,8]

Sets are often used to weed a list of duplicates from a list, by first making it into a set with fromList and then converting it back to a list with toList
The Data.List function nub already does that, but weeding out duplicates for large lists is much faster if you cram them into a set and then convert them back to a list, rather than using nub
But using nub only requires the type of the list's elements to be part of the Eq typeclass, whereas using sets requires them to be in Ord

let setNub xs = Set.toList $ Set.fromList xs
setNub "HEY WHATS CRACKALACKIN"
returns " ACEHIKLNRSTWY"

nub "HEY WHATS CRACKALACKIN"
returns "HEY WATSCRKLIN"

setNub is generally faster than nub on big lists, but nub preserves the ordering of the list's elements, whereas setNub does not. 

##### MAKING OUR OWN MODULES #####

How can we make our own modules?
It is good practise to take functions and types that work towards a similar purpose and put them into a module.
That way, you can easily reuse those functions in other programs by just importing your module. 

Let's see how we can do this by creating a little module that provides some functions for calculating the volume and area of some geometric objects.
We'll start by creating a file called Geometry.hs
(this continues in that file, as well as some notes here)

We say that a module exports functions. This means that when I import a module, I can only use the functions that it exports. 
It can define other functions, which its functions call internally, but we can only see and use the ones that it exports. 

To use our module, we just do

    import Geometry

Geometry.hs has to be in the same folder as the program that's import it, though. 

Modules can also be given a hierarchical structure. Each module can have a number of sub-modules and they can have sub-modules of their own. 
Let's section these functions off so that Geometry is a module that has three sub-modules, one for each type of object. 

First, we make a folder called Geometry
Inside, we place three files: Sphere.hs, Cuboid.hs and Cube.hs 

First is Geometry.Sphere:
Notice how we placed it in a folder called Geometry and then defined the module name as Geometry.Sphere 
We did the same thing for Cuboid and Cube. 
Also notice how, in all three sub-modules, we defined functions with the same names. 
We can do this because they're separate modules. 

We want to use functions from Geometry.Cuboid in Geometry.Cube, but because it exports functions with the same names as Geometry.Cube we have to do a qualified import. 

Now, if we're in a file that's on the same folder level as the Geometry folder, we can do:
    import Geometry.Sphere

And then we can call area and volume and they'll give us the area and volume for a sphere. 
If we want to juggle two or more of these modules, we have to do qualified imports because they export functions with the same names.
So we just do something like:

import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

And then we can call Sphere.area, Cuboid.volume , ... and each will calculate the area/volume for their respective object. 

The next time you find yourself writing a file that's really big and has a lot of functions, try and see which functions serve a common purpose and then try to put them in their own module. 
You'll be able to just import the module the next time you're writing a program that requires some of the same functionality. 

-}