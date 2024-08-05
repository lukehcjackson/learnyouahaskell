doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

{-

succ 9 * 10 = (succ 9) * 10
to get 91 we have to do succ (9*10)

succ 9 + max 5 4 + 1
= (succ 9) + (max 5 4) + 1

div 92 10 = 92 `div` 10 (infix notation)

===================
LISTS
===================

defining a list
    let numbers = [4,6,7,2,1]
must be all the same type

join two lists
    [1,2,3] ++ [4,5,6] returns [1,2,3,4,5,6]
    "hello " ++ "world returns "hello world"
    ['a','b'] ++ ['c','d'] returns "abcd" (strings are lists of char)
++ is O(n) so be careful adding to long lists

add to beginning of list
    'A':" SMALL CAT" gives "A SMALL CAT"
    5 : [4,3,2,1] gives [5,4,3,2,1]
: is O(1)

index into a list
    "abcde" !! 3 returns 'd'
    [50,100,150,200] !! 1 returns 100

lists can contain any number of lists but they must all be of
the same type!!

we can compare lists
    first we compare the heads, then index 1, ...
    [3,2,1] > [2,1,0] returns True
    [3,4,2] > [3,4] returns True
    [3,4,2] == [3,4,2] returns True

getting elements
    head [5,4,3,2,1] returns 5
    tail [5,4,3,2,1] returns [4,3,2,1]
    last [5,4,3,2,1] returns 1
    init [5,4,3,2,1] returns [5,4,3,2]

other list operations
    length returns the length (wow!!)
    null returns True if the list is empty
    reverse reverses a list
    take extracts some number of elements from the beginning of a list
        e.g. take 3 [1,2,3,4,5] returns [1,2,3]
        if you try to take more elements than there are in the list,
        it just returns the whole list
    drop is similar, but it removes elements from the beginning of the list
        e.g. drop 3 [1,2,3,4,5,6] returns [4,5,6]
    maximum returns largest value
    minimum returns the smallest
    sum returns the sum of the list
    product returns the product
    elem returns True if an item is an element of a list
        e.g. 4 `elem` [3,4,5] returns True

=============================================
RANGES

all natural numbers from 1 to 20: [1..20]
all lowercase letters: ['a'..'z']
uppercase K to Z: ['K'..'Z']

every even number from 1 to 20: [2,4..20]
every third number ... : [3,6..20]
however this has limitations. you can only specify one step.
e.g. can't do [1,2,4,8..100] for powers of two
also if you want 20-1 backwards you have to specify [20,19..1]

you can make infinite ranges by not specifying an upper limitations

cycle takes a list and cycles it into an infinite list
    e.g. take 5 (cycle "LOL ") returns "LOL LOL LOL LOL LOL "
repeat takes an element and produces an infinite list of it
    e.g. take 5 (repeat 1) returns [1,1,1,1,1]
replicate makes a finite list from one element
    e.g. replicate 3 10 returns [10,10,10]

=================================================
LIST COMPREHENSIONS

get the first ten even numbers:
    [x*2 | x <-[1..10]] 
    returns [2,4,6,...]
    x is drawn from the range [1..10], and for each element in that range
    we return that element, doubled

say we only wanted the elements which, when doubled, are >= 12
we can add a condition (a predicate):
    [x*2 | x <- [1..10], x*2 >= 12] 
    returns [12,14,16,...]

all numbers from 50 to 100, whose remainder when divided by 7 is 3
    [x | x<-[50..100], x `mod` 7 == 3]

replace every odd number > 10 with "BANG!" and every odd number < 10 with "BOOM!"
if the number isn't odd, we throw it out of the list
    boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
    odd x returns true for an odd number, and the element is only included
    in the list if all the predicates evaluate to true
    boomBangs [7..13] returns ["BOOM!", "BOOM!", "BANG!", "BANG!"]

we can draw from multiple lists. this gives all combinations of the given lists
    [x*y | x <- [2,5,10], y <- [8,10,11]]
    returns [16,20,22,40,50,55,80,100,110] 
we can add more predicates
    [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
    returns [55,80,100,110]

let nouns = ["hobo", "frog", "pope"]
let adjectives = ["lazy", "grouchy", "scheming"]
[adjective ++ noun | adjective <- adjectives, noun <- nouns]
returns ["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",
"grouchy pope","scheming hobo","scheming frog","scheming pope"]

we can write our own version of the length function:
    length' xs = sum [1 | _ <- xs]
    the _ means that we don't actually store the item we draw from the list

========================================
TUPLES

tuples are used when:
    -you have non-homogenous data e.g. ("ABC", "DEF", 500)
    -you know how many values are going into the tuple in advance

e.g. to store a list of 3 x-y coordinates we could define a list of tuple of size 2 (pairs)
    [ (1,2), (8,11), (4,5) ]
then if we tried to store an incorrect coordinate
    e.g. [ (1,2), (3,4,5), (6,7) ]
it would throw an error as we are mixing tuples of size 2 (pairs) and size 3 (triples)

tuple methods:
fst returns the first component of a pair
    fst (8,11) returns 8
snd returns the second component of a pair
    snd (8,11) returns 11

zip takes two lists and joins them into one list by matching elements from the lists into pairs
    zip [1..5] ["one", "two", "three", "four", "five"]
    returns [ (1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five") ]
if the lengths of the lists don't match the longer list gets cut off to match the length of the shorter one
    zip [1..] ["apple", "orange"]
    returns [ (1, "apple"), (2, "orange") ]

example problem
find a right triangle that has integers for side lengths and all sides <= 10 which has a perimeter of 24

1. generate all triangles with sides <= 10
    let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

2. only look at right triangles. if we say that c is the hypotenuse, let's say b must be less than c and a must be less than b
    let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <-[1..b], a^2 + b^2 == c^2 ]

3. now only consider the right triangle whose perimeter is 24
    let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <-[1..b], a^2 + b^2 == c^2, a+b+c == 24 ]

-}