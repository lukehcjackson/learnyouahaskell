{-

RECURSION

==========================

    {-
    
    RECURSION

    =========================

    -}

in imperative languages we do computations by declaring how we get to the result
in haskell we do them by declaring what something IS

there are no while loops or for loops in haskell
we often have to use recursion to declare what something is

==================

MAXIMUM EXAMPLE

in an imperative language we'd find a max by:

var max
for item in list
    if item > max
        max = item
return max

in haskell we can define it like

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

we could even rewrite it using the built in max - which gives the max of two numbers

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

==========================

MORE EXAMPLES

let's define replicate
replicate takes an Int and some element and returns a list of that element, some number of times

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

nb: Num is not a subclass of Ord - so to do the n <= 0 we must make sure it's
not only a number but can also be compared using <= (so must be in Ord)

let's implement take
it takes a certain number of elements from the start of a list

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

right off the bat, if n <= 0 we return an empty list
if we try to take any amount from an empty list we also return an empty list
then we actually do the recursive function call
get the first item in the list and call take on the tail, decrementing the count

let's implement reverse

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

we move the head of the list to the tail
when we call reverse it effectively 'cuts off' the head, reducing the size of the list
continue until we are left with an empty list which is concatenated with all of the
rest of the list items in reverse order

let's implement repeat
repeat takes an element and returns an infinite list of that element

repeat' : a -> [a]
repeat' x = x:repeat' x


let's implement zip
zip takes two lists and zips them together
zip [1,2,3] [2,3] returns [(1,2),(2,3)] and truncates the longer list to match the 
size of the smaller one

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

let's implement elem
it takes an element and a list and sees if that element is in the list

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem` xs

=====================================

QUICK SORT

the edge condition is an empty list
a sorted list is a list where:
    all the values smaller than (or equal to) the head are in front (and sorted)
    then there is the head of the list
    then comes all of the values greater than the head (also sorted)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a<-xs, a <= x]
        biggerSorted  = quicksort [a | a<-xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

to understand this:
[5,1,9,4,6,7,3]
5 is the head of the list so we get
[1,4,3] ++ [5] ++ [9,6,7]
call quicksort again on [1,4,3] and [9,6,7]

=================

the edge case for lists is often []
for trees it's often a node with no children
for numbers it's whatever the function definition specifies, maybe 0 or 1, or not...
they are often identities

-}