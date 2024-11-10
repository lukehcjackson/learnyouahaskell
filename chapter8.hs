
{-
MAKING OUR OWN TYPES AND TYPECLASSES

#### ALGEBRAIC DATA TYPES ####

One way to define our own data type is to use the data keyword
For instance, Bool is defined like:

data Bool = False | True

data means we are defining a new data type. The part before the = denotes the type, which in this case is Bool. 
The parts after the = are value constructors. They specify the different values taht this type can have.
The | is read as 'or'. We can read this as: the Bool type can have a value of True or False. Both the type name and the value constructors have to be capital cased. 

We can think of the Int type as being defined like:

data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  

The first and last value constructors are the minimum and maximum possible values of Int.
It's not actually defined like this!!

How would we represent a shape in Haskell?
We could use tuples - and denote a circle like (43.1, 55.0, 10.4)
where the first and second fields are the coordinates of the circle's center, and the third field the radius. 
However, this could also represent something else, like a 3D vector. 

A better solution would be to make our own type to represent a shape. Let's say that a shape can be a circle or a rectangle. 

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

The Circle value constructor has three fields, which take floats. 
When we write a value constructor, we can optionally add some types after it, and those types define the values it will contain. 
Here, the first two fields are the coordinates of its center and the third the radius. 
The Rectangle value constructor has four fields, which accept floats. 
The first two are the coordinates of the upper left corner, and the second two are the coordinates of the lower right corner. 

When we have said 'fields' we really mean parameters. Value constructors are actually functions that ultimately return a value of a data type. 
Let's look at the type signatures for these two value constructors. 

:t Circle
returns Circle :: Float -> Float -> Float -> Shape

:t Rectangle
returns Rectangle :: Float -> Float -> Float -> Float -> Shape

Let's write a function that takes a shape and returns its surface area. 

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r * 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

The first thing to note is the type declaration. 
This function takes a Shape and returns a Float. 
We couldn't write Circle -> Float because Circle is not a type, Shape is. Just like we can't write True -> Int, we write Bool -> Int. 
The next thing to note is that we can pattern match against constructors. We have done this before when pattern matching against [] or False or ...

surface $ Circle 10 20 10
returns 314.15927

surface $ Rectangle 0 0 100 100
returns 10000.0 

However, if we just write out Circle 10 20 5 in the ghci we will get an error. This is because Haskell doesn't know how to display our data type as a String (yet)
Remember that when we try to print a value out in the prompt, Haskell runs the show function to get the string representation.
To make our Shape type part of the Show typeclass, we modify it like this:

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

For now, let's just say that adding 'deriving (Show)' at the end of a data declaration, Haskell magically makes that type part of the Show typeclass. 
We can now do this:

Circle 10 20 5
returns Circle 10.0 20.0 5.0 

Rectangle 50 230 60 90
returns Rectangle 50.0 230.0 60.0 90.0 

Value constructors are functions, so we can map them and partially apply them and everything. 
If we want a list of concentric circles with different radii, we can do this:

map (Circle 10 20) [4,5,6,7]
returns [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 7.0] 

Let's make an intermediate data type that defines a point in 2D space
We can then use that to make our shapes more understandable:

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

Notice that when defining Point, we used the same name for the data type and the value constuctor. 
This has no special meaning, although it's common to use the same name as the type if there's only one value constructor. 
Now the Circle has two fields, one of type Point and one of type Float. 
We have to adjust our surface function to reflect these changes:

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

The only thing we had to change were the patterns on the left. We disregarded the whole Point in the Circle pattern. 
In the Rectangle pattern, we just used a nested pattern matching to get the fields of the Points. If we wanted to reference the points themselves, we could have used as-patterns. 

ghci> surface (Rectangle (Point 0 0) (Point 100 100))  
10000.0  
ghci> surface (Circle (Point 0 0) 24)  
1809.5574 

How about a function that nudges a shape? It takes a Shape, the amount to move it on the x axis, and the amount to move it on the y axis. 
It returns a new shape that has the same dimensions, only it's located somewhere else. 

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

nudge (Circle (Point 34 34) 10) 5 10
returns Circle (Point 39.0 44.0) 10.0 

If we don't want to deal with Points directly, we can make some auxiliary functions that create shapes of some size at (0,0) and then nudge those. 

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

then

nudge (baseRect 40 100) 60 23
returns Rectangle (Point 60.0 23.0) (Point 100.0 123.0)

You can export your data types into modules. To do that, write your type along with the functions you are exporting.
Then, add some parenthesis and in them specify the value constructors you want to export for it, separated by commas. 
If you want to export all the value constructors for a given type, just write '..' 

If we wanted to export the functions and types we have defined here in a module, we could start it off like this:

module Shapes   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where 

By doing Shape(..), we exported all the value constructors for Shape, so that means whoever imports our module can make Shapes using the Rectangle and Circle value constructors.
It's the same as writing Shape(Rectangle, Circle)

We could also opt to not exprt any value constructors, by just writing Shape
That way, someone importing our module could only make shapes using the auxiliary functions baseCircle and baseRect. 

Data.Map uses that approach - you can't write Map.Map [(1,2),(3,4)] because it doesn't export that value constructor. 
You have to use one of the auxiliary functions like Map.fromList 

##### RECORD SYNTAX #####

We've been tasked with creating a data type to describe a person. 
The info that we want to store about the person is: first name, last name, age, height, phone number, and favourite ice cream flavour. 

data Person = Person String String Int Float String String deriving (Show)

Let's make a person:

let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
guy  
returns Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate" 

What if we want to create a function to get a specific piece of information about a person? We could write a function to get the first name, the last name, etc... 

firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor     

This is very very cumbersome to write, but it does work:

ghci> firstName guy  
"Buddy"  
ghci> height guy  
184.2  
ghci> flavor guy  
"Chocolate" 

In this sort of scenario, there is an alternate way to write data types. This is called record syntax:

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

Instead of just naming the field types one after another and separating them with spaces, we use curly brackets. 
First we write the name of the field, like 'firstName', then the :: (amazingly this is called Paamayim Nekudotayim in Hebrew), then the type. 
The result is exactly the same. The main benefit is that this creates functions that lookup fields in the data type. 
Haskell automatically makes firstName, lastName, age, height, phoneNumber and flavor functions. 

:t flavor
returns flavor :: Person -> String

There's another benefit to record syntax. When we derive Show, it displays differently if we use record syntax. Compare

    data Car = Car String String Int deriving (Show)  

    ghci> Car "Ford" "Mustang" 1967  
    Car "Ford" "Mustang" 1967  

to 

    data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  

    ghci> Car {company="Ford", model="Mustang", year=1967}  
    Car {company = "Ford", model = "Mustang", year = 1967}

##### TYPE PARAMETERS #####

A value constructor can take some values as parameters and produce a new value.
For instance, the Car constructor takes three values and produces a Car value. 

Similarly, type constructors can take types as parameters and produce a new type. 
Let's see how a type we've already met is implemented:

data Maybe a = Nothing | Just a

The 'a' here is the type parameter. Because there's a type parameter involved, we call Maybe a type constructor. 
Depending on what we want this data type to hold when it's not Nothing, this type constructor could end up produce a type of Maybe Int, Maybe Car, Maybe String , ...
No value can just a type of 'Maybe', because that's not a type, it's a type constructor. 
In order for this to be a real type that a value can be part of, it has to have all its type parameters filled up. 

If we pass Char as the type parameter to Maybe, we get a type of Maybe Char. The value Just 'a' has a type of Maybe Char, for example. 

Another type that has a type parameter is the list type! Values can have an [Int] type, a [Char] type, a [[String]] type, but not just a type of [].

:t Just "Haha"
returns Just "Haha" :: Maybe [Char]

:t Just 84
returns Just 84 :: (Num t) => Maybe t

:t Nothing
returns Nothing :: Maybe a

Just 10 :: Maybe Double
returns Just 10.0 

Type parameters are useful because we can make different types with them depending on what kind of types we want contained in our data type. 
When we do :t Just "Haha", the type inference engine figures it out to be of the type Maybe [Char], because if the a in Just a is a String, then the a in Maybe a must also be a String. 

Notice that the type of Nothing is Maybe a. It's type is polymorphic. If some function requires a Maybe Int as a parameter, 
we can give it a Nothing, because a Nothing doesn't contain any value anyway so it doesn't matter.
The Maybe a type can act like a Maybe Int if it has to, just like how 5 can act as an Int or a Double.
The type of the empty list is [a] - an empty list can act like a list of anything!
That's why we can do both [1,2,3] ++ [] and ["ha","ha","ha"] ++ []

Using type parameters is very beneficial, but only when it makes sense. 
Usually we use them when our data type would work regardless of the type of value it holds inside it, like with our 'Maybe a' type. 
If our type acts as some kind of box, it's good to use them. 
We could change our Car data type from this:

    data Car = Car { company :: String  
                   , model :: String  
                   , year :: Int  
                   } deriving (Show)  
                
to this:

    data Car a b c = Car { company :: a  
                         , model :: b  
                         , year :: c   
                         } deriving (Show)  

But would we really benefit? Probably not, because we would end up defining functions that only work on the Car String String Int type. 

For instance, for our first definition of Car, if we wanted a function to display the car's properties in a sentence:

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

but if Car was Car a b c:

tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

We have to force this function to take a Car type of (Show a) => Car String String a
The type signature is more complicated and the only benefit we get is that we can use any type that's an instance of the Show typeclass as the type for c. 
In reality, we'd end up using Car String String Int most of the time, so type parameterizing Car isn't really worth it. 

We usually use type parameters when the type that's contained inside the data type's various value constructors isn't really that important for the type to work. 
A list of stuff is a list of stuff, regardless of what the stuff is. 
If we want to sum a list of numbers, maybe in the summing function we specify that we specifically want a list of numbers. 

Another example of a parameterised type that we've already met is Map k v from Data.Map 
The k is the type of the keys in the map, and the v is the type of the values. 
This is a good example of a place where type parameters are really useful. 
Having maps parameterised allows us to have mapping from any type to any other type, as long as the type of the key is in the Ord typeclass. 
If we were defining a mapping type, we could add a typeclass contraint in the data declaration:

data (Ord k) => Map k v = .......

HOWEVER, it's a very strong convention in Haskell to NEVER add typeclass constraints in data declarations. 
Why?
We don't benefit a lot, and we have to write more class constraints, even when we don't need them. 
Regardless of whether we put the constraint into the data declaration, we have to put the (Ord k) => constraint into functions that assume the keys in a map can be ordered. 
However, for functions where we don't care whether or not they have to be ordered, if we constrained the data declaration we still would have to specify (Ord k) => anyway. 

An example of this in Data.Map is toList
It's type signature is toList :: Map k a -> [(k,a)]
If Map k v had a type constraint in it's data declaration, the type for toList would be
toList :: (Ord k) => Map k a -> [(k,a)]
even though the function doesn't do any comparing of keys by order. 

Let's implement a 3D vector type and add some operations for it. 
We'll use a parameterised type because even though it will usually contain numeric types, it will still support several of them. 

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `plus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

dotProduct :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `dotProduct` (Vector l m n) = i*l + j*m + k*n

These functions can operator on types of Vector Int, Vector Integer, Vector Float, anything as long as the a in Vector a is from the Num typeclass. 
If you examine the type declarations for these functions you will see that they only operate on vectors of the same type, and the numbers involved must be of the same type that is contained in the vectors. 
Note that we didn't put a Num class constraint in the data declaration, because we would have had to repeat it anyway. 

It's important to distinguish between the type constructor and the value constructor. 
When declaring a data type, the part before the = is the type constructor, and the constructors after it (possible separated by |) are value constructors.
Giving a function a type of Vector t t t -> Vector t t t -> t would be wrong, because we have to put types in type declaration and the vector type constructor only takes one parameter, whereas the value constructor takes three. 

##### DERIVED INSTANCES ##### 

-}