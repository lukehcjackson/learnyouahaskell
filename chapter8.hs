
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

We have said that a typeclass is a sort of an interface that defines some behaviour.
A type can be made an instance of a typeclass if it supports that behaviour.

For instance, the Int type is an instance of the Eq typeclass because the Eq typeclass
defines behaviour for stuff that can be equauated. And because integers can be equated,
Int is part of the Eq typeclass. 

The real usefulness comes with the functions that act as an interface for Eq, 
namely == and /=. If a type is part of the Eq typeclass, we can use the == functions with
values of that type. That's why expressions like 4 == 4 and "foo" /= "bar" typecheck. 

Typeclasses are often confused with classes from other languages. In those languages,
classes are a blueprint from which we create objects that contain state and do other actions. 
Typeclasses are more like interfaces - we don't make data from typeclasses.
Instead, we first make our data type, then we think about what it can act like. 
If it can act like something that can be equated, we make it an instance of the Eq typeclass. 
If it can act like something that can be ordered, we make it an instance of the Ord typeclass.

In the next section, we will see how we can manually make our types instances of typeclasses
by implementing the functions defined by the typeclasses. But for now, let's see how Haskell can
automatically make our type an instance of any of the following typeclasses: Eq, Ord, Enum, Bounded, Show, Read. 
Haskell can derive the behaviour of our types in these contexts if we use the 'deriving' keyword when making our data type. 

Consider this data type:

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     }

Let's assume that no two people have the exact same first name, last name, and age. 
Now, if we have records for two people, does it makes sense to see if they represent the same person? Sure! 
We can try to equate them and see if they're equal or not. 
That's why it would make sense for this type to be part of the Eq typeclass. 
We'll derive the instance. 

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)

When we derive the Eq instance for a type and then try to compare two values of that type with == or /=, 
Haskell will see if the value constructors match (there's only one value constructor here) and then
it will check if all the data contained inside matches by testing each pair of fields with ==. 
There's only one catch though - the types of all the fields have to be part of the Eq typeclass. 
Since both String and Int are, we're okay here. 

ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
ghci> mca == adRock
False
ghci> mikeD == adRock
False
ghci> mikeD == mikeD
True
ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
True

Since Person is now in Eq, we can use it as the 'a' for all functions that have a 
class constraint of 'Eq a' in their type signature, such as elem:

let beastieBoys = [mca, adRock, mikeD]
mikeD `elem` beastieBoys
returns True

The Show and Read typeclasses are for things that can be converted to or from strings, respectively. 
Like with Eq, if a type's constructors have fields, their type has to be a part of Show or Read if we want
to make our type an instance of them. Let's make our Person data type be a part of Show and Read as well. 

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

Now we can print a person our to the terminal: 

ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> "mikeD is: " ++ show mikeD
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"

Had we tried to print a person on the terminal before making the Person data type part of Show,
Haskell would have complained at us, claiming it doesn't know how to represent a person as a string. 
Now that we've derived a Show instance for it, it does know how. 

Read is pertty much the inverse typeclass of Show. Show is for converting values of our 'a' type
to a String. Read if for converting Strings to values of our type. Remember though, when we use the
read function, we have to use an explicit type annotation to tell Haskell which type we want to get
as a result. If we don't make the type we want as a result explicit, Haskell doesn't know which type we want. 

ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}

If we use the result of our read later on, in a way that Haskell can infer that it should be read
as a Person, we don't have to use type annotation. 

ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
True

We can also read parameterised types, but we have to fill in the type parameters. 
We can't do read "Just 't'" :: Maybe a, but we can do read "Just 't'" :: Maybe Char 

We can derive instances from the Ord typeclass, which is for types that have values that can be ordered. 
If we compare two values of the same type that were made using different constructors, the value which was
made with a constructor that's defined first is considered smaller. 

For instance, consider the Bool type, which can have a value of either False or True. 
For the purpose of seeing how it behaves when compared, we can think of it as being implemented like:

data Bool = False | True deriving (Ord)

Because the False value consructor is specified first, and the True value constructor is specified after it,
we can consider True as greater than False. 

True `compare` False
returns GT

True > False
returns True

True < False
returns False

In the 'Maybe a' data type, the Nothing value constructor is specified before the Just value constructor. 
So a value of Nothing is always smaller than a value of Just something, even if that something is -99999999. 
But if we compare two Just values, then it goes to compare what's inside them. 

ghci> Nothing < Just 100
True
ghci> Nothing > Just (-49999)
False
ghci> Just 3 `compare` Just 2
GT
ghci> Just 100 > Just 50
True

But we can't do something like Just (*3) > Just (*2), because (*3) and (*2) are functions,
which aren't instances of Ord. 

We can easily use algebraic data types to make enumerations, and the Enum and Bounded typeclasses
help us with that. Consider the following data type:

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

Because all the value constructors are nullary (take no parameters) we can make it part of the
Enum typeclass. The Enum typeclass is for things that have predecessors and successors. 
We can also make it a part of the Bounded typeclass, which is for things that have a lowest
and highest possible value. 
While we're at it, let's also make it an instance of all the other derivable typeclasses and see what we can do with it. 

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

Because it's part of the Show and Read typeclasses, we can convert values of this type
to and from Strings:

ghci> Wednesday
Wednesday
ghci> show Wednesday
"Wednesday"
ghci> read "Saturday" :: Day
Saturday

Because it's part of the Eq and Ord typeclasses, we can compare or equate days:

ghci> Saturday == Sunday
False
ghci> Saturday == Saturday
True
ghci> Saturday > Friday
True
ghci> Monday `compare` Wednesday
LT

It's also part of Bounded, so we can get the lowest and highest day

minBound :: Day
returns Monday

maxBound :: Day
returns Sunday

It's also an instance of Enum. We can get predecessors and successors of days,
and we can make list ranges from them. 

succ Monday
returns Tuesday

pred Saturday
returns Friday

[Thursday .. Sunday]
returns [Thursday, Friday, Saturday, Sunday]

[minBound :: maxBound] :: [Day]
returns [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

##### TYPE SYNONYMS #####

Previously, we mentioned that when writing types, the [Char] and String types are equivalent
and interchangeable. That's implemented with type synonyms. 
Type synonyms don't really do anything per se, they're just about giving some types different
names so that they make more sense to someone reading our code and documentation. 

Here's how the standard library defines String as a synonym for [Char]:

type String = [Char]

We've introduced the type keyword. The keyword might be a bit misleading because we're 
not actually making anything new (we did that with the data keyword), we're just making a
synonym for an already existing type. 

If we make a function that converts a string to uppercase, and call it toUpperString,
we can give it a type declaration of
    toUpperString :: [Char] -> [Char]
or
    toUpperString :: String -> String
Both of these are essentially the same, only the latter is nicer to read. 

When we were dealing with the Data.Map module, we first represented a phonebook with
an association list before converting it to a map. As we've already found out, an
association list is a list of key-value pairs. Let's look at a phonebook that we had:

phoneBook :: [(String,String)]
phoneBook =    
    [("betty","555-2938")   
    ,("bonnie","452-2928")   
    ,("patsy","493-2928")   
    ,("lucille","205-2928")   
    ,("wendy","939-8282")   
    ,("penny","853-2492")   
    ]

We see that the type of phoneBook is [(String, String)]
That tells us that it's an association list that maps from strings to strings, but not much else. 
Let's make a type synonym to convey some more information in the type declaration

type PhoneBook = [(String, String)]

Now the type declaration for our phonebook can be
    phoneBook :: PhoneBook

Let's make a type synonym for String as well:

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

Giving the String type synonyms is something that Haskell programmers do when they want
to convey more information about what the Strings in their function should be used as,
and what they represent. 

So now, when we implement a function that takes a name and a number, and sees if that name
and number combination is in our phonebook, we can give it a very pretty and descriptive type declaration:

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

If we decided not to use type synonyms, our function would have a type of
    String -> Stirng -> [(String,String)] -> Bool
In this case, the type declaration that took advantage of type synonyms is easier to understand. 
However, you shouldn't go overboard with them. We introduce type synonyms to either describe what some
existing type represents in our functions (and thus our type declarations become better documentation)
or when something has a long-ish type that's repeated a lot (like [(String, String)]) but represents
something more specific in the context of our functions. 

Type synonyms can also be parameterised. If we want a type that represents an association list type
but still want it to be general, so it can use any type as the keys and values, we can do this:

type AssocList k v = [(k,v)]

Now, a function that gets the value by a key in an association list can have a type of
    (Eq k) => k -> AssocList k v -> Maybe v
AssocList is a type constructor that takes two types, and produces a concrete type, like AssocList Int String, for instance. 

When we talk about concrete types, we mean fully applied types like Map Int String, or if we're
dealing with a polymorphic function, [a] or (Ord a) => Maybe a
'Maybe' is not a type, it's a type constructor. When we apply an extra type to Maybe, like Maybe String, 
then it becomes a concrete type. 
Values can only have types that are concrete types. 

Just like how we can partially apply functions to get new functions, we can partially apply
type parameters and get new type constructors from them. 
Just like we call a function with too few parameters to get back a new function, we can
specify a type constructor with too few type parameters and get back a partially applied type constructor. 

If we wanted a type that represents a map (from Data.Map) from integers to something, we could either do this:

type IntMap v = Map Int v

or we could do it like this:

type IntMap = Map Int

Either way, the IntMap type constructor takes one parameter, and that is the type of whatever the integers will point to. 

If we went to implement this, we would probably do a qualified import of Data.Map 
In that case, type constructors also have to be preceeded with the module name:
    type IntMap = Map.Map Int

Another datatype that takes two types as its parameters is the Either a b type. This is roughly how it's defined:

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

It has two value constructors:
If the Left is used, then its contents are of type a
if Right is used, then its contents are of type b
So we can use this type to encapsulate a value of one type or another, and then when we get a value of type Either a b, we usually pattern match on both Left and Right
and do different stuff based on which one of them it was:

Right 20
returns Right 20

Left "w00t"
returns Left "w00t"

:t Right 'a'
returns Right 'a' :: Either a Char

:t Left True
returns Left True :: Either Bool b

So far, we've seen that 'Maybe a' was mostly used to represent the results of computations that could have either failed or not. 
But sometimes, 'Maybe a' isn't good enough because Nothing doesn't really convey much information other than that something has failed. 
That's fine for functions that can only fail in one way, or if we don't care how and why the function failed. 
A Data.Map lookup fails only if the key we were looking for wasn't in the map, so we know exactly what happened. 
However, when we're interested in HOW some function failed or WHY, we usually use the result type of Either a b, wehre a is some sort of type that
can tell us something about the possible failure and b is the type of a successful computation. Hence, errors use the Left value constructor while results use Right. 

Example: A high school has lockers. Each locker has a code combination. When a student wants a new locker, they tell the supervisor which locker number they want and he gives them the code. 
However, if someone is already using that locker, he can't tell them the code for the locker and they have to pick a different one. 
We'll use a map from Data.Map to represent the lockers. It'll map from locker numbers to a pair of whether or not the locker is in use, and the locker code. 

import qualified Data.Map as Map
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

We introduce a new data type to represent whether a locker is taken or free
We make a type synonym for the locker code
We also make a type synonym for the type that maps from integers to pairs of locker state and locker code

Now we will make a function that searches for the code in a locker map.
We will use an Either String Code type to represent our result, becauses the lookup can fail in two ways:
The locker could be taken, in which case we can't tell the code OR the locker number might not exist at all. 
If the lookup fails, we will use a String to say what's happened. 

lockerLookup :: Int -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker Number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code 
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

We do a normal lookup in the map. 
If we get a Nothing, we return a value of type Left String, saying that the locker doesn't exist at all. 
If we do find the key, we do an additional check to see if the locker is taken.
If it is, return a Left saying that it's already taken.
If it isn't return a value of type Right Code, in which we give the student the correct code for the locker. 
It's actually a Right String, but we introduced the Code = String type synonym to introduce some additional context / documentation into the type declaration. 

Here's an example map:

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

Now let's try our function:

ghci> lockerLookup 101 lockers  
Right "JAH3I"  
ghci> lockerLookup 100 lockers  
Left "Locker 100 is already taken!"  
ghci> lockerLookup 102 lockers  
Left "Locker number 102 doesn't exist!"  
ghci> lockerLookup 110 lockers  
Left "Locker 110 is already taken!"  
ghci> lockerLookup 105 lockers  
Right "QOTSA"  

We could have used a 'Maybe a' to represent the result, but then we wouldn't know why we couldn't get the code. 
But now, we have information about the failure in our result type. 

##### RECURSIVE DATA TYPES #####

We have seen that a constructor in an algebraic data type can have several (or zero) fields, and each field must be of some concrete type. 
With that in mind, we can make types whose constructors have fields that are of the same type!
Using that, we can create recursive data types, where one value of some type contains values of that type, which in turn contain more values of the same type, and so on... 

[5] is just syntactic sugar for 5:[].
On the left side of the : is a value, and on the right side, there's a list. In this case, the empty list. 
[4,5] is the same as 4:(5:[])
Looking at the first :, it also has a value on the left (4) and a list (5:[]) on the right.
The same is true for [3,4,5,6] = 3:(4:(5:(6:[]))) = 3:4:5:6:[]

We could say that a list can be an empty list, or it can be an element joined together with a : to another list (that can be [] or not)

Let's use algebraic data types to implement our own list:

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

This reads just like our definition of lists from earlier. It's either an empty list, or a combination of a head with some value and a list. 
If this is confusing we can also write it in record syntax:

data List a = Empty | Cons { listHead :: a, listTail :: List a } deriving (Show, Read, Eq, Ord)

Cons is another word for :
In lists, : is actually a constructor that takes a value and another list, and returns a list. 

We can already use our new list type. It has two fields - one of type 'a' and one of type '[a]'. 

Empty
returns Empty

5 `Cons` Empty
returns Cons 5 Empty

4 `Cons` (5 `Cons` Empty)
returns Cons 4 (Cons 5 Empty)

Calling Cons in an infix manner shows how it is just like :
Empty is like []
4 `Cons` (5 `Cons` Empty) is like 4:(5:[])

We can define functions to be automatically infix, by making them comprised only of special characters. 
We can also do the same with constructors, since they're just functions that return a data type:

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

First off, note the new syntactic construct: the fixity declaration. 
When we define functions as operators, we can use that to give them a fixity (but we don't have to!)
A fixity states how tightly the operator binds, and whether it's left-associative or right-associative. 
For instance, *'s fixity is infixl 7 *, and +'s fixity is infixl 6
That means they're both left-associative => (4 * 3 * 2) = ((4 * 3) * 2)
but * binds tighter than +, because it has a greater fixity, so 5 * 4 + 3 = (5 * 4) + 3

Otherwise, we just wrote a :-: (List a) instead of Cons a (List a)
Now we can write out lists in our list type like this:

3 :-: 4 :-: 5 :-: Empty
returns 3 ((:-:) 4 ((:-:) 5 Empty))

When deriving Show for our type, Haskell will still display it as if the constructor was a prefix function, hence the parenthesis (:-:)
    remember 4 + 3 is the same as (+) 4 3

Let's make a function that adds  two of our lists together. This is how ++ is defined for normal lists:

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

So let's just steal that for our own list. Let's call the function .++ 

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

And let's see if it works:

    ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
    ghci> let b = 6 :-: 7 :-: Empty  
    ghci> a .++ b  
    (:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))  

Great! 
If we wanted to, we could implement all of the functions that operate on lists for our own type. 

Notice how we pattern matched on (x :-: xs). That works because pattern matching is actually about matching constructors. 
We an match on :-: because it is a constructor for our own list type, and we can also match on : because it is a constructor for the built-in list type. 
The same goes for []
Because pattern matching works (only) on constructors, we can match for stuff like that, normal prefix constructors, and things like 8 or 'a', which are essentially constructors for the numeric and character types. 

Let's implement a binary search tree. 
An element points to two elements, one on it's left and one on it's right
The element to the left is smaller, the element to the right is bigger
Each of those elements can also point to two elements (or one, or none)
In effect, each element has up to two sub-trees. 
We know that all the elements in the right sub-tree of, say, 5, will be smaller than 5. 
If we needed to find 8 and started at 5, we would go right because 8 > 5. We land at 7 => go right again because 7 > 8.

Sets and maps from Data.Set and Data.Map are implemented using trees, only instead of normal binary search trees, they use balanced binary search trees - which are always balanced. 

Let's implement a BST. 
Let's say a tree is either an empty tree, or it's an element that contains some value and two trees. 
Let's use an algebraic data type:

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

Instead of manually building a tree, we're going to make a function that takes a tree and an element, and inserts the element. 
We do this by comparing the value we want to insert with the root node. If it's smaller -> go left, if it's larger -> go right. 
Keep going for every subsequent node until we reach an empty tree. 
Once we reach an empty tree, insert a node with that value instead of the empty tree. 

In C we would do this by modifying the pointers and values inside the tree. 
In Haskell, we can't really modify the tree - we have to make a new sub-tree every time we go left or right, and in the end return a whole new tree. 
Hence, the type for the insertion function is going to be something like a -> Tree a -> Tree a
It takes an element and a tree, and returns a new tree that has that element inside. 

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

The singleton function is a shortcut for making a node that has some value and two empty sub-trees.
In the insertion function, we first have the edge condition (inserting into an empty tree) as a pattern.
    If we've reached an empty sub-tree, that means we're where we want to be. Instead of the empty tree, we put a singleton tree with our element. 
If we're not inserting into an empty tree, then we have to check some things. 
First off, if the element we're inserting is equal to the root element, just return a tree that's the same. 
If it's smaller than the root, return a tree with the same root value, the same right sub-tree, but instead of the left sub tree, put a tree that has our value inserted into it. 
The same goes (but the other way around) for if our value we're inserting is larger than the root. 

Let's make a function that checks if an element is in the tree. 
First define the edge condition - if we're looking for an element in an empty tree, then it's not in there. 
This is the same edge condition as when we search for an item in an empty list!
If we're not searching in an empty tree, then we check some things:
If the element in the root node is what we're searching for, great
Otherwise, if the element we're searching for is smaller than the root node, search the left sub-tree
Otherwise, search the right sub-tree

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

Let's try it out
Instead of manually building a tree let's use a fold to build a tree from a list

ghci> let nums = [8,6,4,1,7,3,5]  
ghci> let numsTree = foldr treeInsert EmptyTree nums  
ghci> numsTree  
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

        5
    3       7
  1   4   6   8

In that foldr, treeInsert was the folding function (it takes a tree and a list element, and produces a new tree) and EmptyTree was the starting accumulator.
nums was of course the list we were folding over. 

    ghci> 8 `treeElem` numsTree  
    True  
    ghci> 100 `treeElem` numsTree  
    False  
    ghci> 1 `treeElem` numsTree  
    True  
    ghci> 10 `treeElem` numsTree  
    False  

##### TYPECLASSES 102 #####

So far, we've seen some of the standard Haskell typeclasses and we've seen which types are in them. 
We've also learned how to automatically make our own types instances of the standard typeclasses by asking Haskell to derive the instances for us. 
In this section, we're going to learn how to make our own typeclasses, and how to make types instances of them by hand. 

A quick recap on typeclasses: typeclasses are like interfaces. 
A typeclass defines some behaviour (like comparing for equality, comparing for ordering, enumeration)
and then types that can behave in that way are made instances of that typeclass. 
The behaviour of typeclasses is achieved by defining functions, or just type declarations that we then implement. 
So when we say a type is an instance of a typeclass, we mean that we can use the functions that the typeclass defines with our type. 

Typeclasses have pretty much nothing to do with classes like in Java, Python, ...

The Eq typeclass is for stuff that can be equated. 
It defines the functions == and /=. 
If we have a type (say Car) and comparing two cars with the equality function == makes sense, then it makes sense for Car to be an instance of Eq. 
This is how the Eq class is defined in the standard prelude:

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

First off, when we write 'class Eq a where', this means we're defining a new typeclass and that's called Eq. 
The 'a' is the type variable, and it means that 'a' will play the role of the type that we will soon be making an instance of Eq. 
It doesn't have to be called 'a' or even be one letter, it just has to be a lowercase word. 
Then, we define several functions. It's not mandatory to implement the function bodies themselves, we just have to specify the type declarations for the functions. 

It might be easier to understand if we instead wrote
    class Eq equatable where
and then specified the type declarations like
    (==) :: equatable -> equatable -> Bool

We DID actually implement the function bodies for the function Eq defines, only we defined them in terms of mutual recursion. 
We said that two instances of Eq are equal if they are not different, and they are different if they are not equal. 
We didn't HAVE to do this, but we will see how it helps us out soon. 

If we have 'class Eq a where' which has a type declaration '(==) :: a-> a-> Bool',
then when we later examine the type of that function, it will have a type of '(Eq a) => a -> a -> Bool'

So once we have a class, what can we do with it?
Not much.
But once we start making types instances of that class, we start getting some nice functionality. 
Have a look at this type:

data TrafficLight = Red | Yellow | Green

It defines the state of a traffic light. We didn't derive any class instances for it. 
That's because we're going to write some instances for it by hand, even though we COULD derive them for types like Eq and Show.
Here's how we make it an instance of Eq:

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

We did this using the 'instance' keyword:
    'class' is for defining new typeclasses
    'instance' is for making our types instances of typeclasses

When we were defining Eq, we wrote 'class Eq a where' and we said that 'a' takes the role of whichevr type will be made an instance later on. 
We can see that here, because when we're making an instance, we write 'instance Eq TrafficLight where'. We replace 'a' with the actual type. 

Because == was defined in terms of /= and vice versa in the class declaration, we only had to overwrite one of them in the instance declaration. 
That's called the minimal complete definition for the typeclass - the minimum of functions that we have to implement so that our type can behave like the class advertises. 
To fulfill the minimal complete definition for Eq, we have to overwrite either one of == or /=. 
If Eq was defined simply like:

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

we would have to implement both of these functions when making a type an instance of it, because Haskell wouldn't know how these two functions are related. 
In this case the minimal complete definition would be both == and /=. 

You can see that we implemented == by doing pattern matching. 
Since there are many more cases where two lights are not equal, we just specify the ones that ARE equal 
and then do a catch-all pattern saying that if it's none of the previous combinations, then the two lights aren't equal. 

Let's make an instance of Show by hand. 
To satisfy the minimal complete definition for Show, we just have to implement its show function, which takes a value and turns it into a String. 

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

Once again, we use pattern matching. 
Let's see it in use:

    ghci> Red == Red  
    True  
    ghci> Red == Yellow  
    False  
    ghci> Red `elem` [Red, Yellow, Green]  
    True  
    ghci> [Red, Yellow, Green]  
    [Red light,Yellow light,Green light]  

We could have just derived Eq and it would have had the same effect. 
However, deriving Show would have just directly translated the value constructors to Strings. 
But if we want lights to appear like "Red light", then we have to make the instance declaration by hand. 

You can also make typeclasses that are subclasses of other typeclasses. 
The class declaration for Num is a bit long, but here's the first part:

class (Eq a) => Num a where
    ...

As we mentioned previously, there are a lot of places where we can cram in class contraints. 
This is just like writing 'class Num a where', only we state that our type 'a' must be an instance of Eq. 
We're essentially saying that we have to make a type an instance of Eq before we can make it an instance of Num. 
Before some type can be considered a number, it makes sense that we can determine whether values of that type can be equated or not. 
That's all there is to subclassing, really. It's just a class constraint on a class declaration. 
When defining function bodies in the class declaration, or when defining them in instance declarations, we can assume that 'a' is part of Eq and so we can use == on values of that type. 

How are the Maybe or list types made as instances of typeclasses?
What makes Maybe different from, say, TrafficLight is that Maybe in itself isn't a concrete type. 
It's a type constructor that takes one type parameter (like Char or something) to produce a concrete type (like Maybe Char)
Let's take a look at the Eq typeclass again:

class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y) 

From the type declarations, we can see that the 'a' is used as a concrete type, because all the types in functions have to be concrete
    (you can't have a function of type a -> Maybe but you can have a -> Maybe a or Maybe Int -> Maybe String)
That's why we can't do something like

instance Eq Maybe where
    ...

Because, like we've seen, the 'a' has to be a concrete type but Maybe isn't one. 
It's a type constructor that takes one parameter and produces a concrete type. 
It would also be tedious to write 'instance Eq (Maybe Int) where' , 'instance Eq (Maybe Char) where' , ... for every type ever. 
So we could write it out like so:

instance Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

This is like saying that we want to make all types of the form 'Maybe something' an instance of Eq. 
We even could have written 'instance Eq (Maybe something) where' but Haskell style is usually to use single letters. 
The (Maybe m) here plays the role of the 'a' from 'class Eq a where'. 
While Maybe isn't a concrete type, Maybe m is. 
By specifying a type parameter (m), we said that we want all types that are in the form 'Maybe m', where m is any type, to be an instance of Eq. 

There's a problem with this, however. 
We use == on the contents of the Maybe, but we have no assurance that what the Maybe contains can be used with Eq!
So we have to modify our instance declaration like this:

instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

We had to add a class contraint. 
With this instance declaration, we say this:
We want all types of the form 'Maybe m' to be part of the Eq typeclass, but only those types where the m is also part of Eq. 
This is actually how Haskell would derive the instance too!

Most of the time, class constraints in class declarations are used for making a typeclass a subclass of another typeclass
and class constraints in instance declarations are used to express requirements about the contents of some type. 
For instance, here we have required the contents of Maybe to also be part of the Eq typeclass. 

When making instances, if you see that a type is used as a concrete type in the type declarations (like the a in a -> a -> Bool),
you have to supply type parameters and add parentheses so you end up with a concrete type.

If you want to see what the instances of a typeclass are, just do :info YourTypeClass in ghci. 
So :info Num will show which functions the typeclass defines, and it will give you a list of the types in the typeclass. 
:info works for types and type constructors too. :info Maybe gives you the typeclasses that Maybe is an instance of. 
:info can also show the type declaration of a function. 

#### A YES-NO TYPECLASS ####

In JavaScript, you can do something like
    if ("WHAT") alert ("YEAH") else alert ("NO")
This will alert "YEAH" because JavaScript considers a non-empty string to be a truthy value. 

Even though stricly using Bool for boolean semantics works better in Haskell, let's try and implement that JavaScript-ish behaviour anyway 

We will start out with a class declaration

class YesNo a where
    yesno :: a -> Bool

The YesNo typeclass defines one function yesno. That function takes one value of a type that can be considered to hold some concept of true-ness,
and tells us for sure if it's true or not. 
Notice that from the way we use the 'a' in the function, 'a' has to be a concrete type. 

Let's define some instances. 
For numbers, we'll assume that (like in JS) any number that isn't 0 is true-ish and 0 is false-ish

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

Empty lists (and by extension, Strings) are a no-ish value, while non-empty lists are a yes-ish

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

Notice how we just put a type parameter 'a' in there to make the list a concrete type, even though we don't make any assumptions about the type that's contained in the list. 

Bool itself also holds true-ish-ness and false-ish-ness:

instance YesNo Bool where
    yesno = id

id is a standard library function that takes a parameter and returns the same thing,
which is what we would have written anyway. 

Let's make 'Maybe a' an instance too

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

We didn't need a class constraint because we made no assumption about the contents of the Maybe. 
We just said that it's true-ish if it's a Just value, and false-ish if it's a Nothing. 
We still had to write out (Maybe a) instead of just Maybe, because a Maybe -> Bool function can't exist, but Maybe a -> Bool is fine. 

We previously defined a Tree type, which represents a BST. 
We can say an empty tree is false-ish and anything that's not an empty tree is true-ish. 

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

Can a traffic light be a yes or no value?
Sure. If it's red you stop, if it's green you go. Yellow? You still probably go

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

Now that we have some instances we can try this out:

ghci> yesno $ length []  
False  
ghci> yesno "haha"  
True  
ghci> yesno ""  
False  
ghci> yesno $ Just 0  
True  
ghci> yesno True  
True  
ghci> yesno EmptyTree  
False  
ghci> yesno []  
False  
ghci> yesno [0,0,0]  
True  
ghci> :t yesno  
yesno :: (YesNo a) => a -> Bool

That works. 
Let's make a function that mimics the if statement, but it works with YesNo values

yesnoIf : (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

ghci> yesnoIf [] "YEAH!" "NO!"  
"NO!"  
ghci> yesnoIf [2,3,4] "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf True "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf (Just 500) "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf Nothing "YEAH!" "NO!"  
"NO!"  



-}