{-
INPUT AND OUTPUT

We've mentioned that Haskell is a purely functional language. 
Whereas in imperative languages you usually get things done by giving the computer a 
series of steps to execute, functional programming is more of defining what stuff IS. 

In Haskell, a function can't change some state, like changing the contents of a variable
(when a function changes state, we say that the function has side-effects)
The only thing a function can do in Haskell is give us back some result, based on the 
parameters that we gave it. 
If a function is called two times with the same parameters, it has to return the same result. 

This may seem a bit limiting coming from an imperative world. 
However, in an imperative language, you have no guarantee that a simple function that should
just crunch some numbers won't burn down your house while crunching those numbers. 

For instance, when we were making a binary search tree, we didn't insert an element into a tree
by modifying some tree in place. Our function for inserting into a binary search tree actually
returned a new tree, because it can't change the old one. 

While functions being unable to change state is good because it helps us reason about our programs,
there's one problem with that:
If a function can't change anything in the world, how is it supposed to tell us what's calculated?
In order to tell us the results, it has to change the state of an output device (the screen)

Haskell has a clever system for dealing with functions that have side effects, that neatly
separates the part of our program that is pure, and the part that is impure, which does all
the dirty work like talking to the keyboard and the screen. 
With those two parts separated, we can still reason about our pure program, and take
advantage of all the things that purity offers, like laziness, robustness, modularity
while effectively communicating with the outside world. 

##### HELLO WORLD #####

Until now we have been running code using the ghci
Now we will write our first 'real' Haskell program - hello world. 

In helloworld.hs, write
    main = putStrLn "hello world"

Compile and run it:
    ghc --make helloworld
    ./helloworld

Gives us
    hello world

Let's examine what we wrote. 
First off, let's look at the type of the function putStrLn:

:t putStrLn
returns putStrLn :: String -> IO ()

:t putStrLn "hello, world"
returns putStrLn "hello, world" :: IO ()

We can read the type of putStrLn like this:
putStrLn takes a String and returns an I/O Action that has a result
type of () (i.e. the empty tuple, also known as a unit)

An I/O action is something that, when performed, will carry out an action with a side effect
(that's usually either reading from the input or printing stuff to the screen)
and will also contain some kind of return value inside it. 

Printing a string to the terminal doesn't really have any kind of meaningful return value,
so a dummy value of () is used. 

The empty tuple is a value of () and also has a type of (). 

So when will an I/O action be performed?
This is where 'main' comes in. 
An I/O action will be performed when we give it a name of main, and then run our program. 

Having your whole program be just one I/O action seems limiting. 
That's why we can use 'do' syntax to glue together several I/O actions into one. 
Take a look at the following example:

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock")

This reads pretty much like an imperative program. 
If you compile it and try it out, it will probably behave just like you expect it to. 
Notice that we said 'do' and then laid out a set of steps, like we would in an imperative program. 
Each of these steps is an I/O action. 
By putting them together with 'do', we glued them into one I/O action. 
The action that we got has a type of IO (), because that's the type of the last I/O action inside. 

Because of that, main always has a type signature of 
    main :: IO something
where something is some concrete type. 
By convention, we usually don't specify the type declaration for main. 

An interesting thing that we haven't met before is the third line:
    name <- getLine
It looks like it reads a line from the input and stores it in a variable called 'name'
Does it really do that?
Let's look at the type of getLine

:t getLine
returns getLine :: IO String

getLine is an I/O action that contains a result type of String
That makes sense, because it will wait for the user to input something at the terminal,
and then that something will be represented as a String. 

So what's up with name <- getLine?
You can read it like this:
perform the I/O action getLine, and then bind its result value to name
getLine has a type of IO String, so name will have a type of String. 

You can think of an I/O action as a box, which goes out into the real world and does
something there and maybe bring back some data. 
Once it's fetched that data for you, the only way to open the box and get the data inside
is to use the <- construct. 
And if we're taking data out of an I/O action, we can only take it out when we're inside
another I/O action. 

This is how Haskell manages to neatly separate the pure and impure parts of our code. 
getLine is, in a sense, impure, because its result value is not guaranteed to be the same
when performed twice. 
That's why it's sort of 'tainted' with the IO type constructor, and we can only get that
data out in I/O code. 
And because I/O code is tainted too, any computation that depends on tainted I/O data will
have a tainted result.

When we say 'tainted' here, we don't mean tainted in the sense that we can never use the
result contained in an I/O action ever again in pure code. 
We temporarily un-taint the data inside an I/O action when we bind it to a name. 
When we do name <- getLine, name is just a normal string, because it represents what's inside the box. 

We can have a really complicated function that, say, takes your name (a normal string) as a
parameter and tells you your fortune and your whole life's future based on your name.
We can do this:

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Read your future: " ++ tellFortune name 

and tellFortune (or any of the functions it passes 'name' to) doesn't have to know
anything about I/O - it's just a normal String -> String function. 

Is this piece of code valid?

nameTag = "Hello, my name is " ++ getLine

No! 
++ requires both of it's parameters to be lists over the same type
The left parameter is of type String (or [Char] !), whilst getLine has a type of IO String. 
You can't concatenate a String and an I/O action. 
We first have to get the result out of the I/O action to get a value of type String,
and the only way to do that is to say something like
    name <- getLine
inside some other I/O action. 
If we want to deal with impure data, we have to do it in an impure environment. 

Every I/O action that gets performed has a result encapsulated within it. 
That's why our previous example program could have also been written like this:

main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock")

However, foo would have a value of (), so that's kind of pointless. 
Notice we didn't bind the last putStrLn to anything. 
That's because, in a do block, the last action cannot be bound to a name. 
We'll see why that is when we do monads. 
For now, think of it like the do block automatically extracts the value of the last
action, and binds it to its own result. 

Except for the last line, every line in a do block that doesn't bind
can also be written with a bind. 
So putStrLn "blah" can be written as _ <- putStrLn "blah"
That's useless, though, so we leave out the <- for I/O actions that don't contain an important result. 

Beginners sometimes think that
    name = getLine
will read from the input and then bind the value of that to 'name'. 
It won't! This just gives the getLine I/O action a different name, called 'name'. 
Remember, to get the value out of an I/O action, you have to perform it inside
another I/O action by binding it to a name with <-

I/O actions will only be performed when they are given a name of 'main', or when they're
inside a bigger I/O action that we composed with a 'do' block. 
We can also use a 'do' block to glue together a few I/O actions, that we can eventually
use that I/O action in another 'do' block, and so on... 
Either way, they'll only be performed when they fall into 'main'. 

There's also one more case when I/O actions will be performed. 
When we type out an I/O action in the GHCI and press return, it will be performed. 

ghci> putStrLn "HEY"
"HEY"

Even when we just punch out a number or call a function in GHCI and press return, it will
evaluate it (as much as it needs) and then call 'show' on it, and then it will print out
that string to the terminal using putStrLn implicitly. 

Remember let bindings? They have to be in the form of
    let bindings in expression
where bindings are names to be given to expressions, and expression is the expression
that is to be evaluated that sees them. 
We also said that in list comprehensions, the 'in' part isn't needed. 
Well, you can use them in 'do' blocks pretty much like you use them in list comprehensions:

import Data.Char 

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

See how the I/O actions in the 'do' block are lined up?


-}