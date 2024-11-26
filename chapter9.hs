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
Also notice how the 'let' is lined up with the I/O actions, and the names of the 'let'
are lined up with each other?
That's good practice, because indentation is important in Haskell. 

We did map toUpper firstName, turning something like "John" into "JOHN"
We bound that uppercased string to a name (bigFirstName) and then used it in a string
later on that we printed to the terminal. 

You might be wondering when to use <-, and when to use 'let' bindings. 
Remember, <- is (for now) for performing I/O actions and binding their results to names. 
map toUpper firstName, however, isn't an I/O action
It's a pure expression in Haskell
So use <- when you want to bind results of I/O actions to names, and use let bindings
to bind pure expressions to names. 

Now we're going to make a program that continuously reads a line, and prints out the same
line with the words reversed. We will stop when we input a blank line: 

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reversewords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

TIP: to run a program you can either compile and run:
    ghc --make helloworld
    ./helloworld
or you can use the runhaskell command:
    runhaskell helloworld.hs 
and your program will be executed on the fly

First, let's look at the reverseWords function. 
It's just a normal function that takes a string like "hey there man"
It calls 'words' with it to produce a list of words like ["hey","there","man"]
Then we map reverse on the list, getting ["yeh","ereht","nam"]
and then we put that back into one string by using unwords, and the final result is
"yeh ereht nam". 
See how we used function composition here
Without it, we'd have had to do something like
    reverseWords st = unwords(map reverse (words st))

What about main?
First, we get a line from the terminal by performing a getLine call that binds to line. 
Now we have a conditional expression
Remember that in Haskell, every if must have a corresponding else because every expression
has to have some sort of value. 
We make the if so that when a condition is true (in this case, the line entered is blank),
we perform one I/O action. When the condition is false, the I/O action under 'else' is performed. 
That's why in an I/O block, 'if's have to have a form of 
    if condition then I/O action else I/O action

First, let's take a look at what happens under the else clause. 
Because we have to have exactly one I/O action after the else, we use a do block
to glue together two I/O actions into one. 
You could also write that part out as

else (do
    putStrLn $ reverseWords line
    main)

this makes it more explicit that the do block can be viewed as one I/O action, but it's uglier. 
Inside the do block, we call reverseWords on the line that we got from getLine, and then
print that out to the terminal. 
After that, we just perform main. 
It's called recursively and that's okay, because main is itself an I/O action. 
So in a sense, we just go back to the start of the program. 

Now, what happens when the 'null line' condition is True?
In that case, the I/O action after 'then' is performed. 
It says 'return ()'
The return in Haskell is nothing like the return in most other languages!
In imperative languages, return usually ends the execution of a method or subroutine, and
makes it report some sort of value to whoever called it. 
In Haskell (in I/O actions specifically), it makes an I/O action out of a pure value. 
If you think about the box analogy from before, it takes a value and wrap it up in a box. 
The resulting I/O action doesn't actually do anything, it just has that value
encapsulated as its result. 
So in an I/O context, 'return "HAHA"' will have a type of IO String. 
What's the point in transforming a pure value into an I/O action that does nothing?
Well, we needed some I/O action to carry out in the case of an empty input line. 
That's why we just made a bogus I/O action that doesn't do anything by writing return ().

Using return doesn't cause the I/O do block to end in execution or anything like that. 
For instance, this program will quite happily carry out all the way to the last line:

main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH"
    return 4
    putStrLn line

All that these returns do is make I/O actions that don't really do anything except
have an encapsulated result, and then the result is thrown away because it isn't bound to a name. 
We can use return in combination with <- to bind stuff to names. 

main = do
    a <- return "hell"
    b <- return "yeah"
    putStrLn $ a ++ " " ++ b

So, return is kind of the opposite to <-
While return takes a value and wraps it up in a box,
<- takes a box (and performs it) and takes the value out of it, binding it to a name. 
But doing this is kind of redundant, especially since you can use let bindings in do blocks to bind to names:

main = do
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b

When dealing with I/O do blocks, we mostly use return either because we need to create an
I/O action that doesn't do anything,
or because we don't want the I/O action that's made up from a do block to have the
return value of it's last action, but we want it to have a different result value,
so we use return to make an I/O action that always has our desired result contained
and we put it at the end. 

A do block can also have just one I/O action. 
In that case, it's the same as just writing the I/O action. 
Some people would prefer writing 'then do return ()' in this case, because
then the 'else' also has a 'do'. 

Before we move on to files, let's take a look at some functions that are useful when
dealing with I/O. 

putStr is much like putStrLn, in that it takes a String as a parameter and returns
an I/O action that will print the string to the terminal, only putStr doesn't jump
into a new line after printing out the string like putStrLn does. 

main = do   putStr "Hey "
            putStr "I'm "
            putStr "Andy"

running this gives
"Hey I'm Andy"

It's type signature is putStr :: String -> IO (), so the result encapsulated
within the resulting I/O action is the unit. 
A dud value, so it doesn't make sense to bind it. 

putChar takes a character and returns an I/O action that will print it out to the terminal

main = do   putChar 't'
            putChar 'e'
            putChar 'h'

running this gives
teh

putStr is actually defined recursively with the help of putChar
The edge condition of putStr is the empty string, so if we're printing an empty string,
then just return an I/O action that does nothing by using return ()
If it's not empty, then print the first character of the string with putChar
and then print the rest of them using putStr. 

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs

See how we can use recursion in I/O, just like we can use it in pure code. 
Just like in pure code, we defined the edge case and then think what the result actually is. 
It's an action that first outputs the first character and then outputs the rest of the string. 

'print' takes a value of any type that's an instance of Show (meaning we know how to
represent it as a String), calls show with that value to stringify it, and then outputs
that string to the terminal. 
Basically, it's just putStrLn . show
It first runs show on a value, and then feeds that to putStrLn, which returns an I/O action
that will print out our value:

main = do   print True
            print 2
            print "haha"
            print 3.2
            print [3,4,3]

running this gives
True
2
"haha"
3.2
[3,4,3]

Earlier we said that I/O actions are performed only when they fall into main, or when
we try to evaluate them in the GHCI prompt. 
When we type out a value like 3 or [1,2,3], and press the return key,
GHCI actually uses print on that value to display it out on our terminal. 

When we want to print out strings, we usually use putStrLn because we don't want the ""
but for printing out values of other types to the terminal, print is used the most. 

getChar is an I/O action that reads a character from the input. 
Thus, its type signature is 
    getChar :: IO Char
because the result contained within the I/O action is a Char. 
Note that due to buffering, the reading of the characters won't actually happen
until the user mashes the return key. 

main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

This program looks like it should read a character, and check if it's a space. 
If it is, halt execution, and if it isn't, print it to the terminal and repeat. 
It kind of does - only not in the way you might expect. 

running it gives:
(input) hello sir
(output) hello

'hello sir' is the input. We input 'hello sir' and press return. 
Due to buffering, the execution of the program will begin only after we've hit return,
and not after every inputted character. 
But once we press the return key, it acts on what we've been putting in so far. 

The 'when' function is found in Control.Monad 
(to get access to it, do import Control.Monad)
It's interesting because in a do block it looks like a control flow statement,
but it's actually a normal function. 
It takes a boolean value and an I/O action
If that boolean value is true, it returns the same I/O action we supplied to it
If it's false, it returns the 'return ()' action, so an I/O action that does nothing. 
Here's how we could re-write the previous code where we demonstrated getChar using 'when':

import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

So as you can see, it's useful for encapsulating the 
    if something then do some I/O action else return ()
pattern

'sequence' takes a list of I/O actions, and returns an I/O action that will perform
those actions one after another. 
The result contained in that I/O action will be a list of the results of all the
I/O actions that were performed. 
It's type signature is
    sequence :: [IO a] -> IO [a]

Doing this:

main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

Is exactly the same as doing this:

main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

So sequence [getLine, getLine, getLine] makes an I/O action that will perform
getLine three times. 
If we bind that action to a name, the result is a list of all of the results, so in our case,
a list of three things that the user entered at the prompt. 

A common pattern with sequence is when we map functions like print or putStrLn over lists. 
Doing map print [1,2,3,4] won't create an I/O action. 
It will create a list of I/O actions, because that's like writing
    [print 1, print 2, print 3, print 4]
If we want to transform that list of I/O actions into an I/O actions, we have to sequence it

ghci> sequence (map print [1,2,3,4,5])
returns
1
2
3
4
5
[(),(),(),(),()]

What's with the [(),(),(),(),()] at the end?
Well, when we evaluate an I/O action in GHCI, it's performed and then it's result is printed out,
unless that result is (), in which case it's not printed out. 
That's why evaluating putStrLn "hehe" in GHCI just prints out hehe, because the
contained result in putStrLn "hehe" is ()
But when we do getLine in GHCI, the result of that I/O action is printed out, because
getLine has a type of IO String. 

Because mapping a function that returns an I/O action over a list and then sequencing it is so common,
the utility functions mapM and mapM_ were introduced. 
mapM takes a function and a list, maps the function over the list and then sequences it. 
mapM_ does the same, only it throws away the result later. 
We usually use mapM_ when we don't care what result our sequenced I/O actions have. 

ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]
1
2
3

'forever' takes an I/O action and returns an I/O action that just repeats the I/O action
it was given forever. 
It's located in Control.Monad
This little program will indefinetly as the use for some input, and spit it back to them, capslocked:

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

'forM' (located in Control.Monad) is like mapM, only that it has its parameters switched around. 
The first parameter is the list, and the second one is the function to map over that list,
which is then sequenced. 
Why is that useful? 
Well, with some creative use of lambdas and do notation, we can do stuff like this:

import Control.Monad 

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1,2,3 and 4 are: "
    mapM putStrLn colors

The (\a -> do ...)
is a function that takes a number, and returns an I/O action
We have to surround it with parentheses, otherwise the lambda thinks the last two
I/O actions belong to it. 
Notice that we do 'return color' inside the do block
We do that so that the I/O action which the do block defines has the result of our color contained within it
We actually didn't have to do that, because getLine already has that contained within it:
    doing color <- getLine and then return color is just unpacking the result from
    getLine and then repackaging it again, so it's the same as just doing getLine. 
The forM (called with its two parameters) produces an I/O action, whose result we bind
to colors. colors is just a normal list that holds strings.
At the end, we print out all of those colors by doing mapM putStrLn colors

You can think of forM as meaning: make an I/O action for every element in this list 
What each I/O action will do can depend on the element that was used to make the action
Finally, perform those actions and bind their results to something
We don't have to bind it, we can also just throw it away. 

$ runhaskell form_test.hs
Which color do you associate with the number 1?
white
Which color do you associate with the number 2?
blue
Which color do you associate with the number 3?
red
Which color do you associate with the number 4?
orange
The colors that you associate with 1, 2, 3 and 4 are:
white
blue
red
orange

We could have done the same thing without forM, only with forM it's more readable. 
Normally, we write forM when we want to map and sequence some actions that we define 
there on the sport using do notation
In the same vein, we could have replaced the last line with forM colors putStrLn

### FILES AND STREAMS ###

getChar is an I/O action that reads a single character from the terminal
getLine is an I/O action that reads a line from the terminal
These two are pretty straightforward, and most programming languages have some functions
or statements that are parallel to them. 
But now, let's meet getContents

getContents is an I/O action that reads everything from the standard input until it
encounters an end-of-file character.
Its type is
    getContents :: IO String
What's cool about getContents is that it does lazy I/O. 
When we do foo <- getContents, it doesn't read all of the input at once, store it in memory
and then bind it to foo. 
No, it's lazy!
It says: 'i will read the input from the terminal later as we go along, when you need it'

getContents is really useful when we're piping the output from one program into the input
of our program. 

Let's have a look at how we can do this piping to begin with:

Say we have a text file haiku.txt with a haiku in it
And we have the program we wrote earlier to introduce 'forever', which
prompts the user for a line of input, prints it out in capitals, then repeats

Then we can do:

$ ghc --make capslocker 
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...
$ cat haiku.txt
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
capslocker <stdin>: hGetLine: end of file

What we're doing with the use of 'forever' is taking the input and transforming it into some output
We can use getContents to make our program even shorter and better:

import Data.Char 

main = do
    contents <- getContents
    putStrLn (map toUpper contents)

We run the getContents I/O action and name the string it produces 'contents'. 
Then, we map toUpper over that string and print it to the terminal. 
Keep in mind that because Strings are basically lists, which are lazy, and 
getContents is lazy, it won't try to read the whole content at once and store it into 
memory before printing out the capslocked version. 
Rather, it will print out the capslocked version as it reads it, because it will
only read a line from the input when it really needs to. 

$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS

That works.
What about running it by itself and typing in the lines manually?
That works too. we exit out of it by pressing CTRL-D

As you can see, it prints out our capslocked input back to us line by line.
When the result of getContents is bound to contents, it's not represented in memory as a real
string, but more like a promise that it will produce the string eventually. 
When we map toUpper over contents, that's also a promise to map that function over the eventual contents. 
And finally, when putStr happens, it says to the previous promise - "hey, i need a capslocked line"
It doesn't have any lines yet, so it says to contents - "i need a line from the terminal"
That's when getContents actually reads from the terminal and gives a line to the code that
asked it to produce something tangible. 
That code then maps toUpper over the line and gives it to putStr, which prints it
And then putStr says - "i need the next line"
This repeats until there's no more input, which is signified by an end-of-file character

Let's write a program that takes some input and prints out only those lines with <10 characters

main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result

We've made the I/O part of the program as short as possible. 
Because our program is supposed to take some input and print out some output based on the input,
we can implement that by reading the input contents, running a function on them, and then printing
out what the function gave back. 

The shortLinesOnly function works like this:
it takes a String, like "short\nlooooooooooooong\nshort"
That string has three lines, two of them are short and the middle one is long
It runs the lines function on that string, which converts it to
    ["short","loooooooooooong","short"]
which we then bind to the name allLines
That list of strings is then filtered, so that only those lines that are shorter
than 10 characters remain in the list, producing ["short", "short"]
Finally, unlines joins that list into a single newline delimited string, giving
    "short\nshort"

We can even pipe the contents of a text file into this code

This pattern of getting some string from the input, transforming it with a function,
and then outputting that, is so common that there exists a function which makes that even easier,
called 'interact'. 
interact takes a function of type String -> String as a parameter, and returns an I/O action
that will take some input, run that function on it, and then print out the function's result
Let's modify our program to use that

main = interact shortLinesOnly

(shortLinesOnly remains unchanged)

Just to show that this could be achieved in much less code,
even though it will be less readable,
and to demonstrate our function composition skill, we can rework it even further:

main = interact $ unlines . filter ((<10) . length) . lines

interact can be used to make programs that are piped some contents into them, and then
dump some result out, OR it can be used to make programs that appear to take a line of
input from the user, give back some result based on that line, and then take another line
There isn't actually a real distinction between the two, it just depends on how the user is
supposed to use them. 

Let's make a program that continually reads a line and then tells us if the line is 
a palindrome or not. 
We could just use getLine to read a line, tell the user if it's a palindrome, then
run main all over again. 
But it's simpler if we use interact instead. 
When using interact, think about what you need to do to transform some input into
the desired output. 
In our case, we have to replace each line of the input with either
"palindrome" or "not palindrome"
So we have to write a function that transforms something like
    "elephant\nABCBA\nsomething"
into
    "not a palindrome\npalindrome\nnot a palindrome"

respondPalindromes contents = unlines (map \xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
    where isPalindrome xs = xs == reverse xs

Let's write that in point-free:

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where isPalindrome xs = xs == reverse xs

Pretty straightforward
First, it takes something like
    "elephant\nABCBA\nsomething"
into
    ["elephant", "ABCBA", "something"]
Then it maps the lambda function over it, giving
    ["not a palindrome", "palindrome", "not a palindrome"]
and then unlines joins that list into a single, newline delimited string

Now we can do

main = interact respondPalindromes

$ runhaskell palindromes.hs
hehe
not a palindrome
ABCBA
palindrome
cookie
not a palindrome

Even though we made a program that transforms one big string of input into another, it
acts like we made a program that does it line by line. 
That's because Haskell is lazy, and wants to print the first line of the result string,
but it can't because it doesn't have the first line of the input yet. 
So as soon as we give it the first line of the input, it prints the first line of the output. 
We get out of the program with CTRL-D

We can also use this program by piping a file into it. 

So far, we've worked with I/O by printing stuff out to the terminal and reading from it. 
What about reading and writing files? 
Well, in a way, we've already been doing that. 
One way to think about reading from the terminal is to imagine that it's like reading
from a somewhat special file. 
The same goes for writing to the terminal, it's kind of like writing to a file.
We can call these two files stdout and stdin, meaning standard output and standard input
Keeping that in mind, we'll see that writing to and reading from files is very much
like writing to the standard output and reading from the standard input. 

We'll start off with a really simple program that opens a file called girlfriend.txt, 
which contains a verse from the best song written by humankind, and prints it to the terminal

Here's girlfriend.txt:

Hey! Hey! You! You! 
I don't like your girlfriend! 
No way! No way! 
I think you need a new one!

And here's our program:

import System.IO 

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

Running it, we get the expected result:

$ runhaskell girlfriend.hs
Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!

Let's go over this line by line:
Our program is several I/O actions glued together with a do block
In the first line of the do block, we notice a new function called openFile
This is it's type signature:
    openFile :: FilePath -> IOMode -> IO Handle
If you read that out loud, it states:
openFile takes a file path and an IOMode, and returns an I/O action that will 
open a file and have the file's associated handle encapsulated as its result. 

FilePath is just a type synonym for String, simply defined as:

type FilePath = String

IOMode is a type that's defined like this:

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

Just like our type that represented the seven possible days of the week, this type is
an enumeration that represents what we want to do with our opened file. 
Just note that this type is IOMode and not IO Mode. 
IO Mode would be the type of an I/O action that has a value of some type Mode as its result
IOMode is just a simple enumeration. 

Finally, it returns an I/O action that will open the specified file in the specified mode. 
If we bind that action to something, we get a Handle. 
A value of type Handle represents where our file is. 
We'll use that handle so we know which file to read from. 
It would be stupid to read a file but not bind that read to a handle, because we wouldn't
be able to do anything with the file
So in this case, we bound the handle to 'handle'. 

In the next line, we see a function called hGetContents. 
It takes a Handle, so it knows which file to get the contents from,
and returns an IO String - an I/O action that holds as its result the contents of the file. 
This function is pretty much like getContents. 
The only difference is that getContents will automatically read from stdin (the terminal)
whereas hGetContents takes a file handle which tells it which file to read from. 
In all other respects, they work the same. 
And just like getContents, hGetContents won't attempt to read the file all at once and
store it in memory, but it will read it as needed. 
That's really cool, because we can treat contents as the whole contents of the file, but
it's not really loaded in memory. 
So if this were a really huge file, doing hGetContents wouldn't choke up our memory, it
would read only what it needed to from the file, when it needed to.

Note the difference between the handle used to identify a file, and the contents of the file,
bound in our program to 'handle' and 'contents'. 
The handle is just something we use to know what our file is. 
If you imagine the whole file system to be a really big book and each file is a chapter
in the book, the handle is a bookmark that shows you where you're currently
reading or writing a chapter, whereas 'contents' is the actual chapter. 

With putStr contents we just print the contents out to the stdout,
and then we do hClose, which takes a handle and returns an I/O action that closes the file. 
You have to close the file yourself after opening it with openFile!

Another way of doing what we just did is to use the withFile function
which has a type signature of
    withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
It takes a path to a file, an IOMode,
and a function that takes a handle and returns some I/O action. 
What it returns is an I/O action that will open that file,
do something we want with the file, and then close it. 
The result encapsulated in the final I/O action that's returned is the same as the result 
of the I/O action that the function we give it returns. 

This might sound complicated but it's really simple, especially with lambdas
Here's our previous example rewritten to use withFile:

import System.IO 

main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

As you can see, it's very similar to the previous piece of code. 
(\handle -> ...) is the function that takes a handle and returns an I/O action,
and it's usually done like this with a lambda. 
The reason it has to take a function that returns an I/O action, instead of just taking
an action to do and then close the file, it because the I/O action that we'd pass to it
wouldn't know which file to operate on. 
This way, withFile opens the file and then passes the handle to the function we gave it. 
It gets an I/O action back from that function, and then makes an I/O action that's
just like it, only it closes the file afterwards. 

Here's how we can make our own withFile function:

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

We know that the result will be an I/O action, so we can just start off with a do
First, we open the file and get a handle from it
Then, we apply handle to our function to get back the I/O action that does all the work
We bind that action to result, close the handle, and then return result
By returning the result encapsulated in the I/O action we got from f, we make it so
that our I/O action encapsulates the same result as the one we got from f handle. 
So if f handle returns an action that will read a number of lines from stdin and write
them to a file, and has as its result encapsulated the number of lines that it read, 
if we used with with withFile', the resulting I/O action would also have 
the number of lines read as its result. 

Just like how we have hGetContents that works like getContents but for a specific file, 
there's also hGetLine, hPutStr, hPutStrLn, hGetChar, etc. 
They work just like their counterparts without the h,
only they take a handle as a parameter and operate on that specific file instead of stdin/stdout

Loading files and then treating their contents as strings is so common
that we have these three nice functions to make our work even easier:

readFile has a type signature of
    readFile :: FilePath -> IO String
Remember, FilePath is just a fancy name for String. 
readFile takes a path to a file, and returns an I/O action that will read that file
(lazily of course) and binds its contents to something as a string. 
It's usually more handy than doing openFile and binding it to a handle and then doing hGetContents
Here's how we could have written our previous example with readFile:

import System.IO 

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents

Because we don't get a handle with which to identify our file, we can't close it manually,
so Haskell does that for us when we use readFile. 

writeFile has a type of
    writeFile :: FilePath -> String -> IO ()
It takes a path to a file, and a string to write to the file,
and returns an I/O action that will do the writing. 
If such a file already exists, it will be overwritten. 

Here's how to turn girlfriend.txt into a capslocked version and write it to girlfriendcaps.txt:

import System.IO
import Data.Char 

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)

appendFile has a type signature that's just like writeFile,
only appendFile doesn't overwrite an existing file but appends stuff to it instead. 

Let's say we have a file todo.txt that has one task per line that we need to do. 
Now, let's make a program that takes a line from the stdin and adds that to our to-do list

import System.IO 

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

We needed to add the \n to the end of each line because getLine doesn't give us one

One more thing.
We talked about how doing 'contents <- hGetContents handle' doesn't cause the whole
file to be read at once and stored in memory. 
It's I/O lazy, so doing this:

main = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

is actually like connecting a pipe from the file to the output. 
Just like you can think of lists as streams, you can also think of files as streams. 
This will read one line at a time and print it out to the terminal as it goes along. 

You may be asking - how wide is this pipe? How often will the disk be accessed?
For text files, the default buffering is usually line buffering. 
That means that the smallest part of the file to be read at once is one line. 
That's why in this case it actually reads a line, prints it, reads the next , ...
For binary files, the default buffering is usually block buffering. 
That means it will read the file chunk by chunk. 
The size of the chunks are determined by your OS

You can control exactly how buffering is done by using the hSetBuffering function. 
It takes a handle and a BufferMode, and returns an I/O action that sets the buffering. 
BufferMode is a simple enumeration data type and the possible values it can hold are:
    NoBuffering, LineBuffering, or BlockBuffering (Maybe Int)
The Maybe Int is for how big the chunk should be, in bytes. 
If it's Nothing, then the OS determines the chunk size. 
NoBuffering means that it will be read one character at a time.
NoBuffering usually sucks as a buffering mode because it has to access the disk so often. 

Here's our previous piece of code, only it doesn't read the file line by line but 
instead in chunks of 2048 bytes:

main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)

Reading files in bigger chunks can help if we want to minimise disk accesses or when
our file is actually a slow network resource. 

We can also use hFlush, which is a function that takes a handle and returns an I/O action
that will flush the buffer of the file associated with the handle. 
When we're doing line buffering, the buffer is flushed after every line. 
When we're doing block buffering, it's after we've read a chunk. 
It's also flushed after closing a handle. 
That means when we've reached a newline character, the reading (or writing) mechanism
reports all the data so far.
After flushing, the data is available to other programs that are running at the same time. 

Think of reading a block-buffered file like this:
Your toilet bowl is set to flush itself after it has one gallon of water inside it
So you start pouring in water, and once the gallon mark is reached, that water is
automatically flushed and the data in the water that you've poured in so far is read. 
But you can flush the toilet manually too by pressing the button on the toilet. 
This makes the toilet flush, and all the water (data) inside the toilet is read. 

We already made a program to add a new item into our todo list in todolist.txt 
Now let's make a program to remove an item. 

import System.IO
import System.Directory 
import Data.List 

main = do
    handle <- openFile "todo.txt" ReadMode 
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your to-do items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"

First, we open todo.txt in read mode, and bind it's handle to 'handle'

Next, we use a function we haven't seen before from System.IO - openTempFile
Its name is pretty self explanatory
It takes a path to a temporary directory and a template name for a file, and opens a temporary file
We used "." for the temporary directory because this is the current working directory
on pretty much any OS. 
We used "temp" as the template name for the temporary file, which means that the temporary
file will be named temp plus some random characters. 
It returns an I/O action that makes the temporary file, and the result in that I/O
action is a pair of values: the name of the temporary file, and a handle. 
We could just open a normal file called todo2.txt or something, but it's better practice
to use openTempFile so you know you're probably not overwriting anything. 

The reason we didn't use getCurrentDirectory to get the current directory and then pass that
to openTempFile is that we can use "." to refer to the current working directory
on unix-like systems and Windows. 

Next up, we bind the contents of todo.txt to 'contents'. 
Then, split that string into a list of strings, each one line (this is todoTasks)
So todoTasks is now something like
    ["Iron the dishes", "Dust the dog", "Take the salad out of the oven"]
We zip [0..] and a function that takes a number, like 3, and a string, like "hey" and returns
"3 - hey"
so numberedTasks is like
    ["0 - Iron the dishes", "1 - Dust the dog", ...]
We join that list of strings into a single newline delimited string with unlines
and then print that string out to the terminal. 
Instead of doing that, we could have done mapM putStrLn numberedTasks

We ask the user which one they want to delete, and wait for them to enter a number
Let's say they want to delete number 1, which is "Dust the dog", so they punch in 1
numberString is now "1", but because we want a number not a string, we run
read on that to get 1 and bind that to a number. 

Remember the delete and !! functions from Data.List
!! returns an element from a list with some index
delete deletes the first occurence of an element in a list and returns a
new list without that occurrence
(todoTasks !! number) when number is 1 returns "Dust the dog"
We bind todoTasks without the first occurence of "Dust the dog" to newTodoItems
and then join that into a single string with unlines
before writing it to the temporary file that we opened. 
The old file is now unchanged and the temporary file contains all the lines from the old one,
minus the one we deleted. 

After that we close both files
and remove the original one with removeFile, which, as you can see,
takes a path to a file and deletes it. 
After deleting the old todo.txt, we use renameFile to rename the temporary file to todo.txt 
Be careful, removeFile and renameFile (which are both in System.Directory)
take file paths as their parameters, not handles. 

Note unlike in something like bash the temporary file is not deleted once we're done!

And that's that!
We could have done this in fewer lines, but we were careful not to overwrite any
existing files and asked the OS to tell us where we can put our temporary file. 

$ runhaskell deletetodo.hs
These are your TO-DO items:
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
Which one do you want to delete?
1

$ cat todo.txt
Iron the dishes
Take salad out of the oven

$ runhaskell deletetodo.hs
These are your TO-DO items:
0 - Iron the dishes
1 - Take salad out of the oven
Which one do you want to delete?
0

$ cat todo.txt
Take salad out of the oven

### COMMAND LINE ARGUMENTS ###

Dealing with command line arguments is a necessity if you want to make a script or application
that runs on a terminal

Luckily, Haskell's standard library has a nice way of getting the command line
arguments of a program. 

In the previous section, we made one program to add an item to the todo list and one
program to remove an item. 
There are two problems with this approach:

The first is that we hardcoded the name of our todo file in the code
We just decided that the file is named todo.txt and that the user will never have
multiple todo lists. 

One way to solve this is to always ask the user which file they want as their todo list. 
We used that approach when we wanted to know which item the user wants to delete. 
That works, but it's not so good because it requires the user to run the program, 
wait for the program to ask something, and then tell that to the program. 
That's called an interactive program. 
The difficult bit with interactive command line programs is - what if we want
to automate the execution of that program, like in a bash / batch script?
It's harder to make a batch script that interacts with a program than it is a batch script
that just calls one program or several of them.

That's why its sometimes better to have the user tell the program what they want WHEN
they run the program, instead of having the program ask them. 
What better way to do this than command line arguments?

The System.Environment module has two cool I/O actions. 
One is getArgs, which has a type of
    getArgs :: IO [String]
and is an I/O action that will get the arguments that the program was run with,
and have as its contained result a list with the arguments. 
getProgName has a type of
    getProgName :: IO String
and is an I/O action that contains the program name. 

Here's a small program that demonstrates how these two work:

import System.Environment
import Data.List 

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName

We bind getArgs and getProgName to args and progName.

$ ./arg-test first second w00t "multi word arg"
The arguments are:
first
second
w00t
multi word arg
The program name is:
arg-test

Nice. 
Using this we can create some cool command line applications. 
In fact, let's go ahead and make one. 
Let's join our functionality for dealing with todo lists into one program
Let's also make it operate on different files, not just todo.txt 

We'll simply call it todo and it'll be able to do three things:
-View tasks
-Add tasks
-Delete tasks

We won't concern ourselves with possible bad input right now.

Our program will be made such that if we want to add the task
    "Find the magic sword of power"
in the file todo.txt, we have to punch in
    todo add todo.txt "Find the magic sword of power"
To view the tasks, we'll do
    todo view todo.txt
and to remove the task with index 2, we'll do
    todo remove todo.txt 2

We'll start by making a dispatch association list. 

-}