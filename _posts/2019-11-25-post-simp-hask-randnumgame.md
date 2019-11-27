---
toc: true
toc_label: "Getting Started"
classes: wide
title: "Simple Haskell: Random Number Game"
excerpt_separator: "<!--more-->"
categories:
  - Example Problems
tags:
  - Haskell
  - Programming
  - Learning
  - Blog
  - Functional Programming
---

This is the first problem in a series of simple problems, written in Haskell. I regularly use rosettacode.com to find out different ways haskell solves problems, but it lacks thorough explanations that are useful to get a thorough understanding why specific functions and patterns are used in a Haskell solution. This post will focus on the following problem.
Given a random number, allow a user to guess the random number within five chances to guess. 

<!--more-->

This is a good problem to bring up because its going to deal with a lot of IO. Dealing with IO, functionally and safely can be cumbersome, but necessarily so. Strict language design against one of the most common failure points in software is a safeguard for proper code design, which we will explore below.

So, given a random number, let's give a user five chances to guess what the number is. Immediately, we should try to model the state we would like to track in our program. This state includes the decision of the user, as follows:

{% highlight haskell %}
data PlayerChoice = Correct | Incorrect
                  deriving (Eq, Show)
{% endhighlight %}

This is an Algebraic Data Type (ADT). This is similar to how a boolean is defined as `True` or `False`, but our ADT more closely resembles our problem logic we care about.
We derive Eq and Show because we would like to state that `Correct` == `Correct`, and be able to print out `Correct` and `Incorrect` to the command line.
In addition to this state, there are a few other variables like the current player's number choice, and which iteration of player guesses we are on. Game state is modeled as follows:

{% highlight haskell %}
data GameState = GameState { iteration :: Int, choiceState :: PlayerChoice, randNum :: Int}
               deriving Show
{% endhighlight %}

The above code snippet is haskell record syntax, and it's about the closest you will ever get to a class in typical object-oriented programming languages. We also derive `Show` here for debugging purposes.

One note to make before proceeding is that haskell doesn't have while loops. It doesn't have for loops either.
Just so we can get our hands wet, here is a haskell solution of the problem above without any looping at all, just recursion.
When designing functional programs, its a good idea to isolate all your pure functions as much as *functionally* possible. IO is not pure because something outside of our program can affect your programs execution. Having a function that return IO does not make a function pure, but it makes it obviously impure -- this oviousness is helpful for individuals trying to use your Haskell libraries. 

## Solution by Recursion

First we need to define a function that takes in a random number, and returns whether or not the user made a correct choice or not.
This means our function will take a random number `Int` and convert it to a `PlayerChoice`, but because we must interact with the player within IO, then our function play will convert `Int` to `IO Player Choice`, or as follows:

{% highlight haskell %}
play :: Int -> IO PlayerChoice
play rand = do guess <- fmap read getLine
               return $ if guess == rand then Correct else Incorrect
{% endhighlight %}

Inside the function above we extract what the user types into the command prompt from `getLine`, then because is a monad, we use `fmap` to apply `read` to convert the `String` input to an `Int` and extract it with the `do` syntactic sugar notation. Instead of mutating state, we copy the original state `gs`, and modify variables from that state.

{% highlight haskell %}
runGame :: GameState -> IO GameState
runGame gs@GameState {iteration = i, choiceState = attempt, randNum = rand } = 
            let nextIter = i + 1 
            in  if i >= 5 || attempt == Correct
                then return gs { iteration = nextIter, choiceState = attempt }
                else do nextPlay <- play rand
                        runGame gs { iteration = nextIter, choiceState = nextPlay }
{% endhighlight %}

We are missing one final function to run our random number guesser game, and that is a main function to generate the random number and use the state function we created above. Here we jump inside IO and assign a random integer to `rand`, then we initialize our state, and recursively ask the player for a random number until they have either guessed correctly, or exhasted their tries which is the `case` pattern matching at the end of the function.


{% highlight haskell %}
main :: IO ()
main = do ( rand :: Int ) <- randomRIO (0,10)
          result <- runGame GameState {iteration = 0, choiceState = Incorrect, randNum = rand}
          case choiceState result of 
            Correct   -> putStrLn $ "You correctly guessed the number: " ++ show rand
            Incorrect -> putStrLn $ "None of the guesses were correct, the actual number was: " ++ show rand
{% endhighlight %}                   


## What is the opposite of fold?
Looking at this code you might think that this is a lot of code for some pretty simple functionality. You would be right, there is probably a simpler way.
Thinking a bit about the problem, all the recursive function is doing is generating a bunch (read: List) of possible values, and then taking the final value in that list. This sounds just the opposite of a `foldr` which takes a list and smashes the list down into a single value. In fact, Haskell has just this function, and it is aptly named `unfoldr`. 
`unfoldr` takes a seed and generates a list from that seed based on a function that returns a monadic type so that unfoldr knows when to stop. Because we are interacting with IO, we will use `unfoldrM` from the `monad-loops` package.

Here is an implementation of the main function using `unfoldrM`.  `runGame` is modified to accept the type argument of `unfoldrM` which is `unfoldrM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]`:

{% highlight haskell %}
runGame :: GameState -> IO (Maybe (PlayerChoice, GameState))
runGame gs@GameState { iteration = i, choiceState = attempt, randNum = rand } = 
    if i >= 5 || attempt == Correct
    then return Nothing
    else do nextPlay <- play rand
            let nextState = gs { iteration = i + 1, choiceState = nextPlay }
            return $ Just (nextPlay, nextState)

main :: IO ()
main = do ( rand :: Int ) <- randomRIO (0,10)
          attempts <- unfoldrM runGame GameState { iteration = 0, choiceState = Incorrect, randNum = rand }
          case last attempts of 
            Correct   -> putStrLn $ "You correctly guessed the number: " ++ show rand
            _         -> putStrLn $ "None of the guesses were correct, the actual number was: " ++ show rand

{% endhighlight %}   


To recap what is happening above in our new `runGame` function, we are threading nextState through our list, and checking nextPlay each time we unfold a new value into the list.
The value of `attempts` would look something like `[Incorrect, Incorrect, Incorrect, Correct]` if our player succesfully guessed on the fourth try.
With the layout of our new `runGame` function, it became less boiler-platey because we dont have to copy and pass around `gs` as much.

This method is a bit nicer, but now we need to keep track of all the intermediary results. If by some cruel chance some ML model (read: intern) was delegated to guess a number between one and one trillion, for the sake of performance, discarding prior program states will help the cause.

## I Kind of Lied
So I lied when I said Haskell did not have any for loops, sort of. Conveniently, it's possible to replicate while and for loop functionality with `iterate`. Again, because we are interacting with IO, we will use `iterateUntilM` from `monad-loops`.

{% highlight haskell %}
stateCheck :: GameState -> Bool
stateCheck GameState { iteration = i, choiceState = attempt } = 
    i >= 5 || attempt == Correct

runGame :: GameState -> IO GameState
runGame gs@GameState { iteration = i, randNum = rand } = 
    do nextPlay <- play rand
       return gs { iteration = i + 1, choiceState = nextPlay }

main :: IO ()
main = do ( rand :: Int ) <- randomRIO (0,10)
          let initialState = GameState { iteration = 0, choiceState = Incorrect, randNum = rand }
          attempt <- iterateUntilM stateCheck runGame3 initialState
          case choiceState attempt of 
            Correct   -> putStrLn $ "You correctly guessed the number: " ++ show rand
            _         -> putStrLn $ "None of the guesses were correct, the actual number was: " ++ show rand
{% endhighlight %}  


Interestingly enough, this version is the same length as our unfolrM version, but it has more well-defined code boundaries.
Instead of improving the programmers coding experience, let's switch focus to the poor intern that was delegated to the task of guessing random numbers.


## Modeling More than Two States
Instead of modeling our PlayerChoice with `Correct` or `Incorrect`, using the hot-cold scale will help our user guess the random number. The new `PlayerChoice` model will be:

{% highlight haskell %}
data PlayerChoice = Correct | Hotter | Colder | Unknown
                  deriving (Eq, Show)
{% endhighlight %}  

In order to determine what is `Hotter` and what is `Colder`, we need to keep track of two states, a prior and a current state, along with our random number.

Now that the actual integer choice of the user needs to be tracked across different state, `choice` is added to `GameState` as `Maybe Int` as it will not exist at the beginning of program execution. The functions `stateCheck` and `main` are not affected. If we had not modeled `PlayerChoice` as our own ADT and instead used Boolean, refactoring runGame could have lead to some inconspicuous bugs. 
This is our final program:

{% highlight haskell %}

data PlayerChoice = Correct | Hotter | Colder | Unknown
                   deriving (Eq, Show)

data GameState = GameState { iteration :: Int
                           , choiceState :: PlayerChoice
                           , randNum :: Int
                           , choice :: Maybe Int }
               deriving Show


stateCheck :: GameState -> Bool
stateCheck GameState { iteration = i, choiceState = attempt } = 
    i >= 5 || attempt == Correct

runGame :: GameState -> IO GameState
runGame gs@GameState { iteration = i, randNum = rand, choice = ch } = 
    do ( nextGuess :: Int ) <- fmap read getLine
       let hint = case ch of 
             Nothing    -> Unknown -- for when user choice is not set on initialization
             Just guess -> if nextGuess == rand then Correct
                           else case compare (abs $ nextGuess - rand) (abs $ guess - rand) of
                                 LT -> Hotter
                                 GT -> Colder
                                 EQ -> Unknown
       putStrLn $ "Your choice is now " ++ show hint        
       return gs { iteration = i + 1, choiceState = hint, choice = Just nextGuess }

main :: IO ()
main = do ( rand :: Int ) <- randomRIO (0,10)
          let initialState = GameState { iteration = 0, choiceState = Unknown, randNum = rand, choice = Nothing }
          attempt <- iterateUntilM stateCheck runGame initialState
          case choiceState attempt of 
             Correct -> putStrLn $ "You correctly guessed the number: " ++ show rand
             _       -> putStrLn $ "None of the guesses were correct, the actual number was: " ++ show rand
{% endhighlight %} 





The game now plays like this:
{% highlight bash %}
$ main
1
Your choice is now Unknown
2
Your choice is now Hotter
5
Your choice is now Colder
3
Your choice is now Correct
You correctly guessed the number: 3
{% endhighlight %}  

This final example wraps up our simple random number game. Its possible to roll in custom type classes for Ordering on `PlayerChoice`, and even use the offical `State` monad, but this implementation is simple with only one import and no GHC extensions. LambdaCase and Lenses are possible other areas of improvement.
