---
toc: true
toc_label: "Getting Started"
classes: wide
title: "Simple Haskell: Parsing Logs (Part I)"
excerpt_separator: "<!--more-->"
categories:
  - Example Problems
tags:
  - Haskell
  - Programming
  - Learning
  - Blog
  - Functional Programming
  - Error Handling
---





Sane, efficient, and legible error handling can rank pretty high on the difficulty charts when writing a new application. Getting an answer to "Should this function throw an error?" or "When should I abort program execution?" is not-so-straighforward, especially for those new to programming. This post explores some of the basics of functional error handling, and how this is different than the run-by-exception, or check-if-null methodology of other languages.
The goal of this post of `Simple Haskell` is to parse a bunch of log files from `/var/log/kern.log`, specifically for Ubuntu 19.10.


<!--more-->
All of the following code snippets are present in this github repo: [logan-c-brown/log-parse](https://github.com/logan-c-brown/log-parse) 

## Intro
Just to get the obvious out of the way, there are a many different methods to parse log files, and nearly every *nix machine has standard command line utilities that would make this exercise pretty straightforward. We'll ignore those for the sake of experimentation.
Lets take a look at how our data looks:

{% highlight awk %}
Dec  3 20:03:26 x1e kernel: [78491.915556] mce: CPU4: Package temperature/speed normal
Dec  3 20:03:26 x1e kernel: [78491.915572] mce: CPU10: Package temperature/speed normal
Dec  3 20:03:35 x1e kernel: [78501.059597] mce: CPU2: Core temperature above threshold, cpu clock throttled (total events = 21781)
Dec  3 20:03:35 x1e kernel: [78501.059598] mce: CPU8: Core temperature above threshold, cpu clock throttled (total events = 21781)
Dec  3 20:03:35 x1e kernel: [78501.094675] mce: CPU2: Core temperature/speed normal
Dec  3 20:03:35 x1e kernel: [78501.094675] mce: CPU8: Core temperature/speed normal
{% endhighlight %}


Because this problem involves a good bit of parsing, Haksell is a pretty good choice, especially for its powerful type system that enables easy composition of execution state. We'll start out this problem by using regular expressions, and file-handling similar to what you would find in Python, to using parser-combinators and streaming IO libraries for code clarity.

## Parsing and Type Refinement

When dealing with the outside world and input that your program does not generate, it's a good idea to follow the motto of being liberal with input, but returning very strict output (see: [Robustness Principle](https://en.wikipedia.org/wiki/Robustness_principle)). 

To do this in haskell, we will accept input of the most general type we can `IO String`, a type of text data that comes from outside our program, and incrementally refine our representation of what a `log` is. 
Here are three models that we will use to refine our types during log parsing.

1. We start out very general, a `KernelLog` where all the values are string. 

2. Then a `ParsedLog`, where some of the values are corrected.

3. Finally a `CpuLog` which only contains the information we want to return.

 Setting up your models like this is helpful because we will be interested in tracking what log lines are able to be parsed from a `KernelLog` to a `ParsedLog` but not to a `CpuLog`. Or we may be interested in a log line that can be parsed as a `KernelLog`, but not as a `ParsedLog`.

{% highlight haskell %}
data KernelLog = KernelLog { timestamp :: String
                           , machine   :: String
                           , uptime    :: String
                           , event     :: String
                           , message   :: String } deriving (Show)

data ParsedLog = ParsedLog { date         :: UTCTime
                           , machineName  :: String
                           , time         :: Double
                           , eventName    :: String
                           , messageReply :: String } deriving (Show)

data CpuLog = CpuLog Int String
            deriving (Show)
{% endhighlight %}

## Maybe
When parsing text, it's common to run into circumstances where the parser cannot parse input data. Here is an example of regular expression parsing in python

When using regular expressions in a language like python, you would typically need to check if a regular expression matched before processing the match. This would look something like this:

{% highlight python %}
import re
m = re.match("(cat)", "cat in the hat")
if m:
    actual_match = m.group(0)
    # process results on actual_match
else:
    # do something else
# possibly do something else with m outside the if statement
{% endhighlight %}


It's not obvious to the reader that `re.match` would throw an error, or would return a default value, or something else entirely, so it's up to the reader to scroll through documentation to find out what errors it raises. 
\\
On the contrary, what makes Haskell such a great tool at parsing is that you can depend on just the types of the functions alone during parsing to ensure that your parser has no undefined behavior. Haskell syntax and the compiler makes undefined behavior obvious ([partial functions](https://wiki.haskell.org/Partial_functions) being an example of typical undefined behavior in Haskell). Simple log parsing using regular expressions in Haskell follows below.


{% highlight haskell %}
kernelRegex :: Regex
kernelRegex = mkRegex "(^.+[0-9]{2}:[0-9]{2}:[0-9]{2}) (\\w+) kernel: \\[([0-9]+\\.[0-9]+)\\] ([^:]+): (.+)"


readLog :: String -> Maybe KernelLog
readLog line =
    case matchRegex kernelRegex line of
        Nothing -> Nothing
        Just [ts, mach, up, e, mess] -> Just KernelLog { timestamp = ts
                                                         , machine = mach
                                                         , uptime = up
                                                         , event = e
                                                         , message = mess }
{% endhighlight %}

Note that because the type of matchRegex is `matchRegex :: Regex -> String -> Maybe [String]`, then `matchRegex` can return either `Nothing` or `Just [String]`. We must do something about the case when there is no match or `Nothing`, or else our program can throw an exception. The regex above probably looks wild to those uninitiated, we will improve this in Part II to this post. We continue to refine our types during log parsing into a `ParsedLog` and finally a `CpuLog`.


{% highlight haskell %}
parseLogTime :: String -> Maybe UTCTime
parseLogTime timestamp = parseTimeM True defaultTimeLocale "%Y %b  %-d %T" timestamp :: Maybe UTCTime

transformLog :: KernelLog -> Maybe ParsedLog
transformLog KernelLog { timestamp,  machine, uptime, event, message} =
    do logDate <- parseLogTime $ "2019 " ++ timestamp
       time    <- readMaybe uptime :: Maybe Double
       return $ ParsedLog { date = logDate
                          , machineName = machine
                          , time = uptime
                          , eventName = event
                          , messageReply = message}

cpuRegex :: Regex
cpuRegex = mkRegex "^CPU([0-9]+): (.+)$"

getCpuLog :: ParsedLog -> Maybe CpuLog
getCpuLog log =
    case matchRegex cpuRegex $ messageReply log of
        Nothing -> Nothing
        Just [cpu, state] -> do cpuV <- readMaybe cpu :: Maybe Int
                                Just $ CpuLog cpuV state

{% endhighlight %}

Notice how we continue to use the `Maybe` data type throughout each function. Doing so allows us to easily compose together each function without worrying about errors inside each function. All the information about what the function can return or how it can affect program execution is already encoded in the return type. For example, `getCpuLog` can only, ever, return `Nothing` or `Just CpuLog` -- it cannot affect anything else in our program.


Next, we will grab each `CpuLog` and write it to their own file based on cpu-core number.  Haskell's `withFile` function will be used from the `System.IO` module. `withFile` manages file opening and closing for us just like the `with` statement manages context in Python.
Before continuing, the arguments of our function looks like:

{% highlight haskell %}
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
{% endhighlight %}

Our beloved friend `IO` is back! So in order to write a file we need 
- [a `FilePath` -- which is really just a type synonym for `String`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#t:FilePath)
- [an `IOMode` for how we are handling our file ](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html#t:IOMode)
- [and finally a function which takes a file `Handle` and does something to it](https://hoogle.haskell.org/?hoogle=Handle%20-%3E%20IO%20a).

Because what file we want to write to depends on each cpu log, a function to generate a `FilePath` for each cpu log is needed. Take note of the functions `</>` and `<.>` in the next snippet and notice how they syntactically describe and combine file paths. This is like a domain-specific language, and it's syntax is more refined than standard string concatenation. Specialized types and functions like this makes code *intent* more clear.

{% highlight haskell %}
cpuLogFilePath :: CpuLog -> FilePath -> FilePath
cpuLogFilePath (CpuLog cpu mess) dir =  dir </> show cpu  <.> ".log"
{% endhighlight %}

Now just the final parsing to a cpu log is left. `WriteMode` is the `IOMode` of our file handler, and a directory, `FilePath`, a directory, is taken as input, something that will be provided by the user of our application. [Infix notation](https://wiki.haskell.org/Infix_operator) is used on `hPrint` so that we dont explicitly have to define an anonymous function such as `(\inHandle -> hPrint inHandle log)`. The function returns `IO ()` because `cpuLogWrite` runs only side-effects or processing that Haskell cannot type check. 

{% highlight haskell %}
cpuLogWrite :: CpuLog -> FilePath -> IO ()
cpuLogWrite log dirr = withFile ( cpuLogFilePath log dirr) AppendMode
                             (`hPrint` log)
{% endhighlight %}
> *quaere*: Could you type check to ensure writing to a file will always result in the file being written?


## Either this or that
All that is left is to wrap up our functions together to be executed by the command line. This means we need to interact with user input, which can be just about anything a user can type. In order to provide *useful* error messages to the user, we need to parse and refine the user input to a form that we know our application can accept. This step is not necessary, but will be appreciated.
\\
Here, custom exceptions are created to wrap *exceptional* errors that are happening in the application. These exceptions will help split our program execution states into steps of failure starting off with an `InputException`. In order to do this we need the `DeriveDataTypeable` extension, and the `Data.Typeable` import. 


{% highlight haskell %}
data ProgramException = InputException String
                      deriving (Show, Typeable)

instance Exception ProgramException
{% endhighlight %}


Instead of using the `Maybe` type, tracking more specific and descriptive points of failure, or behavior that exhibits more than one possible outcome will improve readability and program execution flow. The `Either` type is made just for this scenario. Either returns one of two datatypes, in the following example, we return a `Program Exception` or a set of valid `FilePath`'s. The function's result is wrapped in IO because we must interact with the filesystem which is outside of the scope of our program.

{% highlight haskell %}
parseMainArgs :: [String] -> IO (Either ProgramException (FilePath, FilePath))
parseMainArgs args = case args of
    []                           -> return $ Left $ InputException "No Arguments Provided"
    [something]                  -> return $ Left $ InputException "Only One Argument Provided"
    (inFile:outDir:something:_)  -> return $ Left $ InputException "Too Many Command Arguments"
    [inFile,outDir]  -> do 
      fileExists      <- doesFileExist inFile
      directoryExists <- doesDirectoryExist outDir
      return $ case (fileExists, directoryExists) of
        (True,  True)  -> Right (inFile, outDir)
        (False, True)  -> Left $ InputException $ "Input File: " ++ inFile ++ " does not exist"
        (True,  False) -> Left $ InputException $ "Output Directory: " ++ outDir ++ " does not exist"
        (False, False) -> Left $ InputException $ "Input File: " ++ inFile ++ " does not exist" 
                                                    ++ " and Output Directory: " ++ outDir ++ " does not exist"

{% endhighlight %}

## throw an Exception (Functionally)

A programming language wouldn't be super useful if there was no method to abort program execution and signal program termination. Haskell does have this functionality, but it prohibits you from using it anywhere outside of IO (unless you use the `System.Unsafe` module!!). In this cpu log parser, program execution will be aborted and will [fail fast](https://en.wikipedia.org/wiki/Fail-fast) if the user input is invalid by using the `throw` function.
\\
In `main` we also chain all of our parsing arguments together with [`(>=>)`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html#v:-62--61--62-)  as `readLog >=> transformLog >=> getCpuLog` which converts a `String` to a `CpuLog`.
\\
Additionally, because we are using lazy IO in haskell we need a way to perform all the file-write-IO on our list of `CpuLog`s. This can be done with `mapM_`, a function that is useful for executing side-effects over a list. The type of it is  
\\
`mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()` 
\\
\\
which in our case turns into  
\\
`mapM_ :: (CpuLog -> IO ()) -> [CpuLog] -> IO ()`

The `map :: (a -> b) -> [a] -> [b]` function would not work here because the `[b]` result of `map` would not get evaluated as nothing in the Haskell program uses the results from `cpuLogWrite`. We also utilize `mapMaybe` because not all rows from the file will be cpu-related logs.

{% highlight haskell %}
main :: IO ()
main = do 
    args        <- getArgs
    parsedArgs  <- parseMainArgs args
    case parsedArgs of
      Left e                 -> putStrLn "Input Error" >> throw e
      Right (inFile, outDir) -> withFile inFile ReadMode (processFile outDir)
  where 
    processFile dir inHandle = do
        content <- hGetContents inHandle
        let rows = lines content
            cLogs = mapMaybe (readLog >=> transformLog >=> getCpuLog) rows
        mapM_ (`cpuLogWrite` dir) cLogs
{% endhighlight %}

In the above snippet, if there is an error with the input, a message is printed to the command line and the error is thrown just as you would in Java. It's important to note that the code reserved throwing errors for only truly exeptional program behavior, and execution paths that cannot be recovered from. `Maybe` and `Either` were used in placed where program execution may not have been successful, and possible unsuccessful program states (which is `Nothing` for `Maybe` and `Left` for `Either` in our scenario) are tracked until they are considered unrecoverable.

## Part II
This wraps up the first version of the log parser, but there are a few goofy looking parts of the code. Noteably by the use of regular expressions, and organization/structure of the log models. Given all our time spent parsing logs, it would be a convenient time to put them into a more universal format such as JSON so other applications can more easily read them in. In addition, limiting the repetitious opening and closing of files should offer a generous speedup, and adding more program exception states will help document failure points of the application.













