---
toc: true
toc_label: "Getting Started"
classes: wide
title: "Simple Haskell: Parsing Logs (Part II)"
excerpt_separator: "<!--more-->"
categories:
  - Example Problems
tags:
  - Haskell
  - Programming
  - Learning
  - Blog
  - Functional Programming
  - Parsing
  - Attoparsec
  - Streaming
  - Conduit
---



Regular expressions are probably one of the most useful tools any programmer can know, but they can be brittle, especially when they get long, or if composition is needed. This post presents and alternative using parser and parser combinators, along with a short solution with a data streaming library.


<!--more-->
All of the following code snippets are present in this github repo: [logan-c-brown/log-parse-2](https://github.com/logan-c-brown/log-parse-2) 

# Parsing, not Searching
While regular expressions can be very good at finding data, putting multiple regular expressions together is not so straighforward. Checking regular expressions for validity during compilation can not be caught during compilation, so regular expressions typically heavily rely on integration and unit tests to ensure they are not mechanically broken. An alternative is using a parser which defines a grammar to validate or break up syntax. For example, JSON must adhere to certain rules for how it "looks" to be considered JSON. 
\\
The same methodology applied to JSON can be applied to creating a custom grammar for log files. [attoparsec](https://hackage.haskell.org/package/attoparsec) is a pretty solid parser library -- it will be used for the following examples.
Looking back at one of the sample log files, they can look like this.

{% highlight bash %}
Dec  8 00:14:55 x1e kernel: [165049.217848] mce: CPU11: Package temperature/speed normal
Dec 12 23:10:12 x1e kernel: [266722.330915] iwlwifi 0000:00:14.3: Unhandled alg: 0x71b
Dec 12 20:43:14 x1e kernel: [257904.974134] mce: CPU2: Package temperature above threshold, cpu clock throttled (total events = 526180)
Dec 12 20:30:39 x1e kernel: [257146.821163] CPU9 is up
{% endhighlight %}

In the log files above, a common syntax would be that they all start with a date in the form of `MONTH DAY TIME`. The parser for the log files should begin with a date, which can be modeled as follows. 

{% highlight haskell %}
data DateFormat = DateFormat Text Text Text  deriving (Show)
{% endhighlight %}

The data type `Text` is used to represent each of the three components of the log's timestamp. `Text` is a more efficient representation of textual data than `String` is, so it is preferred. To be able to parse the date using the grammar above, the applicative functions `<*>`, `<*`, `*>`, and `<$>` are used to break up the Text data. 

- `<*` means to take the left argument, and ignore the right argument, for example `Just "a" <* Just "b"` yields `Just "a"`
- `<*>` helps apply functions, for example `Just (2 +) <*> Just 2` yields `Just 4`. Take a look at the applicative laws [here](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html).
- `<$>` also helps apply functions, for example `(2+) <$> Just 10` yields `Just 4`. This is just like `fmap`.
- `take` takes *n*-amount of characters
- `skipSpace` consumes and ignores spaces
- `sepBy` ensures that every 2 characters is separated by ':'

{% highlight haskell %}
dateP :: Parser DateFormat
dateP = DateFormat <$> P.take 3
                   <* skipSpace
                   <*> P.takeWhile isDigit
                   <* skipSpace
                   <*> (intercalate ":" <$> P.take 2 `sepBy` char ':')
{% endhighlight %}

That is a parser for the date parser for the format in the log! The great part about using a parser is that not only can it be ensured that there is no undefined behavior at runtime, but the type system can be used to merge together many other different parsers. 

## `[\S]+` (but for Parsers)

After the date comes the machine name which should not be spaces. the function `not . isSpace` is identical to the anonymous function `(\x -> not  ( isSpace x) )`. `P.takeWhile` is used so that the parsing fails if at least one non-space character is not matched.
{% highlight haskell %}
newtype Machine = Machine Text  deriving (Show)

machineP :: Parser Machine
machineP = Machine <$> P.takeWhile1 (not . isSpace)
{% endhighlight %}


## Returning non-Text Data

The `EventTime` type represents the bracketed machine time in the log, as seens as `kernel: [165049.217848]`. A regular expression for this would look like `kernel: \[(\d+)\]`. The advantage of using a parser is that the *type* of the data being parsed can be encoded in the parser. 
\\
Whereas the regular expression will only return string inside the capture group `(\d+)`, we can enforce the that the "capture group" of our parser is always a number at compile time. On the other hand, if the regular expression was changed to `([^\]]+)`, there is no way for the compiler to know what other parts of the code is now broken.
\\
An example of type-enforcement can be utilized with the `double` parser function inside the `timeP` parser below.

{% highlight haskell %}
newtype EventTime = EventTime Double 
                  deriving (Show)

timeP :: Parser EventTime
timeP = do string "kernel: [" >> skipSpace
           num <- double
           string "]"
           pure $ EventTime num
{% endhighlight %}

\\
This parser above looks a bit different than the date parser. To avoid lots of symbols in the code, it can be visually helpful to use the `do` syntactic sugar to combine separate parsers. `timeP` parses text as follows:
1. Ensures that the text fed into the parser begines with "kernel: ["
2. The next few characters could be spaces
3. The next characters must be a valid number, and the result is transformed from `Text` data into the type `Double`
4. The next character must be the closing bracket, `]`
5. The result is returned as an EventTime wrapped in the applicative, `Parser`, using the `pure` function.

 
After event time comes a device name, which does not exist on all the logs.
{% highlight haskell %}
newtype Device = Device Text 
               deriving (Show)
-- the device name like `iwlwifi 0000:00:14.3` in the examples
deviceP :: Parser Device
deviceP = Device <$> ((P.takeWhile1 (/= ':') <* string ":" )
                 <|> string "")
{% endhighlight %}

## Parsing the CPU Information
After the device name comes a cpu number, the conversion between to number is not used, because its fairly unlikely people will be regularly using addition or subtraction on cpu numbers. 

{% highlight haskell %}
newtype CpuNum = CpuNum Text deriving (Show)

cpuNumP :: Parser CpuNum
cpuNumP = do string "CPU"
             string " " <|> string ""
             num <- P.takeWhile1 isDigit
             pure $ CpuNum num
{% endhighlight %}





In addition to the CPU number, information about the status of the CPU will also be stored. This information is shown as something like `Core temperature/speed normal` in the log. Because a CPU should not occupy more than one state, an ADT is a good choice for a model. Ignore the `Generic` type class for now, this will be explained during the JSON parsing section.

{% highlight haskell %}
data CpuState = Online
              | Normal
              | AboveNormal
              | Offline
              | Unknown
              deriving (Show, Generic)
{% endhighlight %}

Each of the cpu states has some sort of representation in the log file. In the next code snippet the `<|>` function is used to choose the first successful parsing of the text that mentions the state of the CPU. For the `Maybe` type, `<|>` can be thought of as `coalesce` for those familiar with SQL. 

In the `cpuStateP` parser, if the text matches "is up" then `Online` is returned. Type-safety ensures that the code can't return anything other than a `CpuState` unless all of the parser transformations using `(>>)` are changed.
{% highlight haskell %}
cpuStateP :: Parser CpuState
cpuStateP = 
        (string "Package temperature/speed normal" <* takeText    >> return Normal )
    <|> (string "Core temperature/speed normal" <* takeText       >> return Normal )
    <|> (string "Core temperature above threshold" <* takeText    >> return AboveNormal )
    <|> (string "Package temperature above threshold" <* takeText >> return AboveNormal )
    <|> (string "is up"                                           >> return Online )
    <|> (string "is now offline"                                  >> return Offline )
    <|> (takeText                                                 >> return Unknown )
{% endhighlight %}

The complete cpu-information parser is combined as:
{% highlight haskell%}
cpuP :: Parser Event
cpuP = do num <- cpuNumP
          br <- string ": " <|> string " "
          st  <- cpuStateP
          pure $ Cpu num st
{% endhighlight %}

## Remainder (r)

The rest of the parser is either this CPU state or a generic event message such as `Unhandled alg: 0x71b`. These are modeled as separate types so in the future more parsing can be added for wifi status, or power status, or anything else in the logs just by adding more statements onto this parser. For now the logs are bucketed into either a cpu-related log, or a generic event log.

{% highlight haskell %}
data Event = Event GenericMessage
           | Cpu CpuNum CpuState 
           deriving (Show)

newtype GenericMessage = GenericMessage Text 
                       deriving (Show)

-- either our cpu status or a generic event message associated with a parser
eventP :: Parser Event
eventP = cpuP <* takeText 
     <|> Event . GenericMessage <$> takeText
{% endhighlight %}

## Putting the Parser Together

All that is left is to combine all of our separate parsers into one big one. This is done exactly as it was for the rest of the parsers -- here the `do` syntax is used. The model of a log is completed here as a "successfully" parsed log as a `KernelLog` and an unknown log which is the original text representation of the log and a `String` which is what attoparsec returns during unsuccessful parses.
{% highlight haskell %}
data Log = KernelLog DateFormat Machine EventTime Device Event 
         | UnknownLog Text String 
         deriving (Show)
{% endhighlight %}

All the parsers are combined and are syntactically separated by spaces.

{% highlight haskell %}
kernelLogP :: Parser Log
kernelLogP = do date <- dateP
                skipSpace
                machine <- machineP
                skipSpace
                time <- timeP
                skipSpace
                device <- deviceP
                skipSpace
                event <- eventP
                pure $ KernelLog date machine time device event
{% endhighlight %}

A function to parse from `Text` to a `Log` representation, where all unsucessful parses are thrown into an `UnknownLog`.

{% highlight haskell %}
parseLog :: Text -> Log
parseLog s = case parseOnly kernelLogP s of
               Left err -> UnknownLog s err
               Right parsedLog -> parsedLog
{% endhighlight %}


# JSON
Of all the serialization formats ever created, JSON is one of the most ubuquitous nowadays being that nearly every language supports encoding and decoding from it, and it's fairly compact, human-readable, and robust.
It would suck to do all that log parsing just to dump them back out in their original format for some other program to pick back up and parse again. 




To make the flow of information easier, JSON will be used to store the parsed log data. However, to separate the internal and external representations of log messages, a new set of models is created. By creating separate types, if the JSON keys on the exported data ever needed to be changed, we would not have to run through our application again and rename the types. Rather, the types should just be changed on the export, and a translation layer converts between the export format and the parsing format. Here the function to to that is `toOutputLog`. 


{% highlight haskell %}
data OutputLog = OutputCpuLog { date      :: !Text
                              , machine   :: !Text
                              , eventTime :: !Double
                              , cpu       :: !Text
                              , state     :: !CpuState }
                | OutputParsedLog { date      :: !Text
                                  , machine   :: !Text
                                  , eventTime :: !Double
                                  , device    :: !Text 
                                  , message   :: !Text } 
                | UnknownParsedLog { logMessage :: !Text
                                   , err        :: !String } 
                deriving (Show, Generic)
{% endhighlight %}

## Haskell Writes Code for You
Thanks to type classes, all the boilerplate code to parse and unparse logs does not have to be written. For those familiar with the `circe` library in Scala, this is called Automatic Derivation. In order to do this in haskell, each type used in the export model must be able to derive `Generic`, including `CpuState` as seen earlier.

Once generic is derived with the help of the `DeriveGeneric` GHC extension and the `GHC.Generics` module, GHC can write all of the JSON encoding functions for us by simply defining the type class instances for OutputLog
{% highlight haskell %}
instance ToJSON CpuState
instance ToJSON OutputLog
{% endhighlight %}

If you are ever dealing with converting to and from JSON, I highly encourage this methodlogy to remove any boilerplate and possibly typos when writing a JSON encoder and decoder manually. The conversion function from the internal and external data representations uses pattern matching against the types in `OutputLog`
{% highlight haskell %}
toOutputLog :: Log -> OutputLog
toOutputLog (KernelLog (DateFormat mo day ts) (Machine b) (EventTime c) _ (Cpu (CpuNum num) state)) = 
  OutputCpuLog { date = unwords [mo,day,ts]
               , machine = b
               , eventTime = c
               , cpu = num
               , state = state}
toOutputLog (KernelLog (DateFormat mo day ts) (Machine b) (EventTime c) (Device d) (Event (GenericMessage mess))) = 
  OutputParsedLog { date = unwords [mo,day,ts]
                  , machine = b
                  , eventTime = c
                  , device = d
                  , message = mess}
toOutputLog (UnknownLog mess err) = 
  UnknownParsedLog { logMessage = mess
                   , err = err}
{% endhighlight %}

All that is left is to write all of the JSON to a file.

# Streaming Data
One of the problems with the previous solution was that the program was repeatedly opening and closing files for every log. To solve this, all the log files for each file can be grouped together first, and then each group is written to a file. The downside of this approach is that all the logs need to be stored in memory before being written -- a not so great scenario for large log files. Alternatively, a reactive approach to creating and writing log files can ensure minimal memory overhead because not all log files need to be read at once. Another solution could involve chunking the input stream to regulate memory consuption. All of these are great ideas, but we'll go with grouping the log files first, to keep the solution simple.  


## Polymorphism
First, a function to group all the logs together is needed. Haskell has functions that are like this such as `groups`, but they don't completely group the data because they are preferenced towards lazy evaluation of lists. Instead a new function is made `groupL`. 

`groupL` is a bit different than the functions used previously in that it doesn't define any restrictions on the types of the arguments of the function other than that the function used to create an identifier to group must have a defined Ordering. These kinds of functions are called *polymorphic*, and it is preferenced that any functions written are polymorphic because they enable easier composition and help to satisfy the DRY principle.

{% highlight haskell %}
import qualified Data.Map as M
import           Data.Map (Map)

groupL :: (Ord b) => [a] -> (a -> b) -> Map b [a]
groupL items f = 
  let tups = map (\x -> (f x, [x])) items
  in  M.fromListWith (++) tups
{% endhighlight %}

For example, groupL are not restricted to using only `Log` data.

{% highlight bash %}
ghci>groupL [1.1,2.3,1.8,4.3,4.0] round
fromList [(1,[1.1]),(2,[1.8,2.3]),(4,[4.0,4.3])]
{% endhighlight %}

## Conduit
Now that the Logs are grouped, a function is needed to write this group of logs to a file. For this `conduit` is used. Although unnecessary for this problem, conduit has some cool syntax and functionality specifically for streaming data to files. Again, polymorphism is retained as much as possible to retain input flexibility.

Outline of `sinkGroup`:
1. Create a conduit source from the list of logs (or anything else that has a `ToJSON` instance)
2. Stringify the logs, this converts all the logs to JSON in bytestring, a very efficient data format, and appends on a newline to each string
3. Create a `sink` where all of the data is streamed to, which is a file accepting bytestring data.
4. Tell conduit to pipe our data from a conduit source to a resource sink with `runConduitRes`

{% highlight haskell %}
import           Data.Conduit
import           Data.Aeson (toJSON, encode)
import qualified Data.ByteString.Char8  as BS
import qualified Data.Conduit.ByteStrng as B
import qualified Data.Conduit.List      as CL

sinkGroup :: (ToJSON b) => (a -> b) -> (FilePath, [a]) -> IO ()
sinkGroup f (fp,logs) =
  runConduitRes $
  CL.sourceList logs
  .| CL.map stringify
  .| B.sinkFile fp
  where stringify log = BS.snoc (toStrict $ encode $ toJSON $ f log) '\n'
{% endhighlight %}

The `.|` syntax is similar to the pipe operator in bash, and its meaning is nearly the same.

The final peice of the puzzle, the main function used in part (I), is slightly modified to accept the new grouping.
`sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()` is used to force evaluation for all of the conduits.
\\
`T.pack :: String -> T.Text`, from `Data.Text` is used to convert the lazy String, to Text data format used in the parser. 

{% highlight haskell %}
main :: IO ()
main = do 
    args        <- getArgs
    parsedArgs  <- parseMainArgs args
    case parsedArgs of
      Left e                 -> throw e
      Right (inFile, outDir) -> withFile inFile ReadMode (processFile outDir)
  where 
    processFile dir inHandle = do
        content <- hGetContents inHandle
        let rows = lines content
            cLogs = map (parseLog . T.pack) rows
            grouped = M.toList $ groupL cLogs (logFilePath dir)
            sinks = map (sinkGroup toOutputLog) grouped
        sequence_ sinks
{% endhighlight %}

This wraps up the creation of a simple kernel log parser. 
