---
title: "Thoughts after Reading: Practical Haskell (2nd Ed. apress)"
excerpt_separator: "<!--more-->"
categories:
  - Literature Review
tags:
  - Haskell
  - Programming
  - Learning
  - Literature Review
  - Blog
---

Haskell is one of those languages that everyone knows, but very few people use. In contrast to other nifty languages like Idris or Coq, the breadth of Haskell learning material for newcomers should surely bring more popularity into the commercial space, but it doesn't and the type of learning material is likely one of the reasons. Compare the search engine results of `sending a GET request in {insert some language here}` with Haskell and a widely used commercial programming language and you'll see why. Applied programming books like "Practical Haskell" help to bridge this gap between tutorials in package documentation and a canonical solution accepted by the community.

<!--more-->

A lot of Haskell tutorials out there quickly jump the gap into advanced Haskell tutorials such as `Functional Dependencies` or `Rank-N Types`. Although these are super valuable areas of interest, learning the _canonical_ way to solve a problem takes some jumping through blogs and package documentations to get an 'ah-HA' moment. After getting to know the basics of Haskell, the steepest part of the learning curve is not the language itself, but the lack of community-driven information about making real applications in the language. Don't get me wrong, there are plenty of excellent blogs and documents covering the language, syntax, and extensions. Working through a practical application of a language speeds up the I-just-need-to-write-something-that-works bump.


>. . . the steepest part of the learning curve is not the language itself, but the lack of community-driven information about making real applications


I began this book knowing nearly nothing about Haskell, and my goal was to be able to write something sort of useful (in Haskell). After getting completely stumped trying to learn Python 3 on Python 2 tutorials, I did not want to repeat the same mistake with a very unfamiliar language. "Practical Haskell" was the most updated, 2019 at the time of purchase, compared to other Haskell books that were written nearly a decade ago. For those also beginning the journey learning the quintessential functional programming language, installing the [Haskell ide engine](https://github.com/Haskell/Haskell-ide-engine) will make the process so much smoother and give active feedback for invalid syntax while writing code.


Part I contains essential material for someone without prior Haskell knowledge. Very few of the programming texts I've read have gone over package and project management tools, but in Haskell, they are the foundation of every well-organized project. You could write an application in Python without ever knowing what pipenv is, (and possibly even pip), but since Haskell's standard library is very small, stack and cabal are essential tools (as opposed to using only the GHC). Practicing the chapters problems on creating stack and cabal projects will make creating something in Haskell so much easier. The intro chapters also feature practice with GHCi, an essential tool for testing and playing with code snippets.  
 
   
Functional languages do collections and containers *very* well. Anytime a new function is brought up in the book, immediately get the type of that function, either through using `:t` in GHCi, or with [Hoogle](https://hoogle.Haskell.org/) (yes, `Hoogle`). Getting used to types and manipulating types, functionally, is a core motive in Haskell. Haskell is one of those languages already experienced programmers adopt, and in doing so, many of the foundational data structures and algorithms are glossed over in Haskell resources. Not every programmer nowadays went through the COMPSCI 120 course on data structures, so texts that introduce these topics (with diagrams!) such as those found in Practical Haskell help bridge the Haskell experience knowledge gap.

Many articles and blog posts go about teaching monads as some weird abstraction and metaphor to try and awe a "Eureka!" moment. The easiest way to go about learning the infamous 'monad' is to learn the two operations that define them `return` and `bind`, and then use them. Don't get caught up in the theory -- unless intended -- just start using them in basic contexts, and knowledge about them will grow the more they are used and manipulated. Chapter 6 ends using RWS and mutable references which are not so introductory. Read through chapter 6 slowly and carefully to understand the details of how monads are used in a K-means algorithm. This is a theme in this text, each chapter starts very simple and approachable, but gets rapidly more difficult towards the end of each one. Steadily decreasing reading pace in each chapter will help keep tune with the step-by-step process in the longer chapter problems.

> The easiest way to go about learning the infamous 'monad' is to learn the two operations that define them `return` and `bind`, and then use them.

Depending on goals, reading just halfway through each chapter will reward the reader with top-level knowledge on the basics of Haskell. For example, chapter 10 explores parsers, but only half of it is really necessary to get a basic understanding of what parsing is. Reading the rest of chapter 10 explores the type classes behind parsers and how to parse JSON, which although not necessary to start writing your own parser, they help solidify the gears and switches behind them. Chapter 8 is similar, whereby it introduces how easily parallel computation in Haskell is, then ramps up to producer-consumer queues.


Chapter 16 ushers in a set of guidelines for building an application in Haskell. These guidelines are incredibly helpful to template out an actual application in Haskell. The Haskell ecosystem has a few remnants of "old ways" from either from Haskell98 or abandoned packages. Being made aware of these remnants will help clear up inconsistencies you find when reading older Haskell material and blog posts. For example, picking a parser combinator library in Haskell is like picking a JSON parsing library in Scala. Find an opinionated guide on programming Haskell so that the common "gotchas" are avoided, or at minimum understood.

>picking a parser combinator library in Haskell is like picking a JSON parsing library in Scala

All the chapters in "Practical Haskell" contain a good starting point for jump-starting your own projects. Haskell is a very versatile language, and although many restrictions are intentionally placed in the language to make code safe, I've never felt restricted from writing a solution once I understood the foundational concepts of Haskell. After getting acquainted to the functional methodology, I realized how unsafe the code I usually wrote was, and knowing how to program functionally in Haskell has also helped me write safer imperative code. As an analogy, teaching a Java programmer Haskell is not the same as teaching a runner how to powerlift, rather teaching a Java programmer Haskell will help uncover concepts and methodology on why a problem exists, be it parallelism or parsing, and that some solutions are due to language restrictions or expressiveness.
