---
title: "Tech Lit. Review: Functional Programming Simplified"
excerpt_separator: "<!--more-->"
categories:
  - Literature Review
tags:
  - Scala
  - Programming
  - Learning
  - Literature Review
  - Blog
  - Functional Programming
---

After jumping into the fundamentals of the Scala programming language, and being somewhat familiar with the Object-Oriented programming paradigm, I decided to investigate the other half of Scala. As a regular on Alvin Alexander's blog, http://alvinalexander.com/, a blog containing intuitive Scala how-to's, I decided to read his book Functional Programming Simplified.

<!--more-->

Every chapter in this book gets straight to the point. Most chapters don't last longer than a few pages so there is no excuse to skip pages in each chapter. As far as reading advice goes, you can get away with jumping around chapters before chapter 42 and the introduction to for expressions. After this chapter, the rest of the book's chapters generally follow a sequence, and information from one chapter relies on knowledge learned in the previous chapter, especially for chapters leading up to creation of Debuggable and IO Monad. This isn't to say you shouldn't skip chapters if they are obvious, but they should be really, *really* obvious to be considered worth skipping. Nearly nowhere during reading was any chapter particularly omitable.

> Most chapters don't last longer than a few pages so there is no excuse to skip pages in each chapter.


Compared to other Scala programming books such as Programming in Scala (Odersky) and Programming Scala (Wampler & Payne), which are both very good books in their own regard, the focus of Functional Programming Simplified is to investigate very fundamental functional programming concepts like the functor and monad by building the very small learning blocks through many small chapters. When composed together, the learning blocks form reasons for how functional programming help solve common domain concepts. Eventually after you put many chapters together, you'll see why functional programming is a good fit for many domain problems. For example, chapters 28 through 38 focus on recursion, and by the first page of chapter 39, you'll have a good understanding of the difference between the iterative and mutational approach you already know, to the new, concise and recursive functional approach to problem solving. Recursion never felt intuitive for me, and I struggled through it when I inevitably came across it when studying graph-based algorithms. After some time of studying recursion in this book, I had flashbacks to code I had written before that could be substantially improved with pattern matching and recursion.


> . . . by the first page of chapter 39, you'll have a good understanding of the difference between the iterative and mutational approach you already know, to the new, concise and recursive functional approach.


This book doesn't have practice examples. If you are not a fan of learning by example, I can not think of a better Scala book. Each chapter feels like the review session your professor held just before an exam, you know, the one where they slyly mention one of the review questions will be on the exam. Five or so short and to-the-point practice questions, or even conceptual "Did you understand X?" after several chapters would be appreciated.


Functional programming might seem like a meme paradigm to some. To the naysayers I would advise them to jump to Chapter 74 where Alexander begins to introduce the IO Monad and how to manage state, functionally. The ride doesn't end until chapter 98, so stay alert because if you were to ever use functional programming anywhere in your codebase, starting by tracking errors and state functionally nearly forces you to track them methodologically. As a novice programmer, I came to a few realizations of what makes systems engineering *hard* after reading how to track state.


> . . . if you were to ever use functional programming anywhere in your codebase, starting by tracking errors and state functionally nearly forces you to track them methodologically


Functional Programming Simplified is by no means an intro to Scala book, so coming with a background of a little bit of Scala knowledge will help significantly. If you are interested in learning Haskell at some point, and are already very knowledgeable about other programming paradigms, learning some functional basics through this book is a good start. For those already invested into functional programming, Practical Haskell or Functional Programming in Scala (the big red book) are better bets for you.




