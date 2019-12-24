---
title: "Thoughts after Reading: Programming in Scala 3rd Edition (artima)"
excerpt_separator: "<!--more-->"
categories:
  - Literature Review
tags:
  - Scala
  - Programming
  - Learning
  - Literature Review
  - Blog
---

If all programming texts were lumped into two categories, the first of those categories would be experimental. This group would include texts like Functional Programming Simplified (Alexander) and Scala for the Impatient (Horstmann). These texts get you from point A to point B in the shortest way possible and prioritize coverage of the most important topics. The other category, Classics, read more like a lecture series from a senior, and experienced faculty member. They are slow going, but don't rush topics in favor of thorough chapters that explain, in detail, a variety of examples on the minutiae. Reading both is a necessity for a complete understanding of a programming language, and Programming in Scala, the third edition, falls into the Classics category for a variety of reasons.

<!--more-->

As briefed in the foreword, the book assumes you are familiar with programming. Some chapters do dive into how Scala interacts with Java, but at a level that knowledge about any Java at all is not required. Every chapter is comprehensive, but if you are using the text to learn Scala, jumping around between chapters can cause some confusion, especially if this is your first Scala book. This book has one of the best intros to the Scala type system. The Scala type system is a good reason why many developers choose Scala, so chapter 19 is a very welcome read. Coming directly from python, contravariance and upper and lower type bounds were not immediately intuitive, but reading through the explanations a few times should yield enough understanding to know what is going on when you first come across them in the real world.

> The Scala type system is a good reason why many developers choose Scala, so chapter 19 is a very welcome read.


Many Scala books out there will dive into category theory, or have you re-construct basic functional concepts from scratch. While this can be helpful to get a concrete and fundamental understanding of the hamster running scala, it can be overburdening for those who want to start learning and getting acquainted with a new programming language. If every new programming language required some new branch of mathematics, or required years of syntax research to get up to speed on peer reviewing your coworker's pull request, we would need some benevolent dictator to cull complexity. Thankfully, Scala is not one of these languages because you can start learning with very simple and straightforward code. Programming in Scala defines those languages features that you are most likely to utilize, and it doesn't complicate the process of learning just what Scala is about. Looking for a genuine example with complete clarification? This book has it. The attentive reader will find more value than readers who tend to scan pages for information.


> Programming in Scala defines those languages features that you are most likely to utilize


You'll want this book in pdf format (in addition to paperback as I always recommend). Reason being that the book provides a comprehensive "List of Figures" and a "List of Tables" to serve as excellent reference material if you have some free time to learn something during lunch. Programming books beat out online reference and course material every time, and Programming in Scala is no exception. There is no place you can find on the internet (including my blog) that has as many examples and as smooth of a conceptual learning flow that Programming in Scala has. If you are serious about picking up Scala, either looking for a new job, or buying a this book would be you best bet.


As far as chapter organization goes, this book is smooth. The chapters begin with common object-oriented based code with classes and objects, then gradually introduces functional concepts like case classes, pattern matching, and combinators. This approach makes a lot of sense for java programmers too. A functional concept sneaks its way in during many of the chapters so you never really see the Scala feature 'sticker shock'. Tail recursion -- an invaluable tool of functional programs but not necessarily a strictly functional concept -- is passed in, appropriately during the "Functions and Closures" chapter.   


> There is no place you can find on the internet (including my blog) that has as many examples and as smooth of a conceptual learning flow that Programming in Scala has.

For those new to Scala, but not programming in general, every chapter provides value specifically to the Scala language. Java developers would enjoy chapters 12, 29 and 31. Novice programmers from Python will likely find concept introduction into this book too fast, especially because there is a lack of practice problems which aid in the 'getting comfortable' stage of programming. Many books emphasize the functional aspects of Scala, but Scala really is about how functional programming can interact with object-oriented programming, and it practically allows the functionality of both paradigms. Don't skim this book for the functional concepts or you might get lost on the big ideas of Scala.







