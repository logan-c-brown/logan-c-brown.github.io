---
title: "Tech Lit. Review: Programming Scala 2nd Edition (O'Reilly)"
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

Scala doesn't have the bookshelf girth that, say, Python or Java occupies. Despite being over 15 years old at the time of this blog post, there are probably only a handful of Scala books that are worth the read for the new Scala programmer. The O'reilly animal series of books typically don't dissapoint due to both their consistent content setup, and (onbiously) the animal on each every book cover. Programming Scala reads like a live demo of Scala with questions, answers, and explanations all tucked away in each chapter.

<!--more-->

Programming Scala takes a solid, three chapters to intro the syntax of Scala. Most programming books will have this, but Programming Scala mixes in some community standards and snippets that help teach good Scala code. For example, the chapter, "Type Less, Do More" intros common syntax and types, and it also features a snippet on why null should be avoided in Scala in favor of Option, Some, and None. This should be brought up immediately in every intro to Scala book out there. After the first three intro-like chapters, the text dives immediately into more advanced concepts such as pattern matching and implicits. Implicits are usually the last topic to be referenced in any Scala text, so finding it so early on was unusual. Further on, we'll find notable chapters on the type system, objects in Scala, and functional programming.

> [Option, Some, and None] should be brought up immediately in every intro to Scala book out there

As a personal anecdote, Programming Scala did the best job of teaching me advanced Scala syntax rules such as visibility and types. As my only previous experience had been Python, and moreso the kind of Python code you would find in a jupyter notebook, there are lots of new programming language features to learn. "Visibility Rules" was surprising to say the least. As a Python developer, why should any function ever be private? These tools are supposedly to help engineer the inside and outside of an application, and keep them distinct -- I wished I had learned about this earlier. Chapter 14 and 15 demo through Scala's type system, a topic you can write a whole book about. Path-dependent types, type projections, and all the different "bounds" will keep the attentive reader alert, and they certainly contain the most valuable pages of the book. 


Chapter 17 until the end of the book reads a bit less like the explanation-driven earlier chapters, and more like an intro to a set of tools and activities the active Scala developer will encounter with their day-to-day operations. These application-focused chapters help answer the question "What can I really do with Scala other than write cool pattern matching statements?". "Metaprogramming: Macros and Reflection" frames the last bit of text on a 'Scala can do this too' note which is an already common chord Scala plays. If you are already experienced with Scala, you may find some of these chapters, like "Scala for Big Data", redundant. For those very new to Scala, these chapters expand into topics that you'll want to specialize in, or research more into. 


Application Design is usually a bit different in Scala than, say, Python or Java, so it deservedly needs its own chapter(s). For example, creating a Scala-like Builder or Visitor pattern may not be immediately clear. "Application Design" walks through the common programming design patterns in a Scala-like way and alludes to some ways Scala can help solve some of them. Don't skip through pages 510-514 -- it's not just like 'another mediocre blog post on design patterns' -- each design pattern snippet weighs in on how Scala impacts it. Although some of the more in-depth code examples rely on defunct or soon-to-be defunct Scala such as the CanBuildFrom idiom and the metaprogramming design in version 2.11.x, this text is by no means outdated despite being released in 2014 (almost 6 years at the time of this blog post) and being one of the older Scala programming books. 

> Don't skip through pages 510-514 -- it's not just like 'another mediocre blog post on design patterns'


Given only one book to learn Scala, Programming Scala would be a grande choice. There is plenty of references inside, and it has enough coverage over the language as a whole to create a holistic view of what programming in Scala actually is. Scala is a bit more than a 'better Java', and it's not so focused on being a fully-fledged functional programming language. Bring out a set of highlighters and sticky notes because there are many lines that can get thrown right onto a lecture slide. As a novice programmer, the material is a bit dense, but deservedly so. Given enough time to pay attention to the plethora of code samples, understanding what is equally the most powerful and most confusing feature of Scala, the syntax, will yield enough satisfaction that programming in any other language could feel verbose or anemic. 