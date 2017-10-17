Monday, October 16, 2017

# Advice for Haskell beginners

This post summarizes advice that I frequently give to Haskell beginners asking how to start out learning the language

First, in general I recommend reading the [Haskell Programming from first principles](http://haskellbook.com/) book, mainly because the book teaches Haskell without leaving out details and also provides plenty of exercises to test your understanding. This is usually good enough if you are learning Haskell as your first language.

However, I would like to give a few additional tips for programmers who are approaching Haskell from other programming languages.

## Learn Haskell for the right reasons
Some people learn Haskell with the expectation that they will achieve some sort of programming enlightenment or nirvana. You will be disappointed if you bring these unrealistic expectations to the language. Haskell is not an achievement to unlock or a trophy to be won because learning is a never-ending process and not a finish line.

I think a realistic expectation is to treat Haskell as a pleasant language to use that lets you focus on solving real problems (as opposed to wasting your time fixing silly self-induced problems like null pointers and "undefined is not a function").

## Avoid big-design-up-front
Haskell beginners commonly make the mistake of trying to learn as much of the language as possible before writing their first program and overengineering the first draft. This will quickly burn you out.

You might come to Haskell from a dynamically typed background like JavaScript, Python, or Ruby where you learned to avoid refactoring large code bases due to the lack of static types. This aversion to refactoring promotes a culture of "big-design-up-front" where you try to get your project as close to correct on the first try so that you don't have to refactor your code later.

This is a terrible way to learn Haskell, for two reasons. First, Haskell has a much higher ceiling than most other programming languages, so if you wait until you hit that ceiling before building something you will wait a looooooong time. Second, refactoring is cheap in Haskell so you don't need to get things right the first time.

You will accelerate your learning process if you get dirty and make mistakes. Write really ugly and embarrassing code and then iteratively refine your implementation. There is no royal road to learning Haskell.

## Avoid typeclass abuse
Specifically, avoid creating new typeclasses until you are more comfortable with the language.

Functional programming languages excel because many language features are "first class". For example, functions and effects are first class in Haskell, meaning that you can stick them in a list, add them, nest them, or pass them as arguments, which you can't (easily) do in imperative languages.

However, typeclasses are not first-class, which means that if you use them excessively you will quickly depend on advanced language features to do even simple things. Programming functionally at the term-level is much simpler and more enjoyable than the type-level Prolog that type-classes encourage.

Begin by learning how to solve problems with ordinary functions and ordinary data structures. Once you feel like you understand how to solve most useful problems with these simple tools then you can graduate to more powerful tools like typeclasses. Typeclasses can reduce a lot of boilerplate in proficient hands, but I like to think of them as more of a convenience than a necessity.

You can also take this approach with you to other functional languages (like Elm, Clojure, Elixir, or Nix). You can think of "functions + data structures" as a simple and portable programming style that will improve all the code that you write, Haskell or not.

## Build something useful
Necessity is the mother of invention, and you will learn more quickly if you try to build something that you actually need. You will quickly convince yourself that Haskell is useless if you only use the language to solve Project Euler exercises or Sudoku puzzles.

You are also much more likely to get a Haskell job if you have a portfolio of one or two useful projects to show for your time. These sorts of projects demonstrate that you learned Haskell in order to build something instead of learning Haskell for its own sake.

## Conclusion
Hopefully these tips will help provide some guard rails for learning the language for the first time. That's not to say that Haskell is perfect, but I think you will enjoy the language if you avoid these common beginner pitfalls.
