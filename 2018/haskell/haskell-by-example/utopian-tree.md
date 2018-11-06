# Haskell by example - Utopian tree

In this series we solve coding challenges from Hackerrank in Haskell in a proper, functional way. In the last part I did not mention which how to get up and running with Haskell on your own PC, so I want to fix that. The easiest way is to download [Stack](https://docs.haskellstack.org/en/stable/README/). Then create a file with the ending `.hs` somewhere and open a terminal in the same directory. Type `stack ghci` to open an interactive Haskell REPL, this will take some time the first time as stack has to download the haskell compiler first. Once that is done, you can type `:load <filename>.hs` to compile and type check your code. You can also get the type of an expression with `:type <expr>`.

# The problem

A Utopian Tree has two growth spurts every year, one in spring and one in summer. In spring the tree doubles its size, in summer it grows by one meter. You plant a tree that is initially one meter tall. How big is the tree after `n` growth spurts?

To recap what is going on, for `n == 5`:
```
Period  Height
  0       1      We start with a 1 meter tree
  1       2      Double the size
  2       3      Increase by one meter
  3       6      Double again
  4       7      Plus one meter
  5       14     And a final double
```

# Input format

We will receive multiple lines with one number each. The first line is the number of lines that follow. The other numbers are the amount of growth spurts. The goal is to answer for each number how big the tree is afterwards.

So for example (refering to the table from earlier) when we get
```
3
4
2
0
```
We have to return (remember the first line is just the length)
```
7
3
1
```

If you don't want to get the solution spoiled, head now to [Hackerrank](https://www.hackerrank.com/challenges/utopian-tree/problem) and solve the problem yourself first.

Just a note, the Haskell template of Hackerrank is horrible, do not use it. You will see later the complete solution is much shorter than the template alone.

# The solution

If we think a bit about the problem, it boils down to doing one thing: Doubling or incrementing the current height `n` times (where `n` is the number given to us). In imperative programming languages you would use a for loop to do this, but in Haskell there is no for loop. The reason is simple, you are not allowed to change variables, so you cannot change the `i` variable in the loop. But this is not limiting at all, rather it has some interesting consequences: In Haskell, lists double as control structures. What does that mean? You saw it a bit in the last part of this series:

> Now *for each line* (aka element in the list), we want to first split the string into words and then *for each word* we want to convert the string to an integer.

In other languages "for each x" would be a clear signal to use a for loop, but here we used `map` and lists.

For our new problem `map` is not enough as we have to remember the last height of the tree. But luckily there is also a function for that, `foldl` (for those who know JavaScript, `array.reduce` is basically the same function).
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
```
`foldl` takes a function from the remembered value and the current value to a new remembered value as first argument, the initial value as second argument and a list of values as third one. It returns the remembered value at the end.

So, let's start with our solve function, note that this function will only calculate the height of one tree, not all of trees.
```haskell
solve :: Int -> Int
solve n = foldl fn init list
```
Ok, now we have to find the values of `fn`, `init` and `list`. The initial value should be clear, the problem already states that we start with a tree of height 1.
```haskell
solve :: Int -> Int
solve n = foldl fn 1 list
```
The `list` is a bit more complicated. Remember how I said that lists double as control structures (ie a for loop). We want to double and increment the height alternatingly. We can define a list that expresses exactly this behavior:
```haskell
solve :: Int -> Int
solve n = foldl fn 1 {- stuff missing -} [(*2), (+1)]
```
Now we have a list of two functions, both from `Int` to `Int`. But we don't want to do this twice, but more often! Here comes another strength of Haskell into play: Laziness. Laziness means that values are only computed when they are needed. For example:
```haskell
first :: [Int] -> Int
first [a, b] = a

x = first [1, error "this will crash"]
```
This snippet will work just fine, even though we have an run-time error in there. The reason for this is that we never try to use the value of the error, so it never gets evaluated. But again, this has a nice side effect too: Haskell can deal with infinite lists! Of course an infinite list would never fit in memory, but as Haskell only evaluates the stuff it needs, the list never get allocated completely.
```haskell
allEvenNumbers = filter even [1 ..]
x = take 6 allEvenNumbers
```
Here we filter a list of all positive numbers to return only even ones. In other programming languages, this snippet would cause either a stack overflow error or an infinite loop.

Going back to our `solve` function, we can use `cycle` to create an infinite list by repeating the given list forever.
```haskell
solve :: Int -> Int
solve n = foldl fn 1 {- stuff missing -} cycle [(*2), (+1)]
```
But we don't want the result after infinite growth spurts, but after `n`! We already know how to do this from the last part:
```haskell
solve :: Int -> Int
solve n = foldl fn 1 . take n . cycle $ [(*2), (+1)]
```
We simply `take` `n` values from our infinite list. Now the last question is, what should `fn` do? Here we can use the REPL to help us. If we type in our current solution with a placeholder instead of `fn`, Haskell will tell us what it expects:
```
Prelude> foldl _ 1 $ cycle [(*2), (+1)]

<interactive>:7:7: error:
    • Found hole: _ :: b -> (Integer -> Integer) -> b
      Where: ‘b’ is a rigid type variable bound by
               the inferred type of it :: Num b => b at <interactive>:7:1-30
    • In the first argument of ‘foldl’, namely ‘_’
      In the expression: foldl _ 1
      In the expression: foldl _ 1 $ cycle [(* 2), (+ 1)]
    • Relevant bindings include it :: b (bound at <interactive>:7:1)
```
The message might look scary, but let's digest it step by step. The first line tells us already most of what we want to know: The placeholder has type `b -> (Integer -> Integer) -> b`. The third line tells us that `b` has to be a number type (this is what `Num b => b` means). The reason for this is that the `1` in our code could still be any number type. We could add a type signature to force it to be an integer:
```
Prelude> foldl _ (1 :: Integer) $ cycle [(*2), (+1)]

<interactive>:10:7: error:
    • Found hole: _ :: Integer -> (Integer -> Integer) -> Integer
    • In the first argument of ‘foldl’, namely ‘_’
      In the expression: foldl _ (1 :: Integer)
      In the expression: foldl _ (1 :: Integer) $ cycle [(* 2), (+ 1)]
    • Relevant bindings include
        it :: Integer (bound at <interactive>:10:1)
```
As you can see now, we need a function that takes a value and a function and returns the result of the function applied to the value. Here we can make use of another great tool: Hoogle. If we head to [haskell.org/hoogle](https://www.haskell.org/hoogle/?hoogle=Integer+-%3E+%28Integer+-%3E+Integer%29+-%3E+Integer) and enter the type signature we received from the REPL, we get a bunch of results. If we ignore the first few that are more complicated, we quickly see one entry in the list that we know already:
![$](https://i.imgur.com/mAr2c2z.png)

The `$` operator. The only problem is that the arguments are in the wrong order. But this is not an issue as there is a `flip` function that swaps the arguments. So now we can finish our solve function:
```haskell
solve :: Int -> Int
solve n = foldl (flip ($)) 1 . take n . cycle $ [(*2), (+1)]
```
We have two wrap the operator in parenthesis to use it as a normal function and not as an operator.

# Making it executable

Again, the solve function is not enough, we need to read from standard input and write to standard output. We will use the `interact` function for this again.
```haskell
solve :: Int -> Int
solve n = foldl (flip ($)) 1 . take n . cycle $ [(*2), (+1)]

main :: IO ()
main = interact $ -- stuff missing
```
Ok, we need all the lines except the first. We already did this last time:
```haskell
solve :: Int -> Int
solve n = foldl (flip ($)) 1 . take n . cycle $ [(*2), (+1)]

main :: IO ()
main = interact $ tail . lines
```
Now, *for each line* we want to convert the string to a number, run the solve function and convert the number back into a string
```haskell
solve :: Int -> Int
solve n = foldl (flip ($)) 1 . take n . cycle $ [(*2), (+1)]

main :: IO ()
main = interact $ map (show . solve . read) . tail . lines
```
Finally, we want to put a newline between all strings in the list. There is already a function for that called `unlines` - because it is the inverse of `lines`.
```haskell
solve :: Int -> Int
solve n = foldl (flip ($)) 1 . take n . cycle $ [(*2), (+1)]

main :: IO ()
main = interact $ unlines . map (show . solve . read) . tail . lines
```
And that's it! Two lines of code to have a complete solution without going into stuff like code golf. Even more than last time, we did not write a whole lot ourselves, we just composed functions from the standard library to form bigger functions. This is the core of functional programming!

# Performance PSA

While our current solution works and will pass the tests of Hackerrank, it is not suitable for production. The reason is (unwanted) laziness. The definition of `foldl` looks like this:
```haskell
foldl _ x [] = x
foldl f x (y:xs) = foldl f (f x y) xs
```
The problem is that Haskell does not evaluate `(f x y)` at this point, but only allocates a so called "thunk" that tells *how* to calculate it. When we run through the list we can see the problem:
```haskell
x = foldl (+) 0 [1,2,3,4]
x = foldl (+) (0 + 1) [2,3,4]
x = foldl (+) ((0 + 1) + 2) [3, 4]
x = foldl (+) (((0 + 1) + 2) + 3) [4]
x = foldl (+) ((((0 + 1) + 2) + 3) + 4) []
x = ((((0 + 1) + 2) + 3) + 4)
```
At that point in time, none of the expressions in the parenthesis are evaluated, but for each `+` a thunk has to be allocated. This is a huge waste of space and a severe drop in performance. Luckily there is a *strict* (as in "not lazy") version of `foldl` in the standard library: `foldl'`. We can also implement it ourselves with the `BangPatterns` extension (I will go deeper into language extensions in a later post).
```haskell
foldl' _ !x [] = x
foldl' f !x (y:xs) = foldl f (f x y) xs
```
This exclamation mark causes the argument to be strict, ie it will be evaluated before the rest of the function. With it, our evaluation looks like this:
```haskell
x = foldl (+) 0 [1,2,3,4]
x = foldl (+) (0 + 1) [2,3,4]
x = foldl (+) (1 + 2) [3, 4]
x = foldl (+) (3 + 3) [4]
x = foldl (+) (6 + 4) []
x = 10
```
This time, we did not allocate a bunch of thunks, and our function runs fast. We do not have to define this function ourselves though, we can simply import it from the standard library:
```haskell
module Main where

import Data.Foldable (foldl')

solve :: Int -> Int
solve n = foldl' (flip ($)) 1 . take n . cycle $ [(*2), (+1)]

main :: IO ()
main = interact $ unlines . map (show . solve . read) . tail . lines
```
In general, never use `foldl` always use `foldl'`.

# Closing thoughts

I hope this example is easy enough to get more familiar with the language. If you have any question, be it about my code or Haskell in general, feel free to ask them.
