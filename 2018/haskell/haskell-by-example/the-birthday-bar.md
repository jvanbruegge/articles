# Haskell by example - The birthday bar

This is the beginning of a series where I show you how to solve coding challenges in Haskell in a proper, functional way. If you do not know Haskell, that is fine, I will explain everything in detail. The problems are from Hackerrank, so you can try them out yourself and play with the implementation.

## The problem

You have a chocolate bar where each of the squares has a number on it. You want to share the bar with your friend, so that the length of the slice is equal to his birth month and the sum of the numbers in those squares is the birth day. You have to print how many of such slices you can make with a given chocolate bar.

As example, you have the chocolate bar `[2, 2, 1, 3, 2]`. Your friend's birthday is at the 4. of February. In this case there are two possible slices: `[2, 2]` and `[1, 3]`, because they have length two (month) and sum 4 (day).

## Input format

We will receive the details via standard input where the first like contains just the length of the chocolate bar, the second like contains the bar - separated by spaces - and the last line is first the birthday and then the birth month, also separated by a space. Our example from earlier would look like this:
```
5
2 2 1 3 2
4 2
```
We know that all values we receive will be greater than zero.

If you don't want to get the solution spoiled, head now to [Hackerrank](https://www.hackerrank.com/challenges/the-birthday-bar/problem) and solve the problem yourself first.

Just a note, the Haskell template of Hackerrank is horrible, do not use it. You will see later the complete solution is much shorter than the template alone.

## The solution

We will start with the actual `solve` function that takes the chocolate bar as a list of integers, the birth day and month. We start by writing the type signature.
```haskell
solve :: [Int] -> Int -> Int -> Int
```
Here `solve` is the name of our function, after the double colon is the type of the function. Every arrow represents a function, everything right of the arrow is what the function returns. So this function takes a list of integers and returns a function that takes an integer and so forth until it just returns an integer. This is because every function in Haskell has only one argument (this is called currying). In practice this does not matter, because the syntax is very convenient. We continue to write the beginning of the implementation, starting with naming our function arguments.
```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d =
```
See? You do not even notice that solve returns a function! But it does, if we would want to call the function, we could do any of this:
```haskell
fn1 = solve [1,2,3,4] -- fn1 takes an integer and returns a function that takes an integer
fn2 = solve [1,3] 4 -- fn2 takes an integer and returns an integer
x = solve [1,3] 3 4
x' = fn1 2 4 -- I can give the rest of the arguments later
```
Now, back to the problem. We want to cut the bar in slices whose length is equal to the month of your friend's birth. For this we write ourselves a little helper function that checks if the bar is long enough to make a new slice and if so recursively calls itself again. Else it just returns an empty list. In Haskell we can add helper functions to the function definition with the `where` keyword.
```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d = {- stuff missing here -} slice bar
    where slice b
            | length b >= m = take m b : slice (tail b)
            | otherwise     = []
```
Here, we use a so called "guard". We split the definition in two and only if the condition before the equals sign is met, we evaluate the right side. As said, first we check if we can make another slice and if so, we `take` `m` elements from `b`. As you already guessed, `take` returns a list of the first `m` elements in the list `b`. We then use `:` to prepend the new list to the list returned by `slice`. The argument to `slice` is using `tail` on the input, which returns the list without the first element.

Let's play through this function by hand. The nice thing about functional programming is that you can just replace definitions with their implementations to see what is evaluated.
```haskell
m = 2
b = [2,2,1]
x = slice b -- length b is greater than 2, so we use the first definition
x = take 2 b : slice (tail b) -- replace tail and take with their results
x = [2,2] : slice [2,1] -- replace slice with definition, still the first one
x = [2,2] : (take 2 [2,1] : slice (tail [2,1])) -- replace tail and take again
x = [2,2] : ([2,1] : slice [1]) -- replace slice again, this time the second definition
x = [2,2] : ([2,1] : []) -- prepend list to empty list
x = [2,2] : [[2,1]] -- do again
x = [[2,2], [2,1]] -- now we got a list of list, with the possible slices
```

Now we are only interested in those slices which sum is equal to the date of birth. For this we can `filter` the list.

```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d = filter fn $ slice bar
    where slice b
            | length b >= m = take m b : slice (tail b)
            | otherwise     = []
```
The `$` operator in Haskell is to avoid parenthesis. It evaluates the thing on the right first, before using it as argument to the left. `foo $ bar x` is the same as `foo (bar x)`.

Currently there is still this `fn` in there that we have not defined yet. We could write another helper function, but we don't have to. We can compose existing functions to form the function we need. Let's recap what the function should do: Filter will iterate through all lists in the list of slices, so our function will receive a single slice. First we have to take the sum of that list and then we have to check if that value is equal to the day of birth.
```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d = filter ((== d) . sum) $ slice bar
    where slice b
            | length b >= m = take m b : slice (tail b)
            | otherwise     = []
```
That's it! In Haskell, operators are also just functions, so we can also only provide a part of their arguments. This is what we do here: `(== d)` returns a function that checks if the input is equal to `d`. This input comes from the `sum` function that takes a list of numbers and adds them up.

The last thing missing is to count how many such slices there are. For we just need the length of our filtered list.
```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d = length $ filter ((== d) . sum) $ slice bar
    where slice b
            | length b >= m = take m b : slice (tail b)
            | otherwise     = []
```
Here you can also see: Composition (the `.`) and application (the `$`) are directly related. It depends on the point of view. At the moment our point of view is: "Evaluate the slices of bar, then apply the result to the filter function and then apply that result to the length function". But we can change our point of view very easily:
```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d = length . filter ((== d) . sum) . slice $ bar
    where slice b
            | length b >= m = take m b : slice (tail b)
            | otherwise     = []
```
Now our view is: "apply bar to the function that is the result of composing length with filter and slice". The code still does the same, but (at least in my opinion) the meaning changed a bit.

## Making it executable

The core of our solution is done, but now we need to process the data from standard input and print the result to standard output. In Haskell the entry point of every program is `main`. It is not a function, but a value in fact. Because in Haskell IO is also a value that you can pass around!
```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d = -- omitted

main :: IO ()
main = putStrLn "Hello"
```
Here, `putStrLn` is function that takes a string and returns such an `IO` value. Note that in the type signature, the parenthesis does not stand for function, but for the empty tuple, also called unit. It is just there because `IO` needs to "contain" a value, so we give it unit.

But we don't want a function that takes a string and does IO, we would like a function that reads and writes from stdin/stdout lets us handle just the strings. There is also a function for this: `interact`. It takes a function from string to string and returns IO.
```haskell
interact :: (String -> String) -> IO ()
```
So now we have to create a function that takes a string in the input format from above and returns a string with the result, calling our `solve` function somewhere in between. We will use function composition for this again, step by step. First, we want to split the input into the individual lines. We can use the `lines` function for that, it takes a string and returns a list of strings. Then we are not interested in the first line, so we can use `tail` from earlier to get rid of the first element in that list of lines:
```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d = -- omitted

main :: IO ()
main = interact $ tail . lines
```
Now *for each line* (aka element in the list), we want to first split the string into words and then *for each word* we want to convert the string to an integer. Every time we want to convert every element in a list in Haskell we use the `map` function. It takes a function and a list of values and returns a list with the function applied to every element in the list.
```haskell
map :: (a -> b) -> [a] -> [b]
------
x = map (+1) [1,2,3] -- x is [2,3,4]
```
You see that the type signature uses lower case letters and not capitalized words like earlier. This means that this is a type variable, so you can put any type you want in there. Now let's convert the sentence above into a function:
```haskell
solve :: [Int] -> Int -> Int -> Int
solve bar m d = -- omitted

main :: IO ()
main = interact $ map (map read . words) . tail . lines
```
The outer `map` will apply the inner function for each line it receives. The inner function first splits the line into words and then applies `read` to every element in the list of words.

At the moment our function takes a string and returns a list of list of integers, where the first element in the list is the bar (or the list representing the bar) and the second element is the list consisting of the day and the month of your friend's birth. Now we could call `solve`, but the arguments do not match, solve does not want a list of lists, but three arguments instead. Because we always know how the list of lists will look, we can use pattern matching to extract the arguments from the list.
```haskell
solve :: [[Int]] -> Int
solve [bar, [m, d]] = -- omitted, did not change

main :: IO ()
main = interact $ solve . map (map read . words) . tail . lines
```
You can see we first match on the outer list, naming the first element `bar`. Then we further match on the second element and name its elements `m` and `d`. We also have to adjust our type signature accordingly. In a production environment, you would factor the pattern matching out in a different function, as the original type signature of `solve` was a lot more meaningful than the new one. But as this is only a coding challenge we will not bother to do that.

The last step needed is to convert the integer returned from `solve` back to a string (remember `interact` takes a function from string to string). For this there is the function `show` in haskell.
```haskell
solve :: [[Int]] -> Int
solve [bar, [m, d]] = length . filter ((== d) . sum) . slice $ bar
    where slice b
            | length b >= m = take m b : slice (tail b)
            | otherwise     = []


main :: IO ()
main = interact $ show . solve . map (map read . words) . tail . lines
```
And with that, our solution is complete!

## Closing thoughts

As you could see, when using a functional language, you do not try to tell the computer *how* to do something, but more *what* you want to do. This is one of the reasons why function composition is preferred over function application (within reason of course).

I really enjoy writing Haskell daily, but I know it's a daunting language. I hope this series will make it a bit easier to "grasp" the language. Thank you for reading.
