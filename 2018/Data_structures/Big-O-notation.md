# What does "Big-O notation" mean anyway?

If you are a programmer, chances are that you have stumbled over the term "Big-O notation". But what does this actually mean?

Big-O notation is used to specify the **computational complexity** of your program or data structure. This means basicly means: How many steps do I need to perform a certain action. "Step" in this context does not mean CPU cycles or lines in a program, but arbitrary steps with a **fixed** length, no matter how big they are.

Let's start with an example: Check if an element is in an array. In JavaScript you would implement this like this:
```js
function findInArray(array, element) {
    for(let i = 0; i < array.length; i++) {
        if(array[i] === element) {
            return true;
        }
    }
    return false;
}
```
You can see it quite easily: Everything inside of the loop takes a constant time: Accessing an element in an array with the index, a comparison and a return. Let's define this as 1 "step". But because of the `for` loop, we do this `n` times. In Big-O: `O(n)`.

Other standard examples that are described in Big-O notation are sorting algorithms. Take the most simple sorting algorithm as example, bubble sort. Its implementation could be this:
```js
function bubblesort(array) {
    for(let i = 0; i < array.length; i++) {
        for(let j = 0; j < array.length - 1; j++) {
            if(array[j] > array[j + 1]) {
                let tmp = array[j];
                array[j] = array[j + 1];
                array[j + 1] = tmp;
            }
        }
    }
}
```
For every element in the array it runs through the whole array and swaps if needed. As before, everything in the loop is constant. Then we have a loop doing this `n` times. But we additionally have a second loop that executes the first loop `n` times. So we execute `n` steps `n` times. In Big-O notation: `O(n*n) = O(n²)`

But there are also other cases where the answer is not so straight forward. Take this code snippet for example:
```js
function mapAndFilter(x) {
    return x
        .map(n => n * 2)
        .filter(n > 0);
}
```
What is the complexity of this function? Is it
- `O(1)`?
- `O(n)`?
- `O(n²)`?
- Something different?

The correct answer is `O(n)`. Why? While our code in itself is constant (no direct loops), executing other functions will also take some time. In the map case the browser will have to run trough all elements and call the function on them (so this would be `O(n)`) and the filter is basicly the same as our `findInArray`, so also `O(n)`. Together our function would have `O(n+n) = O(2n)`, but Big-O notation does not care about constant factors, so the `2` gets erased resulting in `O(n)`.

I think this is a good example that you always have to check not only your own code, but also code you import through a library or through the browser. Every algorithm that operates on a datastructure has a **computational complexity**. If you know it, you can write your code to have the smallest complexity possible. And you have to ask yourself what operations do I need to do on my data structure so I can do my job and is there another data structure that has less complexity for those.

This is the first article in a series about common data structures and how they work internally. Follow me if you are interested in new posts about this topic.
