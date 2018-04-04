# How does a Map work?

Sometimes, as a developer, you want to store key-value pairs. For example you may want to access a user object by its username. The best suited data structure for this is a **map**. It allows us to get a value by its unique key in `O(1)`. If you are not familiar with Big-O notation, check out [my article](https://dev.to/supermanitu/what-does-big-o-notation-mean-anyway--1hea) about it.

Let's take the simplest map you can think of: an array! It stores key-value-pairs, but the keys are limited to natural integers only. Random access in an array is `O(1)`. But why?

In memory an array is one **continuous** block of memory. You additionally have to know where the array starts in memory and how big the individual elements are. To access a random element, you simply take the index, multiply it with the element size (assuming a uniform element size) and add the start address of the array:
```
memory_location_to_fetch = index * size_of_single_element + start_of_array
```

# Hashing

Ok, but what if we want to use strings to index our elements? Here we will use a technique called _hashing_. A hashing function takes an input of **arbitrary** length and produces a deterministic output of **fixed** length. This is normally accomplished by the modulo operator.

```
hash(x) = f(x) ℅ size_of_map
```
`f(x)` is some arbitrary function that is dependent on the implementation. The easiest function would be the *identity*: `f(x) = x`.

And what about strings as keys? Or objects?

# Everything is an integer

If we go back to the computer memory, everything is stored in binary. But nothing prevents you from *interpreting* that binary as a normal integer. Let's use a simple string as example. C-strings are just the ASCII characters in one continuous chunk of memory suffixed with a zero byte. To convert from (binary-)number to ASCII, the [ASCII table](http://www.asciitable.com/) is used. As example we will translate the string `Hello` into its number representation (in hex here, because it's easier to translate to binary than decimal):
```
 H     e     l     l     o     \0
0x48  0x65  0x6C  0x6C  0x6F  0x00
```
We can now simply interpret these 6 bytes as a number, and we can use this number as input for our hash function.

# Hash collisions

As you might have already asked yourself, what happens if two different values get hashed to the same result? For example , we have a map of size `10` and put in `0`, `10`, `20`, and so on?

For this we can implement an avoidance strategy:
- Linear hashing: If spot is already occupied, move new value by fixed amount (often 1)
- Double Hashing: If sport is accupied, simply hash the hash again and put our element there
- Using a linked list as array elements and appending collision. This effectively gives us "buckets" filled with objects that have the same hash value.

# Bringing it all together

We will now implement a map that first will only work for integers. I will be using Typescript for the examples here.

As we have already seen we can represent every type as integer, so we only care about integers for now.

First what we actually store inside our map:
```ts
class OurMap<Value> {
    private array: [number, Value][];

    constructor(size: number) {
        this.array = new Array(size);
    }
}
```
We just define an array of a fixed length that stores our key-value pairs.

Now we add an `insert` method:
```ts
class OurMap<Value> {
    private array: [number, Value][];

    constructor(size: number) {
        this.array = new Array(size);
    }

    public insert(key: number, value: Value) {
        const origIndex = this.hash(key);
        let index = origIndex;
	while(this.array[index] !== undefined
	  && this.array[index][0] !== key) {
	    index = (index + 1) % this.array.length;
            if(index === origIndex) {
	        throw new Error('Map is already full');
	    }
	}
	this.array[index] = [key, value];
    }
}
```
Can you see what collision avoidance strategy we have used? If you said linear hashing, you are right! If our spot is already occupied, we go one to the right. 

The `get` method works exactly the same as the insert method, it just doesn't check for `undefined` but for the key you passed in.

Deletion is also similar, but we additionally have to check for elements on the right. If there is an element it might have got there by our linear hashing so if the original element is missing the `get` will not try to look at the right. We have to shift those elements back to the left. This, of course, could lead to further corrections.

# Bonus round: Sets

You might have also stumbled across **sets**. As it turns out, you can implement a set with a map. In older versions of Google Chrome, you were able to see this in the DevTools. There a Set had the same properties as a Map (both keys and values instead of just values).

Let's copy that approach by simply using our keys as values.
```ts
class OurSet {
    private map: OurMap<number>;

    constructor(size: number) {
        this.map = new OurMap<number>(size);
    }

    public insert(value: number) {
        this.map.insert(value, value);
    }
}
```

This works because using the same key will always lead to the same memory location. So if there is already the key you overwrite it which doesn't change anything. If there is no entry you can know that this key does not yet exists in the Set.

# Putting it into perspective

Data structures are all about tradeoffs and use cases. So where does our map fit in?

Maps, just like arrays, have `O(1)` insertion, random access and deletion. This depends on the implementation of the map though. Both have to be allocated with a fixed size upfront. If you need a dynamicly growing data structure, you should use a linked list. It is also `O(1)` for insertion and sequencial access, but is `O(n)` for deletion and random access.

You would use an array if you just want to store multiple elements for easy iteration. Use a map if you need a key-value lookup. You could use an array, but maps deal with hash collisions while an array will just overwrite the old value.

# Conclusion

I hope you have learned something new in this article. This is part two of my series on data structures, you can find part one (about Big-O Notation) [here](https://dev.to/supermanitu/what-does-big-o-notation-mean-anyway--1hea). Feel free to share any feedback.
