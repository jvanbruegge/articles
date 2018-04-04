# Does a Map work?

Sometimes, as a developer, you want to store key-value pairs. For example you may want to get a user object by its username. The best suited data structure for this is a Map. It allows us to get a value by its key in `O(1)`. If you are not familiar with Big-O notation, check out [my article]() about it.

Let's take the simplest Map you can think of: an array! It stores key-value-pairs, but the keys are contraint to only natural integers. As we all know randon array access is `O(1)`. But why?

In memory an array is one **continuous** block of memory. You additionally have to know where the array starts in memory and how big the individual elements are. To get a random element, you simply take the index, multiply it with the element size and add the start of the array:
```
memory_location_to_fetch = index * size_of_single_element + start_of_array
```

# Hashing

Ok, but what if we want to use strings to index our elements? Here we will use hashing. A hashing function takes an input of **arbitrary** length and produces a deterministic output of **fixed** length. This is accomplished by the modulo operator.

```
hash = f(x) ℅ size_of_map
```
`f(x)` is some arbitrary function that is dependent on the implementation. The easiest function would be the *identity*: `f(x) = x`.

And what about strings as keys? Or objects?

# Everything is an integer

If we go back to the computer memory, everything is stored in binary. But nothing prevents you from *interpreting* that binary as a normal integer. Let's use a simple string as example. C-strings are just the ASCII characters in one continuous chunk of memory suffixed with a zero byte. To convert from (binary-)number to ASCII, the [ASCII table](http://www.asciitable.com/) is used. As example we will translate the string `Hello` into its number representation (in hex here, because it's easier to translate to binary than decimal):
```
 H     e     l     l     o     \0
0x48  0x65  0x6C  0x6C  0x6F  0x00
```
We can now simply interpret the 6 bytes as one number and we can use this number as input for our hash function.

# Hash collisions

As you might have already asked yourself, what happens if two different values get hashed to the same result? For example , we have a map of size `10` and put in `0`, `10`, `20`, etc?

For this we can implement an avoidance strategy:
- Linear hashing: If spot is already occupied, move new value by fixed amount (often 1)
- Double Hashing: We simply hash our hash and put our element there
- Using a linked list as array elements (so collisions get appended)

# Bringing it all together

We will now implement a Map that first will only work for integers. As we have already seen we can represent every type as integer, so we can add other types later.

First what we actually store inside our Map
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
Can you see what collision avoidance method we have used? If you said linear hashing, you are right! If our spot is already occupied, we go one right. 
