# What the heck is polymorphism?

Polymorphism is the idea of defining data structures or algorithms in general, so you can use them for more than one data type. The complete answer is a bit more nuanced though. Here I have collected the various forms of polymorphism from the common types that you most likely already used, to the less common ones, and compare how they look in object-oriented or functional languages.

## Parametric polymorphism

This is a pretty common technique in many languages, albeit better known as "Generics". The core idea is to allow programmers to use a wildcard type when defining data structures that can later be filled with any type. Here is how this looks in Java for example:

```java
class List<T> {
    class Node<T> {
        T data;
        Node<T> next;
    }

    public Node<T> head;

    public void pushFront(T data) { /* ... */ }
}
```

The `T` is the type variable because you can later "assign" any type you want:

```java
List<String> myNumberList = new List<String>();
myNumberList.pushFront("foo");
myNumberList.pushFront(8) // Error: 8 is not a string
```

Here the list can only contain elements of type string and nothing else. We get helpful compiler errors if we try to violate this. Also, we did not have to define the list for every possible data type again, because we can just define it **for all** possible types.

But not only imperative or object oriented languages have parametric polymorphism, it is also very common in functional programming. For example in Haskell, a list is defined like this:
```haskell
data List a = Nil | Cons a (List a)
```
This definition means: A list takes a type parameter `a` (everything left of the equals sign defines the type) and is either an empty list (`Nil`) or an element of type `a` and a list of type `List a`. We don't need any external `pushFront` method, because the second constructor already does this:
```haskell
let emptyList = Nil
    oneElementList = Cons "foo" emptyList
    twoElementList = Cons 8 oneElementList -- Error: 8 is not a string
```

## Ad-hoc polymorphism

This is more commonly known as function or operator overloading. In languages that allow this, you can define a function multiple times to deal with different input types. For example in Java:

```java
class Printer {
    public String prettyPrint(int x) { /* ... */ }
    public String prettyPrint(char c) { /* ... */ }
}
```

The compiler will automatically choose the right method depending on the type of data you pass to it. This can make APIs easier to use as you can just call the same function with any type and you do not have to remember a bunch of variants for different types (à la `print_string`, `print_int`, etc).

In Haskell, Ad-hoc polymorphism works via type classes. Type classes are a bit like interfaces in object oriented languages. See here for example the same pretty printer:
```haskell
class Printer p where
    prettyPrint :: p -> String

instance Printer Int where
    prettyPrint x = -- ...

instance Printer Char where
    prettyPrint c = -- ...
```

## Subtype polymorphism

Subtyping is better known as object oriented inheritance. The classic example is a vehicle type, here in Java:

```java
abstract class Vehicle {
    abstract double getWeight();
}

class Car extends Vehicle {
    double getWeight() { return 10.0; }
}

class Truck extends Vehicle {
    double getWeight() { return 100.0; }
}

class Toyota extends Car { /* ... */ }

static void printWeight(Vehicle v) {
    // Allowed because all vehicles have to have this method
    System.out.println(v.getWeight()); 
}
```
Here we can use **any** child class of the vehicle class *as if* it was a vehicle class directly. Note that we cannot go the other way, because not every vehicle is guaranteed to be a car for example.

This relation gets a bit hairy when you are allowed to pass functions around, here for example in Typescript:
```ts
const driveToyota = (c: Toyota) => { /* ... */ };
const driveVehicle = (c: Vehicle) => { /* ... */ };

function driveThis(f: (c: Car) => void): void { /* ... */ }
```
Which of the two functions are you allowed to pass to `driveThis`? You might think the first one, after all as we have seen above a function that expects an object can also be passed its subclasses (see the `printWeight` method). **But** this is wrong if you pass a *function*. You can think of it like this: `driveThis` wants something that can accept **any** car. But if you pass in `driveToyota`, the function can only deal with Toyotas which is not enough. On the other hand if you pass in a function that can drive any *vehicle* (`driveVehicle`), this also includes cars, so `driveThis` would accept it.

As Haskell is not object oriented, subtyping would not make much sense.

## Row polymorphism

Now we come to the less commonly used types of polymorphism that are only implemented in very few languages.

Row polymorphism is like the little brother of subtyping. Instead of saying every object in the form `{ a :: A, b :: B }` **is also** a `{ a :: A }`, we allow to specify a row extension: `{ a :: A | r }`. This makes it easier to use in functions, as you do not have to think if you are allowed to pass the more specific or the more general type, but instead you just check if you type matches the pattern. So `{ a :: A, b :: B }` matches `{ a :: A | r }` but `{ b :: B, c :: C}` does not. This also has the advantage that you do not loose information. If you cast a `Car` to a `Vehicle` you have lost the information which specific vehicle the object was. With the row extension, you keep all the information.

```purescript
printX :: { x :: Int | r } -> String
printX rec = show rec.x

printY :: { y :: Int | r } -> String
printY rec = show rec.y

-- type is inferred as `{x :: Int, y :: Int | r } -> String`
printBoth rec = printX rec ++ printY rec
```

One of the most popular languages implementing row polymorphism is [PureScript](http://www.purescript.org/) and I am also [currently working on bringing it to Haskell](https://github.com/ghc-proposals/ghc-proposals/pull/180).

## Kind polymorphism

Kinds are sort of the types of types. We all know values, it's the data that every function deals with. `5`, `"foo"`, `false` are all examples of values. Then there is the type level, describing values. This should also be very known to programmers. The types of the three values before are `Int`, `String` and `Bool`. But there is even a level above that: Kinds. The kind of all types is `Type` also written as `*`. So this means `5 :: Int :: Type` (`::` means "has type"). There are also other kinds. For example while our list type earlier is a type (`List a`), what is `List` (without the `a`)? It still needs another type as argument to form a normal type. Therefore its kind is `List :: Type -> Type`. If you give `List` another type (for example `Int`) you get a new type (`List Int`).

Kind polymorphism is when you can define a type only once but still use it with multiple kinds. The best example is the `Proxy` data type in Haskell. It is used to "tag" a value with a type:

```haskell
data Proxy a = ProxyValue

let proxy1 = (ProxyValue :: Proxy Int) -- a has kind `Type`
let proxy2 = (ProxyValue :: Proxy List) -- a has kind `Type -> Type`
```

## Higher-rank polymorphism

Sometimes, normal ad-hoc polymorphism is not enough. With ad-hoc polymorphism you provide many implementations for different types and the consumer of your API chooses which type he wants to use. But sometimes you as producer of an API wants to choose which implementation you want to use. This is where you need higher-rank polymorphism. In Haskell this looks like this:

```haskell
-- ad-hoc polymorphism
f1 :: forall a. MyTypeClass a => a -> String
f1 = -- ...

-- higher-rank polymorphism
f2 :: Int -> (forall a. MyTypeClass a => a -> String) -> Int
f2 = -- ...
```
Instead of having the `forall` on the outer most place, we push it inwards and therefore declare: Pass me a function that can deal with any type `a` that implements `MyTypeClass`. You can probably see that `f1` is such a function, so you are allowed to pass it to `f2`.

## Linearity polymorphism

Linearity polymorphism is connected to linear types, aka types that track "usage" of data. A linear type tracks the so called multiplicity of some data. In general you distinguish 3 different multiplicities: "zero", for stuff that exists only at type level and is not allowed to be used at value level; "one", for data that is not allowed to be duplicated (file descriptors would be an example for this) and "many" which is for all other data.

Linear types are useful to guarantee resource usage. For example if you `fold` over a mutable array in a functional language. Normally you would have to copy the array at every step or you have to use low-level unsafe functions. But with linear types you can guarantee that this array can only be used at one place at a time, so no data races can happen.

The polymorphism aspect comes into play with functions like the mentioned `fold`. If you give fold a function that uses its argument only once, the whole `fold` will use the initial value only once. If you pass a function that uses the argument multiple times, the initial value will also be used multiple times. Linarity polymorphism allows you to define the function only once and still offer this guarantee.

Linear types are similar to Rust's borrow checker, but Rust does not really have linearity polymorphism. Haskell is [getting linear types and polymorphism soon](https://github.com/ghc-proposals/ghc-proposals/pull/111).

## Levity polymorphism

In Haskell all normal data types are just references to the heap, just like in Python or Java, too. But Haskell allows also to use so called "unlifted" types, meaning machine integers directly for example. This means that Haskell actually encodes memory layout and location (stack or heap) of data types on the type level! These can be used to further optimize code, so the CPU does not have to first load the reference and then request the data from the (slow) RAM.

Levity polymorphism is when you define functions that work on lifted as well as unlifted types.
