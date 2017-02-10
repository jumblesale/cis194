## What is a functor?

A functor has to have an instance of `fmap`. `fmap` applies a function to a context.

```
class Functor f where  
    fmap :: (a -> b) -> f a -> f b
```

So we can obviously apply this to two instances of Functor, lists and Maybes:

```
instance Functor [] where
    fmap f (x:xs) = f x : fmap xs
```

or:

```
instance Functor [] where
    fmap = map
```

Maybe:

```
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = (Just f x)
```

`<$>` is just the infix operator for `fmap`! So `(+3) <$> (Just 2)` is identical to
 `fmap (+3) (Just 2)`.

 Functors satisfy two laws:

 ```
 -- identity:
 fmap id = id
 -- composition:
fmap (f . g) = fmap f . fmap g
 ```

## Applicatives

Functors are like contexts for values. Applicatives are also Functors but they
 can be contexts for functions! If that sounds crazy hard to understand that's
 because right now it is.

`Maybe` is an Applicative Functor:

```
λ: :t Just (*)
Just (*) :: Num a => Maybe (a -> a -> a)
λ: :t Just (*3)
Just (*3) :: Num a => Maybe (a -> a)
λ: :t Just (3*3)
Just (3*3) :: Num a => Maybe a
λ:
```

So `Maybe` can maybe contain a function.

Applicatives provide two methods: `pure` and `<*>`.

```
class (Functor f) => Applicative f where
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b
```

### `pure`

From the type signature, `pure` clearly puts a value in a context. How do you
 coerce Haskell to put it into a specific context? Well you do it like this:

```
λ: (pure 3) :: Maybe Int
Just 3
λ: pure 3 :: [Int]
[3]
```

Wow ok. So `pure` provides a sort of default context.

```
λ: (pure "hello") :: Either Int String
Right "hello"
```

### `<*>`

What the heck is going on with this operator?


`(<*>) :: f (a -> b) -> f a -> f b`


ok so the f in this is obviously a Functor. Given a Functor `f` and a function
 that maps from `a -> b`, and a Functor containing something of type `a`,
 produce something of type `b` and stick it in the Functor `f`

A function with type `a -> b`: `(*3)`

A Functor containing type a: `Just 3`

So putting that all together:

```
λ: (Just (*3)) <*> (Just 3)
Just 9
```

Huh ok. Let's figue out the instance of these two for `Maybe`.

```
instance Applicative Maybe where
    pure x = Just x
    Nothing <*> _ = Nothing
    (Just f) <*> x = (Just f x)
```

ok that seems to make sense. `pure` puts it into a `Maybe` context and `<*>`
 with `Nothing` gives nothing, or with a `Just function` applies the function
 to the value in the `Just`. What about `Just (*3) (Nothing)` though?

```
instance Applicative Maybe where
    pure x = Just x
    Nothing <*> _ = Nothing
    (Just f) <*> Nothing = Nothing
    (Just f) <*> (Just x) = (Just f x)
```

That covers that case but it's actually a repeat of the implementation of `fmap`.

```
instance Applicative Maybe where
    pure x = Just x
    Nothing <*> _ = Nothing
    (Just f) <*> x = fmap f x
```

This lets you chain stuff together like this:

```
λ: pure (+) <*> (Just 3) <*> (Just 4)
Just 7
```

Although why you would want to is still a mystery.

Importantly though `pure f <*> x` is equivalent to `fmap f x`. Which is why we
 can use the fmap infix operator `<$>` instead.

```
 λ: (+) <$> (Just 4) <*> (Just 5)
Just 9
```

So that helps decipher the cryptic use of `<*>` and `<$>`. `<$>` is just
 `fmap`, `<*>` applies a function in a context.

This is actually kind of cool:

```
 λ: [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
```

Which expands out to:

```
[(+) 1 3] ++ [(+) 1 4] ++ [(+) 2 3] ++ [(+) 2 4] ++
[(*) 1 3] ++ [(*) 1 4] ++ [(*) 2 3] ++ [(*) 2 4]
```

## Monads

Reading a bit ahead we get this:

```
class Applicative f => Monad f where
    return :: a -> f a
    (>>=)  :: f a -> (a -> f b) -> f b
```

So the `Monad` class defines two functions. `return` is identical to `pure`
 in Applicative. (>>=) is a bit weirder. Given a functor containing a thing
 and a function that wraps a thing (the type signature `(a -> f b)`) is 
 identical to `pure` and `return`) then give a Functor of type `b`.

```
λ: (>>=) (Just 3) (\x -> Just x)
Just 3
```

wowo.

```
instance Monad Maybe where
    return = Just
    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x
```

Which lets you do such as

```
λ: (Just 3) >>= (\x -> Just(x+1))
Just 4
```

So we took a value wrapped in a Functor and a function which takes a concrete
 type and returns a value wrapped in a Functor. Then we applied the function
 to the value and returned it wrapped in the Functor. Why?

Actually this is neater:

```
λ: (Just 3) >>= (\x -> return (x+1))
Just 4
```

