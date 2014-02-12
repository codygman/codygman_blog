---
layout: post
title: Read an integer list in Haskell
description: ""
category: 
tags: []
---

Was browsing my feed and came across something similar to this in stack overflow. So really quickly wrote out the imperative version:

```haskell
readInts :: String -> [Int]
readInts x = read x

main = do
  input <- getLine
  let list = read input :: [Int]
  print list
```

Then I of course rewrote it to the condensed version:

```haskell
readInts :: String -> [Int]
readInts x = read x

main = do
  getLine >>= print . readInts
```

I attempted to get rid of the readInts function like this:

```haskell
main = getLine >>=  print . (read :: [Int])
```

However I was met with the type error and... I apparently never finished this post. Maybe later?

