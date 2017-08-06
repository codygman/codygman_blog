---
layout: post
title: "Low-level Haskell vs Low-level Go"
description: ""
category: 
tags: [Haskell]
---

In [Faster Command Line Tools With Haskell](https://codygman.github.io/posts/2017-07-30-faster-command-line-tools-with-haskell.html) we went through the process of comparing performance in an optimized Go program to a high level Haskell program with no optimization, then successively optimizing the Haskell code while maintaining high level logic.

At the end of the post I declared victory over the Go program, but the next day I woke up to someone pointing out I'd made an embarassing benchmarking mistake. Given this context, the point of this post is to compare a low level Haskell version to the low level Go version and see how they compare. Later I hope to revisit the high level Haskell version and see if I can look more into the optimizations necessary to achieve better performance.

The updated code looks like this:

```haskell
process bs = (i, xs V.! i) where
  info = C8.splitWith (=='\t') <$> C8.lines bs
  xs = V.unsafeAccumulate (+) (V.replicate 2009 0) . V.fromList 
     $ map (\(_:key:val:_) -> (toInt key, toInt val)) info
  i = V.maxIndex xs

main = print . process =<< C8.readFile "../ngrams.tsv"
```

After running it we get:

```shell
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
(2006,22569013)

real    0m1.646s
user    0m1.564s
sys     0m0.080s
```

For the Go code we get:

```
cody@zentop:~/faster-command-line-tools-with-haskell/go$ time go run goversion.go 

real    0m0.340s
user    0m0.300s
sys     0m0.040s
```

What gives? This is still pretty slow. Let's try avoiding V.fromList and instead build up the vector directly. I [adapted code recommending this](https://www.reddit.com/r/programming/comments/6qmhuy/faster_command_line_tools_with_haskell/dkyty0n/) in a reddit comment which can be seen below:

```haskell
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Integral
import Control.Monad.Primitive
import Control.Monad (forM_)

processFile :: FilePath -> IO (VM.MVector RealWorld Int)
processFile path = do
  content <- BS.readFile path
  vec <- VGM.new 2009
  forM_ (BS.split '\t' <$> BS.lines content) $ \line -> do
    let (_ : k : v : _) = readDecimal_ <$> line
    VGM.modify vec (+v) k
  return vec

main :: IO ()
main = do
  vec <- processFile "../ngrams.tsv"
  vec' <- VU.freeze vec
  let answer = VU.maxIndex vec'
  print answer
```

So how does it perform...

```
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version 
2006

real    0m0.960s
user    0m0.912s
sys     0m0.044s
```

I rebooted, so let's get another baseline of Go's performance:

```
cody@zentop:~/faster-command-line-tools-with-haskell/go$ time ./goversion 

real    0m0.252s
user    0m0.208s
sys     0m0.044s
cody@zentop:~/faster-command-line-tools-with-haskell/go$ time ./goversion 

real    0m0.252s
user    0m0.228s
sys     0m0.028s
cody@zentop:~/faster-command-line-tools-with-haskell/go$ time ./goversion 

real    0m0.254s
user    0m0.220s
sys     0m0.032s
```

The Haskell code is a about 4 times slower than the equivalent Go code. What else can we do? Let's take a look at the code:

```haskell
processFile :: FilePath -> IO (VM.MVector RealWorld Int)
processFile path = do
  content <- BS.readFile path
  vec <- VGM.new 2009
  forM_ (BS.split '\t' <$> BS.lines content) $ \line -> do
    let (_ : k : v : _) = readDecimal_ <$> line
    VGM.modify vec (+v) k
  return vec
```

See `readDecimal_ <$> line`? We are applying it more than we need to. We should assign k and v to bytestrings and then read them to integers using `readDecimal` after. This should save us a little. After the changes it looks like:

```
processFile :: FilePath -> IO (VM.MVector RealWorld Int)
processFile path = do
  content <- BS.readFile path
  vec <- VGM.new 2009
  forM_ (BS.split '\t' <$> BS.lines content) $ \line -> do
    let (_ : k : v : _) = line
        keyInt = readDecimal_ k
        valInt = readDecimal_ v
    VGM.modify vec (+ valInt) keyInt
  return vec
```

And the we save 200ms!

```
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version 
2006

real	0m0.735s
user	0m0.680s
sys	0m0.052s

```

I thought inlining might help, but I believe it's already being inlined since there was no change in performance. Looking over the code again I did notice `(+ valInt)` which led me to think about the cost of partial application. Let's write out a top level definition and see if that changes performance. It didn't however.

There is an unsafe version of freeze aptly named `unsafeFreeze`. The difference being:

```
O(1) Unsafe convert a mutable vector to an immutable one without copying. The mutable vector may not be used after this operation.
```

Whoa! It's O(1) compared to freeze being O(n). I have high hopes... however it only took off a couple of milliseconds. What if we change modify to unsafeModify which is called within the main loop of the program? This seemd to save us 10ms. Is the ST monad faster than the IO monad? Let's see... that looks like:

```
{-# Language BangPatterns #-}
module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Integral
import Control.Monad.Primitive
import Control.Monad (forM_)
import Control.Monad.ST

processFile :: BS.ByteString -> ST RealWorld (VM.MVector RealWorld Int)
processFile content = do
  vec <- VGM.new 2009
  forM_ (BS.split '\t' <$> BS.lines content) $ \line -> do
    let (_ : k : v : _) = line
        keyInt = readDecimal_ k
        valInt = readDecimal_ v
    VGM.unsafeModify vec (+ valInt) keyInt
  return vec
{-# INLINE processFile #-}

main :: IO ()
main = do
  content <- BS.readFile "../ngrams.tsv"
  vec <- stToIO $ processFile content
  vec' <- VU.unsafeFreeze vec
  let answer = VU.maxIndex vec'
  print answer
```

Looks to be about the same using the stToIO version (I couldn't figure out how to write it using runST):

```
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version 
2006

real	0m0.737s
user	0m0.696s
sys	0m0.040s
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version 
2006

real	0m0.739s
user	0m0.688s
sys	0m0.048s
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version 
2006

real	0m0.734s
user	0m0.692s
sys	0m0.040s
```

What if we use `BS.words` in place of `BS.split '\t'`? Last time that gave us a nice speed up. That looks like:

```haskell
processFile :: BS.ByteString -> IO (VM.MVector RealWorld Int)
processFile content = do
  vec <- VGM.new 2009
  forM_ (BS.words <$> BS.lines content) $ \line -> do
    let (_ : k : v : _) = line
        keyInt = readDecimal_ k
        valInt = readDecimal_ v
    VGM.unsafeModify vec (+ valInt) keyInt
  return vec
{-# INLINE processFile #-}

```

But the results were slower...

```shell
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.782s
user	0m0.732s
sys	0m0.048s
```

Slower... Why? Because words dispatches on multiple comparisons perhaps? What if we take the definition of words and specialize it to tab only? That looks like:

```haskell
import Data.Word (Word8)
import qualified Data.ByteString as B

isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 !w = w == 0x09 -- \t
{-# INLINE isSpaceWord8 #-}

mywords :: BS.ByteString -> [BS.ByteString]
mywords bs = B.splitWith isSpaceWord8 bs
{-# INLINE mywords #-}

processFile :: BS.ByteString -> IO (VM.MVector RealWorld Int)
processFile content = do
  vec <- VGM.new 2009
  forM_ (mywords <$> BS.lines content) $ \line -> do
    let (_ : k : v : _) = line
        keyInt = readDecimal_ k
        valInt = readDecimal_ v
    VGM.unsafeModify vec (+ valInt) keyInt
  return vec
{-# INLINE processFile #-}
```

Performance is... Better by a meaningful margin again! It's been a while ;)

```shell
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.613s
user	0m0.564s
sys	0m0.048s
```

Notice that in isSpaceWord8 which I yanked from ByteString.internal I made sure the incoming word8 was strict with a BangPattern. For curiousities sake, let's see how taking that away affects performance:

```
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.584s
user	0m0.532s
sys	0m0.048s
```

To my surprise... it's faster! This is why you should always profile profile profile. TODO In the future figure out why it was faster to not mark w as strict.

To recap things:

- We started this article with a lower level version using vectors, but it used toList and took 0m1.646s
- We modified it to build the vector directly and it took 0m0.960s
- We apply readDecimal_ only to necessary text and it took 0m0.735s
- We putter around with things that seemed promising but didn't make much of a difference
- We replace BS.split '\t' with a specialized Word8 version inspired by BS.words and it took 0m0.613s
- We modify that specialized function and remove our premature strictness optimization and it took 0m0.584s

Running the go version again, it takes 0m0.256s. So we are down to 2x as slow from where we started at 8x as slow.

I have a theory that as you optimize Haskell code, the other optimizations start to get a better idea of what to do with the code to optimize it further. One way I want to test this theory is to try and use the llvm backend again and see if it results in a performance increase.

First we'll get another baseline:

```
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.585s
user	0m0.540s
sys	0m0.044s
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.587s
user	0m0.536s
sys	0m0.048s
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.589s
user	0m0.548s
sys	0m0.040s
```

After adding `-fllvm`:

```
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.556s
user	0m0.504s
sys	0m0.048s
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.551s
user	0m0.504s
sys	0m0.044s
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time haskell-version
2006

real	0m0.552s
user	0m0.508s
sys	0m0.040s
```

I'm unsure how to proceed from here without looking at the core generated, so I'll leave off here for now. After learning more about ghc core in the future perhaps I can revisit this post.
