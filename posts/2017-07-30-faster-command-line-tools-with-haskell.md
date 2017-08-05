---
layout: post
title: "Faster command line tools with Haskell"
description: "Initial research and implementation of GroupBy for Haskell's vector library"
category: 
tags: [Haskell vector]
---

**Retraction/Update** : The Haskell program at the end of this article was not faster. Friends don’t let friends benchmark after dark. I seem to have made a benchmarking mistake and measured the total time of the benchmark in the Go program vs one iteration of the Haskell program. I was in a hurry for this entire post and rushed through a few pieces of it due to time constraints. If I’d had more time, I would have written a Criterion benchmark for both the Go and Haskell program. In the future I think I’ll do so to reduce the possibility of mistakes like this.

Inspired by [Faster command line tools with Go](https://aadrake.com/posts/2017-05-29-faster-command-line-tools-with-go.html) which itself was inspired by [Faster Command Line Tools in D](http://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/).

The original problem statement:

>It’s a common programming task: Take a data file with fields separated by a delimiter (comma, tab, etc), and run a mathematical calculation involving several of the fields. Often these programs are one-time use scripts, other times they have longer shelf life. Speed is of course appreciated when the program is used more than a few times on large files.

> The specific exercise we’ll explore starts with files having keys in one field, integer values in another. … Fields are delimited by a TAB, and there may be any number of fields on a line. The file name and field numbers of the key and value are passed as command line arguments.


What I wish to be true: 

**I write high level Haskell code as declaratively and naievely as possible and sufficiently smart compiler optimizations make it as fast as hand tuned Go code.**

Let's see if it works out that way!

We'll use the same file described in [Faster command line tools with Go](https://aadrake.com/posts/2017-05-29-faster-command-line-tools-with-go.html) :

> The data we will use is a file of n-grams from the Google Books dataset. The file is 184 Megabytes, uncompressed, and has a total of 10,512,769 lines.

I'll also track progress in a [Github Repo](https://github.com/codygman/faster-command-line-tools-with-haskell) so others can easily reproduce my tests.

First, lets set a baseline by getting the runtime for the fastest Go program from that post. I'm using Go 1.6 which is missing quite a few performance improvements, so I'll update to Go 1.8 using the `ppa:gophers/archive` ppa.

All done, now the fastest version of the Golang code that operates on bytes directly can be found at the bottom of , but it's short enough to paste inline here (note I made small modifications because it wouldn't build directly from the blog post):

```go
func processLine(b []byte) (int, int) {
	key := 0
	val := 0
	i := 0
	for b[i] != '\t' {
		i++
	}
	for i++; b[i] != '\t'; i++ {
		key = key*10 + int(b[i]) - '0'
	}
	for i++; b[i] != '\t'; i++ {
		val = val*10 + int(b[i]) - '0'
	}
	return key, val
}

func processFile(file *os.File) (int, int) {
	file.Seek(0, 0)
	var sumByKey [2009]int

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Bytes()
		k1, v1 := processLine(line)
		sumByKey[k1] += v1
	}
	var k int
	var v int
	for i, val := range sumByKey {
		if val > v {
			k = i
			v = val
		}
	}
	return k, v

}
```

Benchmark:

```go
package main

import (
	"os"
	"testing"
)

func Benchmark_processFile(b *testing.B) {
	file, err := os.Open("../ngrams.tsv")
	defer file.Close()
	if err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		k, v := processFile(file)
		if k != 2006 || v != 22569013 {
			b.Fatalf(`bad result %v | %v`, k, v)
		}
	}
}
```

And it's runtime:

```
cody@zentop:~/faster-command-line-tools-with-haskell/go$ go test -v -bench=.
Benchmark_processFile-4                5         252309722 ns/op
PASS
ok      _/home/cody/faster-command-line-tools-with-haskell/go   2.274s
cody@zentop:~/faster-command-line-tools-with-haskell/go$ go test -v -bench=.
Benchmark_processFile-4                5         252163852 ns/op
PASS
ok      _/home/cody/faster-command-line-tools-with-haskell/go   2.274s
cody@zentop:~/faster-command-line-tools-with-haskell/go$ go test -v -bench=.
Benchmark_processFile-4                5         252327565 ns/op
PASS
ok      _/home/cody/faster-command-line-tools-with-haskell/go   2.274s
```

Now, I want to create a Haskell version and compare it's performance and if necessary, the process of profiling performance!

Navigating back...

`cd /home/cody/faster-command-line-tools-with-haskell`

Creating a new Haskell project using [LTS Haskell 9.0 (ghc-8.0.2)](https://www.stackage.org/lts-9.0):

`
stack new haskell-version simple --resolver lts-9.0
`

We'll use the [bytestring](https://www.stackage.org/lts-9.0/package/bytestring-0.10.8.1) package by adding it to `haskell-version.cabal`:

```
name:                haskell-version
version:             0.1.0.0
... snip ...
build-depends:       base >= 4.7 && < 5
                     , bytestring >= 0.10.8.11

```

Now instead of porting the most performant version, I want to focus on the high level problem and write the simplest and most declarative solution similar to how [Faster command line tools with Go](https://aadrake.com/posts/2017-05-29-faster-command-line-tools-with-go.html)'s author did. 

My hypothesis (and hope!) is that the high level Haskell version will be competitive with the hand-optimized Go version.

Their first version was:


```go
func processFile(filePath string, keyIndex, valueIndex int) {
	delim := "\t"

	fileHandle, err := os.Open(filePath)
	defer fileHandle.Close()

	maxFieldIndex := int(math.Max(float64(keyIndex), float64(valueIndex)))
	sumByKey := make(map[string]int)

	if err != nil {
		log.Fatal(err)
	}

	fileReader := bufio.NewScanner(fileHandle)

	for fileReader.Scan() {
		fields := strings.Split(strings.TrimSpace(fileReader.Text()), delim)

		if maxFieldIndex < len(fields) {
			value, err := strconv.Atoi(fields[valueIndex])
			if err != nil {
				log.Fatal(err)
			}
			sumByKey[fields[keyIndex]] += value
		}
	}

	maxValue := 0
	maxKey := ""
	for k, v := range sumByKey {
		if v > maxValue {
			maxValue = v
			maxKey = k
		}
	}
	fmt.Println("max_key:", maxKey, "sum:", sumByKey[maxKey])

}
```

We'll also use a strict (I usually use strict unless I have good reason not to) Map in the Haskell version, meaning we'll want to add `containers` to our cabal file:

```
name:                haskell-version
version:             0.1.0.0
... snip ...
build-depends:       base >= 4.7 && < 5
                   , bytestring >= 0.10.8.11
                   , containers >= 0.5.7.1

```


A first Haskell version looks like:

```Haskell
module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as M
import Data.List

toInt :: C8.ByteString -> Int
toInt = read . C8.unpack

processFile = do
  bsLines <- C8.lines <$> C8.readFile "../ngrams.tsv"
  let info = C8.splitWith (=='\t') <$> bsLines :: [[C8.ByteString]]
  let m = foldl'
             (\acc x -> let (_:key:val:_) = (x :: [C8.ByteString]) in M.insertWith (\new old -> old + new) key (toInt val) acc)
             M.empty
             info
  pure $ last $ sortOn snd (M.toList m)

main :: IO ()
main = do
  x <- processFile
  print x
```


Let's run it:


```shell
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ stack build
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ time stack exec -- haskell-version
("2006",22569013)

real    0m10.960s
user    0m10.828s
sys     0m0.140s
```


Alright, we are 5x slower than the hand optimized Go code. Let's see why. NOTE that runtime for profiled builds will be much slower as in any language.


```prof
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ stack clean && stack build --profile && stack exec -- haskell-ver
sion -p                                                                                                                               
haskell-version-0.1.0.0: configure (exe)
Configuring haskell-version-0.1.0.0...
haskell-version-0.1.0.0: build (exe)
Preprocessing executable 'haskell-version' for haskell-version-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version-t
mp/Main.p_o )                                                                                                                         
Linking .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version ...
haskell-version-0.1.0.0: copy/register
Installing executable(s) in
/home/cody/faster-command-line-tools-with-haskell/haskell-version/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin
("2006",22569013)
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ cat haskell-version.prof 
        Sun Jul 30 22:51 2017 Time and Allocation Profiling Report  (Final)

           haskell-version +RTS -p -RTS

        total time  =       20.74 secs   (20742 ticks @ 1000 us, 1 processor)
        total alloc = 49,046,382,928 bytes  (excludes profiling overheads)

COST CENTRE      MODULE    SRC                         %time %alloc

toInt            Main      src/Main.hs:8:1-24           56.5   54.5
processFile.info Main      src/Main.hs:12:7-67          22.0   26.3
processFile.m.\  Main      src/Main.hs:14:25-126        14.7   14.7
processFile      Main      src/Main.hs:(10,1)-(17,39)    6.2    4.2


                                                                                               individual      inherited
COST CENTRE                MODULE                SRC                        no.     entries  %time %alloc   %time %alloc

MAIN                       MAIN                  <built-in>                  63          0    0.0    0.0   100.0  100.0
 CAF                       GHC.IO.Handle.FD      <entire-module>            108          0    0.0    0.0     0.0    0.0
 CAF                       GHC.IO.Handle.Text    <entire-module>            106          0    0.0    0.0     0.0    0.0
 CAF                       GHC.IO.Encoding       <entire-module>            100          0    0.0    0.0     0.0    0.0
 CAF                       GHC.IO.FD             <entire-module>             99          0    0.0    0.0     0.0    0.0
 CAF                       Text.Read.Lex         <entire-module>             94          0    0.0    0.0     0.0    0.0
 CAF                       GHC.Conc.Signal       <entire-module>             92          0    0.0    0.0     0.0    0.0
 CAF                       GHC.IO.Encoding.Iconv <entire-module>             84          0    0.0    0.0     0.0    0.0
 CAF:main1                 Main                  <no location info>         124          0    0.0    0.0     0.0    0.0
  main                     Main                  src/Main.hs:(20,1)-(22,9)  126          1    0.0    0.0     0.0    0.0
 CAF:main2                 Main                  <no location info>         122          0    0.0    0.0     0.0    0.0
  processFile              Main                  src/Main.hs:(10,1)-(17,39) 128          1    0.0    0.0     0.0    0.0
 CAF:main7                 Main                  <no location info>         117          0    0.0    0.0     0.0    0.0
  processFile              Main                  src/Main.hs:(10,1)-(17,39) 130          0    0.0    0.0     0.0    0.0
 CAF:toInt                 Main                  src/Main.hs:8:1-5          121          0    0.0    0.0     0.0    0.0
  toInt                    Main                  src/Main.hs:8:1-24         136          1    0.0    0.0     0.0    0.0
 CAF:toInt3                Main                  <no location info>         120          0    0.0    0.0     0.0    0.0
  toInt                    Main                  src/Main.hs:8:1-24         138          0    0.0    0.0     0.0    0.0
 main                      Main                  src/Main.hs:(20,1)-(22,9)  127          0    0.0    0.0   100.0  100.0
  processFile              Main                  src/Main.hs:(10,1)-(17,39) 129          0    6.2    4.2   100.0  100.0
   processFile.info        Main                  src/Main.hs:12:7-67        131          1   22.0   26.3    22.0   26.3
   processFile.m           Main                  src/Main.hs:(13,7)-(16,17) 132          1    0.4    0.0    71.9   69.5
    processFile.m.\        Main                  src/Main.hs:14:25-126      133   10512769   14.7   14.7    71.4   69.5
     processFile.m.\.(...) Main                  src/Main.hs:14:29-66       134   10512769    0.0    0.0     0.0    0.0
     processFile.m.\.key   Main                  src/Main.hs:14:29-66       135   10512769    0.0    0.0     0.0    0.0
     processFile.m.\.val   Main                  src/Main.hs:14:29-66       139   10512769    0.0    0.0     0.0    0.0
     processFile.m.\.\     Main                  src/Main.hs:14:97-105      140   10512355    0.3    0.3     0.3    0.3
     toInt                 Main                  src/Main.hs:8:1-24         137          0   56.5   54.5    56.5   54.5
```


Alright, it looks like most of the time is spent constructing `info` which is our map of years and sums. However I also see that 56.5% of time is spent in the toInt function. Let's revisit it:


```haskell
toInt :: C8.ByteString -> Int
toInt = read . C8.unpack
```

There is a recent `bytestring-conversions` package by Toralf Wittner which does this using typeclasses and an older library by Don Stewart called `bytestring-lexing`. I haven't used either, so first I'll try out bytestring-lexing since Don has created many simple and performant libraries I've used in the past. 

Our cabal file should now look something like this:

```
name:                haskell-version
version:             0.1.0.0
... snip ...
build-depends:       base >= 4.7 && < 5
                   , bytestring >= 0.10.8.11
                   , bytestring-lexing
                   , containers >= 0.5.7.1

```


Then the code change is as follows:

```
+ import Data.ByteString.Lex.Integral

toInt :: C8.ByteString -> Int
- toInt = read . C8.unpack
+ toInt = readDecimal_
```

Now we run again (be sure to include stack clean just in case, using stack 1.4 I had issues with --profile build being "sticky"):

```
stack clean && stack build && time stack exec -- haskell-version
haskell-version-0.1.0.0: configure (exe)
Configuring haskell-version-0.1.0.0...
haskell-version-0.1.0.0: build (exe)
Preprocessing executable 'haskell-version' for haskell-version-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version-tmp/Main.o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version ...
haskell-version-0.1.0.0: copy/register
Installing executable(s) in
/home/cody/faster-command-line-tools-with-haskell/haskell-version/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin
("2006",22569013)
3.491 secs
```

WOW! We are down to 3.491 seconds with *one* simple(?) change from the naieve version.

Many might be asking: Once you identified toInt as a bottleneck, how did you find bytestring-lexing or bytestring-conversions? Did you know about them in advance?

I actually did not know about them in advance. I searched "[haskell bytestring to int](https://encrypted.google.com/search?q=haskell+bytestring+to+int)" on Google. I was presented with the results below. Note that I've annotated with [] my choice in picking or dismissing each of these search results.


**Annotated Google Results:**

```
Data.ByteString.Conversion - Hackage 
bytestring-conversion-0.2: Type-classes to convert values to and from ByteString. Safe Haskell, None. Language, Haskell2010 ... Parse ByteString s. Methods.
>>>> PERFECT MATCH! I do wonder if there are other libraries though. Toralf wittner has that great and fast cql-io library I've used before. Maybe I can stop here. Ah well, I'll look for more.

Data.ByteString.Conversion.To - Hackage
bytestring-conversion-0.2: Type-classes to convert values to and from ByteString. Safe Haskell, None. Language ... ToByteString Int · ToByteString Int8.
>>>> PASS, this is just a subpage of the library I've made a mental note of above

haskell - What is the best way to convert a ByteString to an Int? - Stack ...
Jan 16, 2012 - Show is used to create a String representation of something, that is useful for debugging and plain-text serialization. The Show typeclass is not ...
>>>> PASS (sadly none of the answers helped me much) Oh good, a stack overflow answer where an expert might have answered

How to convert a Integer to a ByteString in Haskell - Stack Overflow
Feb 17, 2010 - to yield lazy bytestrings, which can of course be converted to strict ones. The cereal package provides much the same interface, but yields strict bytestrings only (so no infinite streaming of encodings). Have a look at the binary package, or any of its non-lazy variants: cereal or binary-strict .
>>>> PASS, but I'm not totally sure why I passed it up. I think because it was mentioning binary a lot, which is related to the problem of turning some bytes into an int rather than a bytestring literal into an int

haskell - How to convert a ByteString to an Int and dealing with ...
Jan 15, 2013 - I need to read a binary format in Haskell. The format is fairly simple: ... The binary package contains tools to get integer types of various sizes and ...
>>>> PASS.. This one lured me in, but ended up being more about binary

haskell - How do I convert 4 bytes of a ByteString to an Int? - Stack ...
Oct 23, 2014 - How can I convert a ByteString to an Int? I have the following 4 bytes ... For binary parsing, have a look at the binary package ... I managed to get ...
>>>> PASS... more binary

Haskell: "Reading" ByteString - Stack Overflow
Mar 23, 2012 - You can use readInt or readInteger from Data.ByteString.Char8 . If you want to read some other type of data, you'll need to write your own ...
>>>> PASS. This one looked promising, but the one below it caught my eye before I clicked through

performance - Efficient number reading in Haskell - Stack Overflow
Dec 20, 2010 - The only time I encountered parsing doubles on the critical path, I used this: ... Update: OK, seems that readDouble is in package bytestring-lexer which is not installed by default. Any other idea? performance parsing haskell ...
>>>> WINNER..
```

When you get to that final search results stack overflow page you'll see this answer:

```
The only time I encountered parsing doubles on the critical path, I used this:

    {-# LANGUAGE ForeignFunctionInterface #-}
    import qualified Data.ByteString.Char8 as B
    import Foreign.C.Types
    import Foreign.C.String
    import System.IO.Unsafe
    
    foreign import ccall unsafe "stdlib.h atof" c_atof :: CString -> IO CDouble
    unsafeReadDouble = unsafePerformIO . flip B.useAsCString c_atof

There wasn't anything that looked like a readDouble in bytestring at that time, though. That would probably be a better solution if it's now standard.

- JB.
```

I was not satisfied with this answer, though I was willing to use it if necessary. Then I scrolled down, glanced at the name, and was happy to see Don Stewart's name since he is a famous Haskeller that highly values simplicity and performance. Author of such blog posts as [Write Haskell as fast as C: exploiting strictness, laziness and recursion](https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/), [Engineering Large Projects in a Functional Language](https://donsbot.wordpress.com/2010/07/11/engineering-large-projects-in-a-functional-language/), and author of an automated gc tuner for GHC announced in the blog post [ghc-gc-tune: Tuning Haskell GC settings for fun and profit](https://donsbot.wordpress.com/2010/07/05/ghc-gc-tune-tuning-haskell-gc-settings-for-fun-and-profit/).

Don's answer was as follows:

```
Another solution: install the bytestring-lexing package, and use readDouble, which I optimized for you.

  cabal install bytestring-lexing

The package provides optimized parsing functions for floating point literals:

  readDouble :: ByteString -> Maybe (Double, ByteString)
```

This is what led me to make the simple fix that made our program roughly 3x faster. Is finding the correct library and making that small change simple? I believe so. Now next time I need to do this task I'll know what library I need.

However, Go had a fast solution in the standard library: atoi

Ideally I think, Haskell would have some fast equivalent of atoi in the standard library that works with Text and ByteString. However that also brings up the discussion about Haskell ideally using Text as a default (which I'll note is becoming more possible as [Backpack](https://ghc.haskell.org/trac/ghc/wiki/Backpack) progresses)


Now, let's look at the profile for the faster version:


```
~/f/haskell-version:master*? λ stack clean && stack build --profile && stack exec -- haskell-version +RTS -p
bytestring-lexing-0.5.0.2: configure
bytestring-lexing-0.5.0.2: build
bytestring-lexing-0.5.0.2: copy/register
haskell-version-0.1.0.0: configure (exe)
Configuring haskell-version-0.1.0.0...
haskell-version-0.1.0.0: build (exe)
Preprocessing executable 'haskell-version' for haskell-version-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version-tmp/Main.p_o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version ...
haskell-version-0.1.0.0: copy/register
Installing executable(s) in
/home/cody/faster-command-line-tools-with-haskell/haskell-version/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin
Completed 2 action(s).
("2006",22569013)
~/f/haskell-version:master*? λ cat haskell-version.prof 
	Sun Jul 30 23:42 2017 Time and Allocation Profiling Report  (Final)

	   haskell-version +RTS -p -RTS

	total time  =        7.92 secs   (7924 ticks @ 1000 us, 1 processor)
	total alloc = 22,528,821,312 bytes  (excludes profiling overheads)

COST CENTRE        MODULE                       SRC                                                   %time %alloc

processFile.info   Main                         src/Main.hs:13:7-67                                    51.5   57.3
processFile.m.\    Main                         src/Main.hs:15:25-126                                  31.9   32.0
processFile        Main                         src/Main.hs:(11,1)-(18,39)                             11.6    9.1
readDecimal_.start Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(260,5)-(265,32)    2.3    0.7
processFile.m.\.\  Main                         src/Main.hs:15:97-105                                   1.1    0.7


                                                                                                                                      individual      inherited
COST CENTRE                      MODULE                       SRC                                                  no.     entries  %time %alloc   %time %alloc

MAIN                             MAIN                         <built-in>                                           156          0    0.0    0.0   100.0  100.0
 CAF                             GHC.IO.Handle.FD             <entire-module>                                      198          0    0.0    0.0     0.0    0.0
 CAF                             GHC.IO.Handle.Text           <entire-module>                                      196          0    0.0    0.0     0.0    0.0
 CAF                             GHC.IO.Encoding              <entire-module>                                      190          0    0.0    0.0     0.0    0.0
 CAF                             GHC.IO.FD                    <entire-module>                                      189          0    0.0    0.0     0.0    0.0
 CAF                             GHC.Conc.Signal              <entire-module>                                      184          0    0.0    0.0     0.0    0.0
 CAF                             GHC.IO.Encoding.Iconv        <entire-module>                                      176          0    0.0    0.0     0.0    0.0
 CAF:main1                       Main                         <no location info>                                   310          0    0.0    0.0     0.0    0.0
  main                           Main                         src/Main.hs:(21,1)-(23,9)                            312          1    0.0    0.0     0.0    0.0
 CAF:main2                       Main                         <no location info>                                   308          0    0.0    0.0     0.0    0.0
  processFile                    Main                         src/Main.hs:(11,1)-(18,39)                           314          1    0.0    0.0     0.0    0.0
 CAF:main7                       Main                         <no location info>                                   306          0    0.0    0.0     0.0    0.0
  processFile                    Main                         src/Main.hs:(11,1)-(18,39)                           316          0    0.0    0.0     0.0    0.0
 CAF:readDecimal__$sreadDecimal_ Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:246:1-12         280          0    0.0    0.0     0.0    0.0
  readDecimal_                   Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(246,1)-(335,59) 323          1    0.0    0.0     0.0    0.0
 CAF:toInt                       Main                         src/Main.hs:9:1-5                                    307          0    0.0    0.0     0.0    0.0
  toInt                          Main                         src/Main.hs:9:1-20                                   322          1    0.0    0.0     0.0    0.0
 main                            Main                         src/Main.hs:(21,1)-(23,9)                            313          0    0.0    0.0   100.0  100.0
  processFile                    Main                         src/Main.hs:(11,1)-(18,39)                           315          0   11.6    9.1   100.0  100.0
   processFile.info              Main                         src/Main.hs:13:7-67                                  317          1   51.5   57.3    51.5   57.3
   processFile.m                 Main                         src/Main.hs:(14,7)-(17,17)                           318          1    0.9    0.0    36.9   33.7
    processFile.m.\              Main                         src/Main.hs:15:25-126                                319   10512769   31.9   32.0    36.0   33.7
     processFile.m.\.(...)       Main                         src/Main.hs:15:29-66                                 320   10512769    0.0    0.0     0.0    0.0
     processFile.m.\.key         Main                         src/Main.hs:15:29-66                                 321   10512769    0.0    0.0     0.0    0.0
     processFile.m.\.val         Main                         src/Main.hs:15:29-66                                 326   10512769    0.0    0.0     0.0    0.0
     processFile.m.\.\           Main                         src/Main.hs:15:97-105                                329   10512355    1.1    0.7     1.1    0.7
     readDecimal_                Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(246,1)-(335,59) 324          0    0.6    0.0     3.1    0.9
      readDecimal_.start         Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(260,5)-(265,32) 325   10512769    2.3    0.7     2.5    0.9
       readDecimal_.loop0        Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(268,5)-(274,32) 327   10512769    0.2    0.0     0.2    0.2
        readDecimal_.loop1       Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(278,5)-(284,52) 328    2272392    0.1    0.1     0.1    0.2
         readDecimal_.loop2      Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(285,5)-(291,53) 330     327077    0.0    0.0     0.0    0.0
          readDecimal_.loop3     Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(292,5)-(298,54) 331      43808    0.0    0.0     0.0    0.0
           readDecimal_.loop4    Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(299,5)-(305,55) 332       6556    0.0    0.0     0.0    0.0
            readDecimal_.loop5   Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(306,5)-(312,56) 333        724    0.0    0.0     0.0    0.0
             readDecimal_.loop6  Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(313,5)-(319,57) 334         71    0.0    0.0     0.0    0.0

```


The majority of the time is spent in building up the map. Is there anyway we can speed that up? From previous knowledge, I know that there is a Data.IntMap.Strict module and I'm almost sure that inserting ints would be faster. We can trade building more ints with the readDecimal_ function from Data.ByteString.Lex.Integral in exchange for using Data.IntMap.Strict. I think it will be a good tradeoff. Lets see.

Here is the full code with the change to Data.IntMap.strict. I had to change on import and use toInt on the key in the first argument of foldl':

```haskell
module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.IntMap.Strict as M
import Data.List
import Data.ByteString.Lex.Integral

toInt :: C8.ByteString -> Int
toInt = readDecimal_

processFile = do
  bsLines <- C8.lines <$> C8.readFile "../ngrams.tsv"
  let info = C8.splitWith (=='\t') <$> bsLines :: [[C8.ByteString]]
  let m = foldl'
             (\acc x -> let (_:key:val:_) = (x :: [C8.ByteString]) in M.insertWith (\new old -> old + new) (toInt key) (toInt val) acc)
             M.empty
             info
  pure $ last $ sortOn snd (M.toList m)

main :: IO ()
main = do
  x <- processFile
  print x
```

Let's see what the results are:

```shell
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ stack clean && stack build && stack exec -- time haskell-version
haskell-version-0.1.0.0: configure (exe)
Configuring haskell-version-0.1.0.0...
haskell-version-0.1.0.0: build (exe)
Preprocessing executable 'haskell-version' for haskell-version-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version-t
mp/Main.o )                                                                                                                           
Linking .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version ...
haskell-version-0.1.0.0: copy/register
Installing executable(s) in
/home/cody/faster-command-line-tools-with-haskell/haskell-version/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin
(2006,22569013)
2.59user 0.06system 0:02.65elapsed 99%CPU (0avgtext+0avgdata 240216maxresident)k
0inputs+0outputs (0major+12897minor)pagefaults 0swaps
```

2.59 seconds! Wow, we are *very* close to the optimized Go program without too much arcane knowledge using Haskell :)

Let's look at the profile for this one!

```
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ stack clean && stack build --profile && stack exec -- haskell-version +RTS -p                                                                                                                          
haskell-version-0.1.0.0: configure (exe)
Configuring haskell-version-0.1.0.0...
haskell-version-0.1.0.0: build (exe)
Preprocessing executable 'haskell-version' for haskell-version-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version-tmp/Main.p_o )                                                                                                                         
Linking .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version ...
haskell-version-0.1.0.0: copy/register
Installing executable(s) in
/home/cody/faster-command-line-tools-with-haskell/haskell-version/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin
(2006,22569013)
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ cat haskell-version.prof 
        Sun Jul 30 23:52 2017 Time and Allocation Profiling Report  (Final)

           haskell-version +RTS -p -RTS

        total time  =        7.59 secs   (7595 ticks @ 1000 us, 1 processor)
        total alloc = 21,369,235,392 bytes  (excludes profiling overheads)

COST CENTRE        MODULE                       SRC                                                   %time %alloc

processFile.info   Main                         src/Main.hs:13:7-67                                    54.9   60.4
processFile.m.\    Main                         src/Main.hs:15:25-134                                  21.6   26.7
processFile        Main                         src/Main.hs:(11,1)-(18,39)                             11.6    9.6
readDecimal_.start Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(260,5)-(265,32)    5.5    1.6
readDecimal_       Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(246,1)-(335,59)    1.2    0.0
processFile.m.\.\  Main                         src/Main.hs:15:97-105                                   1.1    0.8
processFile.m      Main                         src/Main.hs:(14,7)-(17,17)                              1.1    0.0


                                                                                                                                      individual      inherited                                                                                                              
COST CENTRE                      MODULE                       SRC                                                  no.     entries  %time %alloc   %time %alloc                                                                                                              

MAIN                             MAIN                         <built-in>                                           154          0    0.
0    0.0   100.0  100.0CAF                             GHC.IO.Handle.FD             <entire-module>                                      195          0    0.
0    0.0     0.0    0.0                                                                                                               
 CAF                             GHC.IO.Handle.Text           <entire-module>                                      193          0    0.0    0.0     0.0    0.0                                                                                                               
 CAF                             GHC.IO.Encoding              <entire-module>                                      187          0    0.0    0.0     0.0    0.0                                                                                                               
 CAF                             GHC.IO.FD                    <entire-module>                                      186          0    0.0    0.0     0.0    0.0                                                                                                               
 CAF                             GHC.Conc.Signal              <entire-module>                                      182          0    0.0    0.0     0.0    0.0                                                                                                               
 CAF                             GHC.IO.Encoding.Iconv        <entire-module>                                      174          0    0.0    0.0     0.0    0.0                                                                                                               
 CAF:main1                       Main                         <no location info>                                   306          0    0.0    0.0     0.0    0.0                                                                                                               
  main                           Main                         src/Main.hs:(21,1)-(23,9)                            308          1    0.0    0.0     0.0    0.0                                                                                                               
 CAF:main2                       Main                         <no location info>                                   304          0    0.0    0.0     0.0    0.0                                                                                                               
  processFile                    Main                         src/Main.hs:(11,1)-(18,39)                           310          1    0.0    0.0     0.0    0.0                                                                                                               
 CAF:main7                       Main                         <no location info>                                   302          0    0.0    0.0     0.0    0.0                                                                                                               
  processFile                    Main                         src/Main.hs:(11,1)-(18,39)                           312          0    0.0    0.0     0.0    0.0                                                                                                               
 CAF:readDecimal__$sreadDecimal_ Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:246:1-12         277          0    0.0    0.0     0.0    0.0                                                                                                               
  readDecimal_                   Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(246,1)-(335,59) 317          1    0.0    0.0     0.0    0.0                                                                                                               
 CAF:toInt                       Main                         src/Main.hs:9:1-5                                    303          0    0.0    0.0     0.0    0.0                                                                                                               
  toInt                          Main                         src/Main.hs:9:1-20                                   316          1    0.0    0.0     0.0    0.0                                                                                                               
 main                            Main                         src/Main.hs:(21,1)-(23,9)                            309          0    0.0    0.0   100.0  100.0                                                                                                               
  processFile                    Main                         src/Main.hs:(11,1)-(18,39)                           311          0   11.6    9.6   100.0  100.0                                                                                                               
   processFile.info              Main                         src/Main.hs:13:7-67                                  313          1   54.9   60.4    54.9   60.4                                                                                                               
   processFile.m                 Main                         src/Main.hs:(14,7)-(17,17)                           314          1    1.1    0.0    33.5   30.1                                                                                                               
    processFile.m.\              Main                         src/Main.hs:15:25-134                                315   10512769   21.6   26.7    32.4   30.1                                                                                                               
     processFile.m.\.(...)       Main                         src/Main.hs:15:29-66                                 321   10512769    0.0    0.0     0.0    0.0                                                                                                               
     processFile.m.\.key         Main                         src/Main.hs:15:29-66                                 320   10512769    0.3    0.0     0.3    0.0                                                                                                               
     processFile.m.\.val         Main                         src/Main.hs:15:29-66                                 326   10512769    0.3    0.0     0.3    0.0                                                                                                               
     processFile.m.\.\           Main                         src/Main.hs:15:97-105                                327   10512355    1.1    0.8     1.1    0.8                                                                                                               
     readDecimal_                Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(246,1)-(335,59) 318          0    1.2    0.0     9.0    2.5                                                                                                               
      readDecimal_.start         Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(260,5)-(265,32) 319   21025538    5.5    1.6     7.8    2.5                                                                                                               
       readDecimal_.loop0        Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(268,5)-(274,32) 322   21025538    0.9    0.0     2.3    1.0                                                                                                               
        readDecimal_.loop1       Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(278,5)-(284,52) 323   12785161    0.6    0.1     1.4    1.0                                                                                                               
         readDecimal_.loop2      Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(285,5)-(291,53) 324   10839846    0.6    0.0     0.8    0.8                                                                                                               
          readDecimal_.loop3     Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(292,5)-(298,54) 325   10556577    0.2    0.8     0.2    0.8                                                                                                               
           readDecimal_.loop4    Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(299,5)-(305,55) 328       6556    0.0    0.0     0.0    0.0                                                                                                               
            readDecimal_.loop5   Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(306,5)-(312,56) 329        724    0.0    0.0     0.0    0.0                                                                                                               
             readDecimal_.loop6  Data.ByteString.Lex.Integral src/Data/ByteString/Lex/Integral.hs:(313,5)-(319,57) 330         71    0.0    0.0     0.0    0.0                                               
```

It looks like over half of our time is spent in info, which splits all of our lines we've read by '\t' into a new list. I wonder if using BangPattern's to make info strict would help? Let's see....

It actually made it a little slower. If you notice, we actually discard parts of the list later in the fold:

```
let m = foldl'
             (\acc x -> let (_:key:val:_) = (x :: [C8.ByteString]) in M.insertWith (\new old -> old + new) (toInt key) (toInt val) acc)
             M.empty
             info
```

If we make info strict then we load those pieces that we don't use into memory anyway. Thanks laziness! Hm, that makes me wonder... what would happen if we use lazy ByteStrings? Let's try it... why not? I'll keep the IntMap strict since we know every piece of it constructed is needed for the final result.


Let's look over the code again:

```haskell
toInt :: C8.ByteString -> Int
toInt = readDecimal_

processFile = do
  bsLines <- C8.lines <$> C8.readFile "../ngrams.tsv"
  let info = C8.splitWith (=='\t') <$> bsLines :: [[C8.ByteString]]
  let m = foldl'
             (\acc x -> let (_:key:val:_) = (x :: [C8.ByteString]) in M.insertWith (\new old -> old + new) (toInt key) (toInt val) acc)
             M.empty
             info
  pure $ last $ sortOn snd (M.toList m)
```

AHA! C8.splitWith (=='\t') is doing the same functionality that C8.words is commonly used for. I'm also pretty sure since it's widely used that C8.words will be faster than C8.splitWith (=='\t').

Here's the code after the change:

```
processFile = do
  info <- fmap C8.words . C8.lines <$> C8.readFile "../ngrams.tsv"
  let m = foldl'
             (\acc x -> let (_:key:val:_) = (x :: [C8.ByteString]) in M.insertWith (\new old -> old + new) (toInt key) (toInt val) acc)
             M.empty
             info
  pure $ last $ sortOn snd (M.toList m)
```

Now let's run it!

```shell
cody@zentop:~/faster-command-line-tools-with-haskell/haskell-version$ stack clean && stack build && stack exec -- time haskell-version
haskell-version-0.1.0.0: configure (exe)
Configuring haskell-version-0.1.0.0...
haskell-version-0.1.0.0: build (exe)
Preprocessing executable 'haskell-version' for haskell-version-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version-tmp/Main.o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskell-version/haskell-version ...
haskell-version-0.1.0.0: copy/register
Installing executable(s) in
/home/cody/faster-command-line-tools-with-haskell/haskell-version/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin
(2006,22569011)
2.00user 0.07system 0:02.07elapsed 99%CPU (0avgtext+0avgdata 245192maxresident)k
0inputs+0outputs (0major+14078minor)pagefaults 0swaps
```

2 seconds! Awesome. We beat Go :P

Now to the elephant in the room: Why was C8.words faster?

To the implementation!

```
-- | 'words' breaks a ByteString up into a list of words, which
-- were delimited by Chars representing white space.
words :: ByteString -> [ByteString]
words = P.filter (not . B.null) . B.splitWith isSpaceWord8
{-# INLINE words #-}
```

B.splitWith is actually used within words, but it uses [isSpaceWord8](https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/src/Data-ByteString-Internal.html#isSpaceWord8) from Data.ByteString.Internal:

```haskell
-- | Selects words corresponding to white-space characters in the Latin-1 range
-- ordered by frequency.
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w =
    w == 0x20 ||
    w == 0x0A || -- LF, \n
    w == 0x09 || -- HT, \t
    w == 0x0C || -- FF, \f
    w == 0x0D || -- CR, \r
    w == 0x0B || -- VT, \v
    w == 0xA0    -- spotted by QC..
{-# INLINE isSpaceWord8 #-}
```

It does a comparison to the Word8 value directly rather than `\t`, though thinking in terms of platonic ideals I'm wondering if this isn't something a sufficiently smart compiler should aim to optimize. 

There we have it: Faster command line tools with Haskell and a short practical guide to benchmarking and improving performance!
