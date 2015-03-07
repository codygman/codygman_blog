---
layout: post
title: A simple and quick text parsing tutorial using Haskell
description: "A simple and quick text parsing tutorial using Haskell"
category: 
tags: [Haskell]
---

I was just browsing [Hacker News](http://www.news.ycombinator.com) yesterday and came across a simple whitespace delimited ascii chart:

```
              perf3                   perf3 / 
              macros      chars       chars * 100
  -------------------------------------------------
  scala       15963       24885       64,15
  nim         11121       20263       54,88
  java        17969       53223       33,76
  ocaml       7063        24371       28,98
  coffee      2326        15653       14,86
  racket      2461        17229       14,28
  cs          5414        45039       12,02
  clojure     1174        10099       11,62
  ruby        1255        13247       9,47
  go          3048        36321       8,39
  vb          4523        58099       7,78
  rust        4084        56516       7,23
  js          1726        26437       6,53
  c           3649        73047       5,00
  haskell     1163        30115       3,86
  python      304         19632       1,55
  forth       563         44715       1,26
  php         331         27332       1,21
```

You may notice that this is sorted by the last column "perf3/chars * 100". I simply wanted to play with the table and sort on the "perf3 macros" and "chars" columns. I started to open up Calc and paste in the table above to a csv document to do this, but thought it would be a great time to sharpen my Haskell skills for a small real world task.

So how do we make sense of the above chart? I'm going to start by saving the chart above into a file named "langstats.txt". Next we can read the file into a string using the `readFile` function and then "pipe" the results with =<< to the print function.

```haskell
λ> print =<< readFile "langstats.txt"
"              perf3                   perf3 / \n              macros      chars       chars * 100\n  -------------------------------------------------\n  scala       15963       24885       64,15\n  nim         11121       20263       54,88\n  java        17969       53223       33,76\n  ocaml       7063        24371       28,98\n  coffee      2326        15653       14,86\n  racket      2461        17229       14,28\n  cs          5414        45039       12,02\n  clojure     1174        10099       11,62\n  ruby        1255        13247       9,47\n  go          3048        36321       8,39\n  vb          4523        58099       7,78\n  rust        4084        56516       7,23\n  js          1726        26437       6,53\n  c           3649        73047       5,00\n  haskell     1163        30115       3,86\n  python      304         19632       1,55\n  forth       563         44715       1,26\n  php         331         27332       1,21\n"
```

Oh, they are all sorted by newlines first of all. What is the function to turn newline separated text into a list in Haskell? It's the aptlay named lines function from Data.List!

```haskell
λ> lines <$> readFile "langstats.txt"
["              perf3                   perf3 / ","              macros      chars       chars * 100","  -------------------------------------------------","  scala       15963       24885       64,15","  nim         11121       20263       54,88","  java        17969       53223       33,76","  ocaml       7063        24371       28,98","  coffee      2326        15653       14,86","  racket      2461        17229       14,28","  cs          5414        45039       12,02","  clojure     1174        10099       11,62","  ruby        1255        13247       9,47","  go          3048        36321       8,39","  vb          4523        58099       7,78","  rust        4084        56516       7,23","  js          1726        26437       6,53","  c           3649        73047       5,00","  haskell     1163        30115       3,86","  python      304         19632       1,55","  forth       563         44715       1,26","  php         331         27332       1,21"]
```

We don't need those first 3 items, so we can use the `drop` function (no need to import it since it's from the base library).

```haskell
λ> drop 3 . lines  <$> readFile "langstats.txt"
["  scala       15963       24885       64,15","  nim         11121       20263       54,88","  java        17969       53223       33,76","  ocaml       7063        24371       28,98","  coffee      2326        15653       14,86","  racket      2461        17229       14,28","  cs          5414        45039       12,02","  clojure     1174        10099       11,62","  ruby        1255        13247       9,47","  go          3048        36321       8,39","  vb          4523        58099       7,78","  rust        4084        56516       7,23","  js          1726        26437       6,53","  c           3649        73047       5,00","  haskell     1163        30115       3,86","  python      304         19632       1,55","  forth       563         44715       1,26","  php         331         27332       1,21"]
```

Ah progress, now we have a list of each line of the table! I can see now that each object is separated by whitespace. My intuition tells me that turning a bunch of whitespace delimited text into a list is a common task in every language. Haskell turns out to be no exception with the words function which is also from Data.List.

So taking the first line of our list of lines looks like this (picture the `.` between head and lines as being a pipe of sorts that applies the lines function and then the head function):

```haskell
λ> head . drop 3 . lines <$> readFile "langstats.txt"
"  scala       15963       24885       64,15"
```

Now we can apply the words function to it and see what we get:

```haskell
λ> words . head . drop 3 . lines <$> readFile "langstats.txt"
["scala","15963","24885","64,15"]
```

Now we know how to transform the first item of the list into a list of columns, but we want to transform every row (line) into columns (words).

In Haskell it's usually useful to find a function that transforms a singular item of a list then `map` that function to the entire list. I won't try to explain what `map` does in the hopes that the example makes it obvious:

```haskell
λ> map words . drop 3 . lines <$> readFile "langstats.txt"
[["scala","15963","24885","64,15"],["nim","11121","20263","54,88"],["java","17969","53223","33,76"],["ocaml","7063","24371","28,98"],["coffee","2326","15653","14,86"],["racket","2461","17229","14,28"],["cs","5414","45039","12,02"],["clojure","1174","10099","11,62"],["ruby","1255","13247","9,47"],["go","3048","36321","8,39"],["vb","4523","58099","7,78"],["rust","4084","56516","7,23"],["js","1726","26437","6,53"],["c","3649","73047","5,00"],["haskell","1163","30115","3,86"],["python","304","19632","1,55"],["forth","563","44715","1,26"],["php","331","27332","1,21"]]
```

Now we have 2d list representing our the individual rows and their columns. How do we go about sorting them? Well first we need to turn the strings that are numbers into strings. In Haskell the default lists must all have the same type though, you can't just mix ints and strings.

The answer is to use an abstract data type to represent each of our `Entries`. Let's define one:

```haskell
λ> data Entry = Entry { langName :: String, perf3 :: Int, totalChars :: Int} deriving Show
```

How do we make one? Well using the first element `["scala","15963","24885","64,15"]` and translating those string numbers into numbers:

```haskell
λ> Entry "scala" 15963 24885
Entry {langName = "scala", perf3 = 15963, totalChars = 24885}
```

Alright now let's make a function to parse that same first element into an Entry automatically. We'll make our `parseEntry` function return either `Just Entry` if 3 list items are present or `Nothing` otherwise.

We need to use the multiline definition format (start `:{` and end `:}`) or pattern matches won't work right in ghci.

```haskell
λ> -- 
λ> :{
*Main Main Data.List| let parseEntry (a:b:c:_) = Just $ Entry a (read b :: Int) (read c :: Int)
*Main Main Data.List|     parseEntry _ = Nothing
*Main Main Data.List| :}
```

Let's try it out:

```haskell
λ> parseEntry ["scala","15963","24885","64,15"]
Just (Entry {langName = "scala", perf3 = 15963, totalChars = 24885})
λ> parseEntry []
Nothing
λ> parseEntry ["",""]
Nothing
```

Now we can use `map` just like before to apply it to the entire 2d list:

```haskell
λ> map parseEntry . map words . drop 3 . lines <$> readFile "langstats.txt"
[Just (Entry {langName = "scala", perf3 = 15963, totalChars = 24885}),Just (Entry {langName = "nim", perf3 = 11121, totalChars = 20263}),Just (Entry {langName = "java", perf3 = 17969, totalChars = 53223}),Just (Entry {langName = "ocaml", perf3 = 7063, totalChars = 24371}),Just (Entry {langName = "coffee", perf3 = 2326, totalChars = 15653}),Just (Entry {langName = "racket", perf3 = 2461, totalChars = 17229}),Just (Entry {langName = "cs", perf3 = 5414, totalChars = 45039}),Just (Entry {langName = "clojure", perf3 = 1174, totalChars = 10099}),Just (Entry {langName = "ruby", perf3 = 1255, totalChars = 13247}),Just (Entry {langName = "go", perf3 = 3048, totalChars = 36321}),Just (Entry {langName = "vb", perf3 = 4523, totalChars = 58099}),Just (Entry {langName = "rust", perf3 = 4084, totalChars = 56516}),Just (Entry {langName = "js", perf3 = 1726, totalChars = 26437}),Just (Entry {langName = "c", perf3 = 3649, totalChars = 73047}),Just (Entry {langName = "haskell", perf3 = 1163, totalChars = 30115}),Just (Entry {langName = "python", perf3 = 304, totalChars = 19632}),Just (Entry {langName = "forth", perf3 = 563, totalChars = 44715}),Just (Entry {langName = "php", perf3 = 331, totalChars = 27332})]
```

Also, instead of prepending `map parseEntry` to ` map words` I could have composed `parseEntry` with `words` like this:

```haskell
λ> map (parseEntry . words) . drop 3 . lines <$> readFile "langstats.txt"
[Just (Entry {langName = "scala", perf3 = 15963, totalChars = 24885}),Just (Entry {langName = "nim", perf3 = 11121, totalChars = 20263}),Just (Entry {langName = "java", perf3 = 17969, totalChars = 53223}),Just (Entry {langName = "ocaml", perf3 = 7063, totalChars = 24371}),Just (Entry {langName = "coffee", perf3 = 2326, totalChars = 15653}),Just (Entry {langName = "racket", perf3 = 2461, totalChars = 17229}),Just (Entry {langName = "cs", perf3 = 5414, totalChars = 45039}),Just (Entry {langName = "clojure", perf3 = 1174, totalChars = 10099}),Just (Entry {langName = "ruby", perf3 = 1255, totalChars = 13247}),Just (Entry {langName = "go", perf3 = 3048, totalChars = 36321}),Just (Entry {langName = "vb", perf3 = 4523, totalChars = 58099}),Just (Entry {langName = "rust", perf3 = 4084, totalChars = 56516}),Just (Entry {langName = "js", perf3 = 1726, totalChars = 26437}),Just (Entry {langName = "c", perf3 = 3649, totalChars = 73047}),Just (Entry {langName = "haskell", perf3 = 1163, totalChars = 30115}),Just (Entry {langName = "python", perf3 = 304, totalChars = 19632}),Just (Entry {langName = "forth", perf3 = 563, totalChars = 44715}),Just (Entry {langName = "php", perf3 = 331, totalChars = 27332})]
```

Now we have a type of `[Maybe Entry]` (well `IO [Maybe Entry]`, but that doesn't make much difference for us). To be honest I'm not sure how to sort `[Maybe a]`'s offhand, so we'll just turn our `[Maybe Entry]` into `[Entry]` safely by composing two functions, `map fromJust` and  `filter isJust`.

`filter isJust` will make sure that of our `[Maybe Entry]` we *only* have `[Just Entry]`, so then it'll be safe to map the fromJust function that turns `Maybe a -> a`. The example will probably explain better:

```haskell
λ> map fromJust . filter isJust . map (parseEntry . words) . drop 3 . lines <$> readFile "langstats.txt"
[Entry {langName = "scala", perf3 = 15963, totalChars = 24885},Entry {langName = "nim", perf3 = 11121, totalChars = 20263},Entry {langName = "java", perf3 = 17969, totalChars = 53223},Entry {langName = "ocaml", perf3 = 7063, totalChars = 24371},Entry {langName = "coffee", perf3 = 2326, totalChars = 15653},Entry {langName = "racket", perf3 = 2461, totalChars = 17229},Entry {langName = "cs", perf3 = 5414, totalChars = 45039},Entry {langName = "clojure", perf3 = 1174, totalChars = 10099},Entry {langName = "ruby", perf3 = 1255, totalChars = 13247},Entry {langName = "go", perf3 = 3048, totalChars = 36321},Entry {langName = "vb", perf3 = 4523, totalChars = 58099},Entry {langName = "rust", perf3 = 4084, totalChars = 56516},Entry {langName = "js", perf3 = 1726, totalChars = 26437},Entry {langName = "c", perf3 = 3649, totalChars = 73047},Entry {langName = "haskell", perf3 = 1163, totalChars = 30115},Entry {langName = "python", perf3 = 304, totalChars = 19632},Entry {langName = "forth", perf3 = 563, totalChars = 44715},Entry {langName = "php", perf3 = 331, totalChars = 27332}]
```

Now we can use the `sortBy` and `on` functions to sort based on different Entry fields:

```haskell
λ> sortBy (compare `on` langName) . map fromJust . filter isJust . map (parseEntry . words) . drop 3 . lines <$> readFile "langstats.txt"
[Entry {langName = "c", perf3 = 3649, totalChars = 73047},Entry {langName = "clojure", perf3 = 1174, totalChars = 10099},Entry {langName = "coffee", perf3 = 2326, totalChars = 15653},Entry {langName = "cs", perf3 = 5414, totalChars = 45039},Entry {langName = "forth", perf3 = 563, totalChars = 44715},Entry {langName = "go", perf3 = 3048, totalChars = 36321},Entry {langName = "haskell", perf3 = 1163, totalChars = 30115},Entry {langName = "java", perf3 = 17969, totalChars = 53223},Entry {langName = "js", perf3 = 1726, totalChars = 26437},Entry {langName = "nim", perf3 = 11121, totalChars = 20263},Entry {langName = "ocaml", perf3 = 7063, totalChars = 24371},Entry {langName = "php", perf3 = 331, totalChars = 27332},Entry {langName = "python", perf3 = 304, totalChars = 19632},Entry {langName = "racket", perf3 = 2461, totalChars = 17229},Entry {langName = "ruby", perf3 = 1255, totalChars = 13247},Entry {langName = "rust", perf3 = 4084, totalChars = 56516},Entry {langName = "scala", perf3 = 15963, totalChars = 24885},Entry {langName = "vb", perf3 = 4523, totalChars = 58099}]
```

Hm, that's getting difficult to see... let's pretty print it. But first let's store our results (before sorting) above in a variable called `results`. I avoided using variables for some time and this function is still of type `IO`, so we'll need to use `<-` instead of `=` for assignment to "draw out" the value.

```haskell
λ> results <- map fromJust . filter isJust . map (parseEntry . words) . drop 3 . lines <$> readFile "langstats.txt"
```

Here's a free pretty printing function (Keep in mind we have to use `mapM_` on functions that do IO):

```haskell
let prettyPrint (Entry a b c) = putStrLn $ a ++ " " ++ show b ++ " " ++ show c
```

Now let's sort alphabetically again...

```haskell
λ> mapM_ prettyPrint . sortBy (compare `on` langName) $ results
c 3649 73047
clojure 1174 10099
coffee 2326 15653
cs 5414 45039
forth 563 44715
go 3048 36321
haskell 1163 30115
java 17969 53223
js 1726 26437
nim 11121 20263
ocaml 7063 24371
php 331 27332
python 304 19632
racket 2461 17229
ruby 1255 13247
rust 4084 56516
scala 15963 24885
vb 4523 58099
```

Now we can try sorting by `perf3`, the second column:

```haskell
λ> mapM_ prettyPrint . sortBy (compare `on` perf3) $ results
python 304 19632
php 331 27332
forth 563 44715
haskell 1163 30115
clojure 1174 10099
ruby 1255 13247
js 1726 26437
coffee 2326 15653
racket 2461 17229
go 3048 36321
c 3649 73047
rust 4084 56516
vb 4523 58099
cs 5414 45039
ocaml 7063 24371
nim 11121 20263
scala 15963 24885
java 17969 53223
```

Wait... it's in the wrong order... that's okay we can `reverse` it!

```haskell
λ> mapM_ prettyPrint . reverse . sortBy (compare `on` perf3) $ results
java 17969 53223
scala 15963 24885
nim 11121 20263
ocaml 7063 24371
cs 5414 45039
vb 4523 58099
rust 4084 56516
c 3649 73047
go 3048 36321
racket 2461 17229
coffee 2326 15653
js 1726 26437
ruby 1255 13247
clojure 1174 10099
haskell 1163 30115
forth 563 44715
php 331 27332
python 304 19632
```

Success! We turned a whitespace delimited table into a 2d list, then transformed that 2d list into a list of our abstract data type (Entries), and finally we sorted it and pretty printed the results.
