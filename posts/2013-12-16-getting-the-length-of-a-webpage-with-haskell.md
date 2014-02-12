---
layout: post
title: Getting the length of a webpage with haskell
description: ""
category: 
tags: []
---

I was wanting to see how many words were on a webpage today, so I opened up a python interpreter and typed in the following code:

```python
import urllib
webpage_content = urllib.urlopen("http://www.reddit.com").read()
words = webpage_content.split()
print len(words)
```

Very succinct code that does what I need it to. I've been using haskell lately though and wondered what it's solution would look like. First I wrote a very imperative version:

```haskell
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BCL

main = do
  putStrLn "Enter a url: "
  url <- getLine
  pageText <- simpleHttp url
  let pageTextCount = length . words . BCL.unpack $ pageText
  print $ "There are " ++ show pageTextCount ++ " words!"
```

<div style="text-align:center" markdown="1">
[Play with the code in an online IDE](https://www.fpcomplete.com/user/codygman/getting-number-of-words-on-a-web-page-imperatively) (free signup required)
</div>

This isn't nearly as short as the python version, but its still pretty understandable to most. Having to unpack the lazy bytestring might throw a few off, but everything else is straightforward. After writing this, I realized I could use bind and functional composition to shorten things up even more than python!

```haskell
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as BCL

main = do
    putStrLn "Enter a url: "
    getLine >>= simpleHttp >>= print . length . BCL.words
```

<div style="text-align:center" markdown="1">
[Play with the code in an online IDE](https://www.fpcomplete.com/user/codygman/getting-number-of-words-on-a-web-page-with-bytestrings) (free signup required)
</div>

After that I wondered if I could use the (official?) Network.HTTP package easier, and made a very similar version with it.

```haskell
import Network.HTTP

main = do
    putStrLn "Enter a url: "
    getLine >>= simpleHTTP . getRequest >>= getResponseBody >>= print . length . words
```

<div style="text-align:center" markdown="1">
[Play with the code in an online IDE](https://www.fpcomplete.com/user/codygman/getting-length-of-website-in-haskell) (free signup required)
</div>

Hope this helps :)
