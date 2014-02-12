---
layout: post
title: Network.URL and fmap fun in haskell
description: ""
category: 
tags: []
---

Well I'm up late once again, and I've found myself toying with [Network.URL](http://hackage.haskell.org/package/url-2.1/docs/Network-URL.html) and [fmap](http://hackage.haskell.org/package/base-4.6.0.1/docs/Prelude.html#v:fmap). The tutorial that taught me what fmap was is [here](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html).

I'm not quite sure how to do it, but all I'd really like to do is give a url, parameter name, and a parameter value and get a valid url out of it or an error. So let's start playing around...

```haskell
-- My first go at building the url is ugly but it works
fmap exportURL $ fmap (\x -> add_param x ("hoogle", "fmap")) $ importURL "http://www.haskell.org/hoogle/"

-- Next I tried cleaning things up with '<$>' which is the infix version of fmap
(\x -> add_param x ("hoogle", "fmap")) <$> importURL "http://www.haskell.org/hoogle/"

-- After that I wanted to eliminate the lambda and tried this:
add_param . flip ("hoogle", "fmap") <$> importURL "http://www.haskell.org/hoogle/"
--     Couldn't match expected type `a0 -> URL -> c0'
--                 with actual type `([Char], [Char])'
--     In the first argument of `flip', namely `("hoogle", "fmap")'
--     In the second argument of `(.)', namely `flip ("hoogle", "fmap")'
--     In the first argument of `(<$>)', namely
--       `add_param . flip ("hoogle", "fmap")'

-- I'm clearly misunderstanding flip, so lets try to distill this down
-- I can get a normal url just using fromJust (Maybe a -> a) (don't do
-- this in important code, handle the Nothing!)
fromJust $ importURL "http://www.haskell.org/hoogle/"

-- simply adding query parameters
add_param (fromJust $ importURL "http://www.haskell.org/hoogle/") ("hoogle", "fmap") 

```

Next I wanted to eliminate this lambda:

```haskell
add_param ("query" "haskell") (fromJust $ importURL "http://www.haskell.org/hoogle/")
```

We can use flip for that:

```haskell
-- :t flip add_param
-- flip add_param :: (String, String) -> URL -> URL
flip add_param ("hoogle", "fmap") (fromJust $ importURL "http://www.haskell.org/hoogle/")
-- URL {url_type = Absolute (Host {protocol = HTTP False, host = "www.google.com", port = Nothing}), url_path = "", url_params = [("query","haskell")]}
```

Awesome, now we can eliminate the lambda:

```haskell
flip add_param ("hoogle", "fmap") <$> importURL "http://www.haskell.org/hoogle/"
-- Just (URL {url_type = Absolute (Host {protocol = HTTP False, host = "www.google.com", port = Nothing}), url_path = "", url_params = [("query","haskell")]})
```

Then we end up with this difference in code (remember <$> is the infix version of the fmap function):

```haskell
-- first try
fmap exportURL $ fmap (\x -> add_param x ("hoogle", "fmap")) $ importURL "http://www.haskell.org/hoogle/"
-- Just "http://www.haskell.org/hoogle/?hoogle=fmap"

-- new code
exportURL <$> flip add_param ("hoogle", "fmap") <$> importURL "http://www.haskell.org/hoogle/"
-- Just "http://www.haskell.org/hoogle/?hoogle=fmap"
```

So what was the point of all this? Learning how to use flip to reverse function parameters enabling us to make our code more concise and knowing how to add query parameters to a url :D You can probably infer that fmap (aka <$>) is "magic", meaning it knows about context.

Truthfully, I was going to make a longer post but then I ended up having a slight detour thinking about the semantics of this.
